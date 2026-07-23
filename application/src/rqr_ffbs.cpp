#include <RcppArmadillo.h>
#include <cmath>
#include <string>
#include <vector>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

namespace {

struct SpdResult {
  arma::mat matrix;
  arma::mat chol_lower;
  double jitter;
  double relative_jitter;
  double min_eigenvalue;
  double matrix_scale;
  double jitter_scale;
  bool absolute_jitter_fallback;
};

struct DrawResult {
  arma::vec draw;
  double jitter;
  double relative_jitter;
  bool used_psd;
  int clamped_eigenvalues;
  double min_eigenvalue;
  double matrix_scale;
  double jitter_scale;
  bool absolute_jitter_fallback;
};

arma::mat symm(const arma::mat& x) {
  return 0.5 * (x + x.t());
}

SpdResult spd_factor(const arma::mat& input, const arma::vec& ladder) {
  arma::mat base = symm(input);
  const double matrix_scale = arma::abs(base).max();
  const bool absolute_fallback = matrix_scale == 0.0;
  const double jitter_scale = absolute_fallback ? 1.0 : matrix_scale;
  arma::mat L;
  if (arma::chol(L, base, "lower")) {
    return SpdResult{
      base, L, 0.0, 0.0, NA_REAL, matrix_scale, jitter_scale, false
    };
  }
  arma::vec eigenvalues;
  if (!arma::eig_sym(eigenvalues, base)) {
    Rcpp::stop("Symmetric eigendecomposition failed after Cholesky failure.");
  }
  const double min_eigenvalue = eigenvalues.min();
  for (arma::uword j = 0; j < ladder.n_elem; ++j) {
    const double relative = ladder(j);
    if (!std::isfinite(relative) || relative <= 0.0) continue;
    arma::mat candidate = base;
    candidate.diag() += relative * jitter_scale;
    if (arma::chol(L, candidate, "lower")) {
      return SpdResult{
        candidate, L, relative * jitter_scale,
        absolute_fallback ? NA_REAL : relative,
        min_eigenvalue, matrix_scale, jitter_scale, absolute_fallback
      };
    }
  }
  Rcpp::stop("Positive-definite Cholesky factorization failed after the declared jitter ladder.");
}

arma::mat inv_from_lower(const arma::mat& L) {
  arma::mat eye = arma::eye(L.n_rows, L.n_cols);
  arma::mat Linv = arma::solve(arma::trimatl(L), eye);
  return Linv.t() * Linv;
}

DrawResult mvn_draw(const arma::vec& mean, const arma::mat& covariance,
                    const arma::vec& ladder, const bool allow_repair) {
  arma::mat cov = symm(covariance);
  const double matrix_scale = arma::abs(cov).max();
  arma::mat L;
  if (arma::chol(L, cov, "lower")) {
    return DrawResult{
      mean + L * arma::randn<arma::vec>(mean.n_elem), 0.0, 0.0,
      false, 0, NA_REAL, matrix_scale, matrix_scale, false
    };
  }
  arma::vec values;
  arma::mat vectors;
  if (!arma::eig_sym(values, vectors, cov)) {
    Rcpp::stop("Symmetric eigendecomposition failed for a Gaussian covariance.");
  }
  const double scale = arma::abs(values).max();
  const bool near_psd = scale == 0.0 || values.min() / scale >= -1e-10;
  if (near_psd) {
    const double min_eigenvalue = values.min();
    if (!allow_repair && arma::any(values < 0.0)) {
      Rcpp::stop(
        "Gaussian covariance has a negative eigenvalue, and projection is "
        "disabled under numerical_policy='fail'."
      );
    }
    int clamped = 0;
    for (arma::uword j = 0; j < values.n_elem; ++j) {
      if (values(j) < 0.0) { values(j) = 0.0; ++clamped; }
    }
    return DrawResult{
      mean + vectors * (arma::sqrt(values) % arma::randn<arma::vec>(mean.n_elem)),
      0.0, 0.0, true, clamped, min_eigenvalue, matrix_scale,
      matrix_scale, false
    };
  }
  if (!allow_repair) {
    Rcpp::stop(
      "Gaussian covariance is indefinite, and repair is disabled under "
      "numerical_policy='fail'."
    );
  }
  SpdResult fac = spd_factor(cov, ladder);
  return DrawResult{
    mean + fac.chol_lower * arma::randn<arma::vec>(mean.n_elem),
    fac.jitter, fac.relative_jitter, false, 0, fac.min_eigenvalue,
    fac.matrix_scale, fac.jitter_scale, fac.absolute_jitter_fallback
  };
}

} // namespace

//' C++ Gaussian covariance-draw diagnostic kernel
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rqr_mvn_draw_cpp(const arma::vec& mean,
                            const arma::mat& covariance,
                            const arma::vec& jitter_ladder,
                            const bool allow_repair) {
  Rcpp::RNGScope scope;
  DrawResult result = mvn_draw(mean, covariance, jitter_ladder, allow_repair);
  return Rcpp::List::create(
    Rcpp::Named("draw") = result.draw,
    Rcpp::Named("info") = Rcpp::List::create(
      Rcpp::Named("strategy") = result.jitter > 0.0 ? "cholesky_jitter" :
        (result.used_psd ? "psd_eigen" : "cholesky"),
      Rcpp::Named("jitter") = result.jitter,
      Rcpp::Named("relative_jitter") = result.relative_jitter,
      Rcpp::Named("min_eigenvalue") = result.min_eigenvalue,
      Rcpp::Named("matrix_scale") = result.matrix_scale,
      Rcpp::Named("jitter_scale") = result.jitter_scale,
      Rcpp::Named("absolute_jitter_fallback") =
        result.absolute_jitter_fallback,
      Rcpp::Named("clamped_eigenvalues") = result.clamped_eigenvalues
    )
  );
}

//' C++ scalar-observation FFBS kernel
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rqr_ffbs_cpp(const arma::vec& z,
                        const arma::mat& H,
                        const arma::vec& V,
                        const arma::cube& GG,
                        const arma::vec& m0,
                        const arma::mat& C0,
                        const int evolution_mode,
                        const arma::cube& W,
                        const arma::mat& D,
                        const bool sample_path,
                        const arma::vec& jitter_ladder,
                        const std::string evolution_label,
                        const bool allow_covariance_repair) {
  Rcpp::RNGScope scope;
  const arma::uword p = m0.n_elem;
  const arma::uword T = z.n_elem;
  if (T < 1 || H.n_rows != p || H.n_cols != T || V.n_elem != T ||
      GG.n_rows != p || GG.n_cols != p || GG.n_slices != T ||
      C0.n_rows != p || C0.n_cols != p || W.n_rows != p ||
      W.n_cols != p || W.n_slices != T || D.n_rows != p || D.n_cols != p) {
    Rcpp::stop("Incompatible FFBS dimensions.");
  }
  if (evolution_mode != 0 && evolution_mode != 1) Rcpp::stop("Unknown evolution mode.");
  if (arma::any(V <= 0.0) || !V.is_finite() || !H.is_finite() || !GG.is_finite()) {
    Rcpp::stop("H, V, and GG must be finite and V positive.");
  }

  arma::mat a(p, T), m(p, T), sm(p, T);
  arma::cube R(p, p, T), C(p, p, T), sC(p, p, T);
  arma::vec q(T); q.fill(NA_REAL);
  arma::vec residual(T); residual.fill(NA_REAL);
  arma::vec mprev = m0;
  arma::mat Cprev = symm(C0);
  double max_jitter = 0.0;
  int jitter_count = 0;
  int psd_draw_count = 0;
  std::vector<std::string> repair_stage;
  std::vector<int> repair_time;
  std::vector<std::string> repair_strategy;
  std::vector<double> repair_jitter;
  std::vector<double> repair_relative_jitter;
  std::vector<double> repair_min_eigenvalue;
  std::vector<double> repair_matrix_scale;
  std::vector<double> repair_jitter_scale;
  std::vector<bool> repair_absolute_jitter_fallback;
  std::vector<int> repair_clamped;
  auto record_repair = [&](const std::string& stage, const int time,
                           const std::string& strategy, const double jitter,
                           const double relative_jitter, const double min_eigenvalue,
                           const double matrix_scale, const double jitter_scale,
                           const bool absolute_jitter_fallback,
                           const int clamped) {
    if (jitter <= 0.0 && clamped <= 0) return;
    repair_stage.push_back(stage);
    repair_time.push_back(time);
    repair_strategy.push_back(strategy);
    repair_jitter.push_back(jitter);
    repair_relative_jitter.push_back(relative_jitter);
    repair_min_eigenvalue.push_back(min_eigenvalue);
    repair_matrix_scale.push_back(matrix_scale);
    repair_jitter_scale.push_back(jitter_scale);
    repair_absolute_jitter_fallback.push_back(absolute_jitter_fallback);
    repair_clamped.push_back(clamped);
  };

  for (arma::uword t = 0; t < T; ++t) {
    const arma::mat Gt = GG.slice(t);
    a.col(t) = Gt * mprev;
    arma::mat P = symm(Gt * Cprev * Gt.t());
    arma::mat Wt = evolution_mode == 0 ? W.slice(t) : D % P;
    R.slice(t) = symm(P + Wt);
    const double zt = z(t);
    if (R_IsNaN(zt) && !R_IsNA(zt)) Rcpp::stop("z may contain finite values or NA only; NaN is invalid.");
    if (std::isinf(zt)) Rcpp::stop("z may contain finite values or NA only; Inf is invalid.");
    if (!R_IsNA(zt)) {
      const arma::vec h = H.col(t);
      const arma::vec rh = R.slice(t) * h;
      q(t) = arma::dot(h, rh) + V(t);
      if (!std::isfinite(q(t)) || q(t) <= 0.0) Rcpp::stop("Nonpositive forecast variance.");
      residual(t) = z(t) - arma::dot(h, a.col(t));
      m.col(t) = a.col(t) + rh * residual(t) / q(t);
      C.slice(t) = symm(R.slice(t) - (rh * rh.t()) / q(t));
    } else {
      m.col(t) = a.col(t);
      C.slice(t) = R.slice(t);
    }
    SpdResult cf = spd_factor(C.slice(t), jitter_ladder);
    C.slice(t) = cf.matrix;
    if (cf.jitter > 0.0) {
      ++jitter_count; max_jitter = std::max(max_jitter, cf.jitter);
      record_repair("filter_covariance", t + 1, "cholesky_jitter", cf.jitter,
                    cf.relative_jitter, cf.min_eigenvalue, cf.matrix_scale,
                    cf.jitter_scale, cf.absolute_jitter_fallback, 0);
    }
    mprev = m.col(t);
    Cprev = C.slice(t);
  }

  sm = m;
  sC = C;
  if (T > 1) {
    for (arma::sword ti = static_cast<arma::sword>(T) - 2; ti >= 0; --ti) {
      const arma::uword t = static_cast<arma::uword>(ti);
      SpdResult rf = spd_factor(R.slice(t + 1), jitter_ladder);
      if (rf.jitter > 0.0) {
        ++jitter_count; max_jitter = std::max(max_jitter, rf.jitter);
        record_repair("backward_smoothing_prior_covariance", t + 2,
                      "cholesky_jitter", rf.jitter, rf.relative_jitter,
                      rf.min_eigenvalue, rf.matrix_scale, rf.jitter_scale,
                      rf.absolute_jitter_fallback, 0);
      }
      arma::mat B = C.slice(t) * GG.slice(t + 1).t() * inv_from_lower(rf.chol_lower);
      sm.col(t) = m.col(t) + B * (sm.col(t + 1) - a.col(t + 1));
      sC.slice(t) = symm(C.slice(t) + B * (sC.slice(t + 1) - rf.matrix) * B.t());
    }
  }

  Rcpp::RObject path_out = R_NilValue;
  if (sample_path) {
    arma::mat path(p, T);
    DrawResult terminal = mvn_draw(
      m.col(T - 1), C.slice(T - 1), jitter_ladder, allow_covariance_repair
    );
    path.col(T - 1) = terminal.draw;
    if (terminal.used_psd) ++psd_draw_count;
    if (terminal.jitter > 0.0) { ++jitter_count; max_jitter = std::max(max_jitter, terminal.jitter); }
    record_repair("terminal_draw_covariance", T,
                  terminal.jitter > 0.0 ? "cholesky_jitter" : "eigen_clamp",
                  terminal.jitter, terminal.relative_jitter,
                  terminal.min_eigenvalue, terminal.matrix_scale,
                  terminal.jitter_scale, terminal.absolute_jitter_fallback,
                  terminal.clamped_eigenvalues);
    if (T > 1) {
      for (arma::sword ti = static_cast<arma::sword>(T) - 2; ti >= 0; --ti) {
        const arma::uword t = static_cast<arma::uword>(ti);
        SpdResult rf = spd_factor(R.slice(t + 1), jitter_ladder);
        if (rf.jitter > 0.0) {
          ++jitter_count; max_jitter = std::max(max_jitter, rf.jitter);
          record_repair("backward_sampling_prior_covariance", t + 2,
                        "cholesky_jitter", rf.jitter, rf.relative_jitter,
                        rf.min_eigenvalue, rf.matrix_scale, rf.jitter_scale,
                        rf.absolute_jitter_fallback, 0);
        }
        arma::mat B = C.slice(t) * GG.slice(t + 1).t() * inv_from_lower(rf.chol_lower);
        arma::vec h = m.col(t) + B * (path.col(t + 1) - a.col(t + 1));
        arma::mat HC = symm(C.slice(t) - B * rf.matrix * B.t());
        DrawResult state = mvn_draw(h, HC, jitter_ladder, allow_covariance_repair);
        path.col(t) = state.draw;
        if (state.used_psd) ++psd_draw_count;
        if (state.jitter > 0.0) { ++jitter_count; max_jitter = std::max(max_jitter, state.jitter); }
        record_repair("backward_draw_covariance", t + 1,
                      state.jitter > 0.0 ? "cholesky_jitter" : "eigen_clamp",
                      state.jitter, state.relative_jitter,
                      state.min_eigenvalue, state.matrix_scale,
                      state.jitter_scale, state.absolute_jitter_fallback,
                      state.clamped_eigenvalues);
      }
    }
    path_out = Rcpp::wrap(path);
  }

  double min_q = NA_REAL;
  for (arma::uword t = 0; t < T; ++t) {
    if (std::isfinite(q(t))) min_q = std::isfinite(min_q) ? std::min(min_q, q(t)) : q(t);
  }
  return Rcpp::List::create(
    Rcpp::Named("filter_mean") = m,
    Rcpp::Named("filter_cov") = C,
    Rcpp::Named("prior_mean") = a,
    Rcpp::Named("prior_cov") = R,
    Rcpp::Named("smooth_mean") = sm,
    Rcpp::Named("smooth_cov") = sC,
    Rcpp::Named("path") = path_out,
    Rcpp::Named("forecast_variance") = q,
    Rcpp::Named("residual") = residual,
    Rcpp::Named("diagnostics") = Rcpp::List::create(
      Rcpp::Named("backend") = "cpp",
      Rcpp::Named("evolution_mode") = evolution_label,
      Rcpp::Named("max_jitter") = max_jitter,
      Rcpp::Named("jitter_count") = jitter_count,
      Rcpp::Named("psd_draw_count") = psd_draw_count,
      Rcpp::Named("repair_count") = static_cast<int>(repair_stage.size()),
      Rcpp::Named("repair_records") = Rcpp::DataFrame::create(
        Rcpp::Named("stage") = repair_stage,
        Rcpp::Named("time") = repair_time,
        Rcpp::Named("strategy") = repair_strategy,
        Rcpp::Named("jitter") = repair_jitter,
        Rcpp::Named("relative_jitter") = repair_relative_jitter,
        Rcpp::Named("min_eigenvalue") = repair_min_eigenvalue,
        Rcpp::Named("matrix_scale") = repair_matrix_scale,
        Rcpp::Named("jitter_scale") = repair_jitter_scale,
        Rcpp::Named("absolute_jitter_fallback") =
          repair_absolute_jitter_fallback,
        Rcpp::Named("clamped_eigenvalues") = repair_clamped,
        Rcpp::Named("stringsAsFactors") = false
      ),
      Rcpp::Named("min_forecast_variance") = min_q
    )
  );
}
