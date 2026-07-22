#include <RcppArmadillo.h>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

namespace {

struct SpdResult {
  arma::mat matrix;
  arma::mat chol_lower;
  double jitter;
};

arma::mat symm(const arma::mat& x) {
  return 0.5 * (x + x.t());
}

SpdResult spd_factor(const arma::mat& input, const arma::vec& ladder) {
  arma::mat base = symm(input);
  const double scale = std::max(1.0, arma::abs(base.diag()).max());
  arma::mat L;
  for (arma::uword j = 0; j < ladder.n_elem; ++j) {
    const double relative = ladder(j);
    if (!std::isfinite(relative) || relative < 0.0) continue;
    arma::mat candidate = base;
    if (relative > 0.0) candidate.diag() += relative * scale;
    if (arma::chol(L, candidate, "lower")) {
      return SpdResult{candidate, L, relative * scale};
    }
  }
  Rcpp::stop("Positive-definite Cholesky factorization failed after the declared jitter ladder.");
}

arma::mat inv_from_lower(const arma::mat& L) {
  arma::mat eye = arma::eye(L.n_rows, L.n_cols);
  arma::mat Linv = arma::solve(arma::trimatl(L), eye);
  return Linv.t() * Linv;
}

arma::vec mvn_draw(const arma::vec& mean, const arma::mat& covariance,
                   const arma::vec& ladder, double& jitter, bool& used_psd) {
  arma::mat cov = symm(covariance);
  arma::mat L;
  jitter = 0.0;
  used_psd = false;
  if (arma::chol(L, cov, "lower")) {
    return mean + L * arma::randn<arma::vec>(mean.n_elem);
  }
  arma::vec values;
  arma::mat vectors;
  if (!arma::eig_sym(values, vectors, cov)) {
    Rcpp::stop("Symmetric eigendecomposition failed for a Gaussian covariance.");
  }
  const double scale = std::max(1.0, arma::abs(values).max());
  if (values.min() >= -1e-10 * scale) {
    values.transform([](double value) { return value < 0.0 ? 0.0 : value; });
    used_psd = true;
    return mean + vectors * (arma::sqrt(values) % arma::randn<arma::vec>(mean.n_elem));
  }
  SpdResult fac = spd_factor(cov, ladder);
  jitter = fac.jitter;
  return mean + fac.chol_lower * arma::randn<arma::vec>(mean.n_elem);
}

} // namespace

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
                        const std::string evolution_label) {
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

  for (arma::uword t = 0; t < T; ++t) {
    const arma::mat Gt = GG.slice(t);
    a.col(t) = Gt * mprev;
    arma::mat P = symm(Gt * Cprev * Gt.t());
    arma::mat Wt = evolution_mode == 0 ? W.slice(t) : D % P;
    R.slice(t) = symm(P + Wt);
    if (std::isfinite(z(t))) {
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
    if (cf.jitter > 0.0) { ++jitter_count; max_jitter = std::max(max_jitter, cf.jitter); }
    mprev = m.col(t);
    Cprev = C.slice(t);
  }

  sm = m;
  sC = C;
  if (T > 1) {
    for (arma::sword ti = static_cast<arma::sword>(T) - 2; ti >= 0; --ti) {
      const arma::uword t = static_cast<arma::uword>(ti);
      SpdResult rf = spd_factor(R.slice(t + 1), jitter_ladder);
      arma::mat B = C.slice(t) * GG.slice(t + 1).t() * inv_from_lower(rf.chol_lower);
      sm.col(t) = m.col(t) + B * (sm.col(t + 1) - a.col(t + 1));
      sC.slice(t) = symm(C.slice(t) + B * (sC.slice(t + 1) - R.slice(t + 1)) * B.t());
    }
  }

  Rcpp::RObject path_out = R_NilValue;
  if (sample_path) {
    arma::mat path(p, T);
    double draw_jitter = 0.0;
    bool used_psd = false;
    path.col(T - 1) = mvn_draw(
      m.col(T - 1), C.slice(T - 1), jitter_ladder, draw_jitter, used_psd
    );
    if (used_psd) ++psd_draw_count;
    if (draw_jitter > 0.0) { ++jitter_count; max_jitter = std::max(max_jitter, draw_jitter); }
    if (T > 1) {
      for (arma::sword ti = static_cast<arma::sword>(T) - 2; ti >= 0; --ti) {
        const arma::uword t = static_cast<arma::uword>(ti);
        SpdResult rf = spd_factor(R.slice(t + 1), jitter_ladder);
        arma::mat B = C.slice(t) * GG.slice(t + 1).t() * inv_from_lower(rf.chol_lower);
        arma::vec h = m.col(t) + B * (path.col(t + 1) - a.col(t + 1));
        arma::mat HC = symm(C.slice(t) - B * R.slice(t + 1) * B.t());
        path.col(t) = mvn_draw(h, HC, jitter_ladder, draw_jitter, used_psd);
        if (used_psd) ++psd_draw_count;
        if (draw_jitter > 0.0) { ++jitter_count; max_jitter = std::max(max_jitter, draw_jitter); }
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
      Rcpp::Named("min_forecast_variance") = min_q
    )
  );
}
