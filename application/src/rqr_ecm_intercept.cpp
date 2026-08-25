#include <Rcpp.h>
#include <cmath>
#include <string>
#include <vector>

// [[Rcpp::plugins(cpp17)]]

namespace {

double sample_variance(const Rcpp::NumericVector& y) {
  const int n = y.size();
  if (n < 2) return NA_REAL;
  double mean = 0.0;
  for (int i = 0; i < n; ++i) mean += y[i];
  mean /= static_cast<double>(n);
  double ss = 0.0;
  for (int i = 0; i < n; ++i) {
    const double d = y[i] - mean;
    ss += d * d;
  }
  return ss / static_cast<double>(n - 1);
}

double product_scale(const Rcpp::NumericVector& y) {
  const int n = y.size();
  double scale = sample_variance(y);
  if (!std::isfinite(scale) || scale <= 0.0) {
    double mean = 0.0;
    for (int i = 0; i < n; ++i) mean += y[i];
    mean /= static_cast<double>(n);
    double ss = 0.0;
    for (int i = 0; i < n; ++i) {
      const double d = y[i] - mean;
      ss += d * d;
    }
    scale = ss / static_cast<double>(n);
  }
  if (!std::isfinite(scale) || scale <= 0.0) scale = 1.0;
  return std::max(scale, 1.0);
}

void validate_common(const Rcpp::NumericVector& y,
                     const Rcpp::NumericVector& delta,
                     const double q,
                     const double omega,
                     const double prior_prec) {
  const int n = y.size();
  if (n < 1 || delta.size() != n) {
    Rcpp::stop("C++ intercept ECM requires y and mean_tilt with the same positive length.");
  }
  if (!std::isfinite(q) || q <= 0.0 || q >= 1.0 ||
      !std::isfinite(omega) || omega <= 0.0 ||
      !std::isfinite(prior_prec) || prior_prec <= 0.0) {
    Rcpp::stop("C++ intercept ECM received invalid q, learning rate, or prior precision.");
  }
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(y[i]) || !std::isfinite(delta[i])) {
      Rcpp::stop("C++ intercept ECM inputs must be finite.");
    }
  }
}

Rcpp::NumericVector residuals(const Rcpp::NumericVector& y,
                              const double b1,
                              const double b2) {
  const int n = y.size();
  Rcpp::NumericVector out(n);
  for (int i = 0; i < n; ++i) out[i] = (y[i] - b1) * (y[i] - b2);
  return out;
}

double objective_total(const Rcpp::NumericVector& y,
                       const Rcpp::NumericVector& delta,
                       const double b1,
                       const double b2,
                       const double q,
                       const double omega,
                       const double prior_prec) {
  const int n = y.size();
  double product = 0.0;
  double linear = 0.0;
  for (int i = 0; i < n; ++i) {
    const double e = (y[i] - b1) * (y[i] - b2);
    product += e * (q - (e < 0.0 ? 1.0 : 0.0));
    linear += q * delta[i] * (b1 + b2 - 2.0 * y[i]);
  }
  return omega * product - omega * linear +
    0.5 * prior_prec * (b1 * b1 + b2 * b2);
}

Rcpp::List stationarity(const Rcpp::NumericVector& y,
                        const Rcpp::NumericVector& delta,
                        const double b1,
                        const double b2,
                        const double q,
                        const double omega,
                        const double prior_prec) {
  const int n = y.size();
  double sum_delta = 0.0;
  for (int i = 0; i < n; ++i) sum_delta += delta[i];
  const double tilt_shift = omega * q * sum_delta;
  double g1 = prior_prec * b1 - tilt_shift;
  double g2 = prior_prec * b2 - tilt_shift;
  double min_abs_e = R_PosInf;
  int zero_count = 0;
  for (int i = 0; i < n; ++i) {
    const double e = (y[i] - b1) * (y[i] - b2);
    double psi = q - (e < 0.0 ? 1.0 : 0.0);
    if (e == 0.0) {
      psi = q - 0.5;
      ++zero_count;
    }
    g1 += -omega * (y[i] - b2) * psi;
    g2 += -omega * (y[i] - b1) * psi;
    min_abs_e = std::min(min_abs_e, std::abs(e));
  }
  const double max_abs = std::max(std::abs(g1), std::abs(g2));
  return Rcpp::List::create(
    Rcpp::Named("max_abs_midpoint_gradient") = max_abs,
    Rcpp::Named("minimum_absolute_residual_product") = min_abs_e,
    Rcpp::Named("zero_residual_count") = zero_count
  );
}

Rcpp::List inverse_latent(const Rcpp::NumericVector& e,
                          const double q,
                          const double floor,
                          const std::string& floor_type) {
  const int n = e.size();
  Rcpp::NumericVector tau(n);
  double min_abs = R_PosInf;
  int zero_count = 0;
  for (int i = 0; i < n; ++i) {
    const double abs_e = std::abs(e[i]);
    min_abs = std::min(min_abs, abs_e);
    if (abs_e == 0.0) ++zero_count;
    double scale = abs_e;
    if (floor > 0.0) {
      if (floor_type == "hard") {
        scale = std::max(abs_e, floor);
      } else {
        scale = std::sqrt(e[i] * e[i] + floor * floor);
      }
    } else if (abs_e == 0.0) {
      Rcpp::stop("C++ intercept ECM encountered a zero residual product without a positive floor.");
    }
    tau[i] = 1.0 / (q * (1.0 - q) * scale);
  }
  return Rcpp::List::create(
    Rcpp::Named("tau") = tau,
    Rcpp::Named("minimum_absolute_residual_product") = min_abs,
    Rcpp::Named("zero_residual_count") = zero_count
  );
}

double update_root(const Rcpp::NumericVector& y,
                   const Rcpp::NumericVector& delta,
                   const Rcpp::NumericVector& tau,
                   const double beta_other,
                   const double q,
                   const double omega,
                   const double prior_prec) {
  const int n = y.size();
  const double xi = (1.0 - 2.0 * q) / (q * (1.0 - q));
  const double phi_sigma = 2.0 / (omega * q * (1.0 - q));
  double precision = prior_prec;
  double rhs = 0.0;
  double sum_delta = 0.0;
  for (int i = 0; i < n; ++i) {
    const double A = y[i] - beta_other;
    const double c_component = y[i] * (y[i] - beta_other);
    const double weight = tau[i] / phi_sigma;
    precision += A * A * weight;
    rhs += A * (tau[i] * c_component - xi) / phi_sigma;
    sum_delta += delta[i];
  }
  rhs += omega * q * sum_delta;
  if (!std::isfinite(precision) || precision <= 0.0 ||
      !std::isfinite(rhs)) {
    Rcpp::stop("C++ intercept ECM produced an invalid scalar Gaussian system.");
  }
  return rhs / precision;
}

void canonicalize(double& b1, double& b2, const bool enabled) {
  if (enabled && b1 > b2) std::swap(b1, b2);
}

} // namespace

//' C++ intercept-only fixed-target MTI-ECM start
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rqr_ecm_intercept_run_cpp(
    const Rcpp::NumericVector& y,
    const double coverage_level,
    const double learning_rate,
    const Rcpp::NumericVector& mean_tilt,
    const double prior_prec,
    const double beta1_start,
    const double beta2_start,
    const int max_iter,
    const double tol_objective,
    const double tol_parameters,
    const double tol_stationarity,
    const int stable_iterations,
    const double residual_product_floor,
    const Rcpp::NumericVector& floor_schedule,
    const std::string floor_type,
    const bool monotone_backtracking,
    const int backtracking_max_steps,
    const double monotone_tolerance,
    const bool canonicalize_complete_roots,
    const std::string start_label) {
  validate_common(y, mean_tilt, coverage_level, learning_rate, prior_prec);
  if (max_iter < 1 || stable_iterations < 1 ||
      !std::isfinite(tol_objective) || tol_objective < 0.0 ||
      !std::isfinite(tol_parameters) || tol_parameters < 0.0 ||
      !std::isfinite(tol_stationarity) || tol_stationarity < 0.0 ||
      !std::isfinite(residual_product_floor) || residual_product_floor < 0.0 ||
      floor_schedule.size() < 1) {
    Rcpp::stop("C++ intercept ECM received invalid control values.");
  }
  if (floor_type != "hard" && floor_type != "smooth") {
    Rcpp::stop("C++ intercept ECM floor_type must be 'hard' or 'smooth'.");
  }

  double b1 = beta1_start;
  double b2 = beta2_start;
  if (!std::isfinite(b1) || !std::isfinite(b2)) {
    Rcpp::stop("C++ intercept ECM start values must be finite.");
  }
  canonicalize(b1, b2, canonicalize_complete_roots);

  const double scale = product_scale(y);
  const double base_floor = residual_product_floor * scale;
  std::vector<double> floors;
  floors.reserve(floor_schedule.size());
  for (int i = 0; i < floor_schedule.size(); ++i) {
    if (!std::isfinite(floor_schedule[i]) || floor_schedule[i] <= 0.0) {
      Rcpp::stop("C++ intercept ECM floor_schedule must contain positive values.");
    }
    floors.push_back(base_floor * floor_schedule[i]);
  }

  Rcpp::NumericVector e0 = residuals(y, b1, b2);
  double obj = objective_total(
    y, mean_tilt, b1, b2, coverage_level, learning_rate, prior_prec
  );
  double min_abs0 = R_PosInf;
  for (int i = 0; i < e0.size(); ++i) {
    min_abs0 = std::min(min_abs0, std::abs(e0[i]));
  }

  std::vector<std::string> trace_label;
  std::vector<int> trace_stage;
  std::vector<int> trace_iteration;
  std::vector<double> trace_objective;
  std::vector<double> trace_rel_obj;
  std::vector<double> trace_rel_param;
  std::vector<double> trace_stationarity;
  std::vector<double> trace_min_abs;
  std::vector<double> trace_floor;
  std::vector<int> trace_backtracking;
  std::vector<double> trace_step;
  std::vector<bool> trace_swapped;
  std::vector<int> trace_repairs;
  std::vector<double> trace_cond1;
  std::vector<double> trace_cond2;
  std::vector<double> trace_root1;
  std::vector<double> trace_root2;
  std::vector<double> trace_width;

  auto add_trace = [&](const int stage, const int iteration,
                       const double objective, const double rel_obj,
                       const double rel_param, const double stat,
                       const double min_abs, const double floor,
                       const int backtracking, const double step,
                       const bool swapped, const double root1,
                       const double root2) {
    trace_label.push_back(start_label);
    trace_stage.push_back(stage);
    trace_iteration.push_back(iteration);
    trace_objective.push_back(objective);
    trace_rel_obj.push_back(rel_obj);
    trace_rel_param.push_back(rel_param);
    trace_stationarity.push_back(stat);
    trace_min_abs.push_back(min_abs);
    trace_floor.push_back(floor);
    trace_backtracking.push_back(backtracking);
    trace_step.push_back(step);
    trace_swapped.push_back(swapped);
    trace_repairs.push_back(0);
    trace_cond1.push_back(1.0);
    trace_cond2.push_back(1.0);
    trace_root1.push_back(root1);
    trace_root2.push_back(root2);
    trace_width.push_back(root2 - root1);
  };

  add_trace(0, 0, obj, NA_REAL, NA_REAL, NA_REAL, min_abs0, NA_REAL,
            0, 0.0, false, b1, b2);

  std::string convergence_code = "max_iter_reached";
  bool converged = false;
  bool stalled = false;
  int stable_count = 0;
  int total_backtracking = 0;
  int root_swap_count = 0;
  int iterations = 0;
  bool zero_residual_encountered = false;
  const int stage_iter_limit =
    std::max(1, static_cast<int>(std::ceil(
      static_cast<double>(max_iter) / static_cast<double>(floors.size())
    )));

  for (std::size_t stage = 0; stage < floors.size(); ++stage) {
    const double floor_current = floors[stage];
    for (int stage_iter = 0; stage_iter < stage_iter_limit; ++stage_iter) {
      if (iterations >= max_iter || stalled || converged) break;
      ++iterations;
      const double b1_old = b1;
      const double b2_old = b2;
      const double obj_old = obj;

      Rcpp::NumericVector e_old = residuals(y, b1_old, b2_old);
      Rcpp::List latent = inverse_latent(
        e_old, coverage_level, floor_current, floor_type
      );
      const int zero_count = Rcpp::as<int>(latent["zero_residual_count"]);
      zero_residual_encountered = zero_residual_encountered || zero_count > 0;
      Rcpp::NumericVector tau = latent["tau"];

      double b1_star = update_root(
        y, mean_tilt, tau, b2, coverage_level, learning_rate, prior_prec
      );
      double b2_star = update_root(
        y, mean_tilt, tau, b1_star, coverage_level, learning_rate, prior_prec
      );

      const bool swapped_before = canonicalize_complete_roots && b1_star > b2_star;
      canonicalize(b1_star, b2_star, canonicalize_complete_roots);
      if (swapped_before) ++root_swap_count;

      double candidate_obj = objective_total(
        y, mean_tilt, b1_star, b2_star, coverage_level, learning_rate,
        prior_prec
      );
      double step_size = 1.0;
      int backtracking_steps = 0;
      bool accepted = candidate_obj <= obj_old + monotone_tolerance;

      if (!accepted && monotone_backtracking) {
        for (int bt = 1; bt <= backtracking_max_steps; ++bt) {
          const double lambda = std::pow(0.5, bt);
          double bt_b1 = b1_old + lambda * (b1_star - b1_old);
          double bt_b2 = b2_old + lambda * (b2_star - b2_old);
          canonicalize(bt_b1, bt_b2, canonicalize_complete_roots);
          const double bt_obj = objective_total(
            y, mean_tilt, bt_b1, bt_b2, coverage_level, learning_rate,
            prior_prec
          );
          backtracking_steps = bt;
          if (bt_obj <= obj_old + monotone_tolerance) {
            b1_star = bt_b1;
            b2_star = bt_b2;
            candidate_obj = bt_obj;
            step_size = lambda;
            accepted = true;
            break;
          }
        }
      }

      if (!accepted) {
        convergence_code = "stalled_backtracking";
        stalled = true;
        break;
      }

      b1 = b1_star;
      b2 = b2_star;
      obj = candidate_obj;
      total_backtracking += backtracking_steps;

      const double parameter_change =
        std::max(std::abs(b1 - b1_old), std::abs(b2 - b2_old));
      const double parameter_scale =
        1.0 + std::max(std::abs(b1_old), std::abs(b2_old));
      const double relative_parameter_change =
        parameter_change / parameter_scale;
      const double relative_objective_change =
        std::abs(obj - obj_old) / (1.0 + std::abs(obj_old));

      Rcpp::List stat = stationarity(
        y, mean_tilt, b1, b2, coverage_level, learning_rate, prior_prec
      );
      const double stat_max = Rcpp::as<double>(
        stat["max_abs_midpoint_gradient"]
      );
      const double stat_min = Rcpp::as<double>(
        stat["minimum_absolute_residual_product"]
      );

      const bool is_final_stage = stage + 1 == floors.size();
      const bool stable = is_final_stage &&
        relative_objective_change <= tol_objective &&
        relative_parameter_change <= tol_parameters &&
        stat_max <= tol_stationarity;
      stable_count = stable ? stable_count + 1 : 0;
      if (stable_count >= stable_iterations) {
        converged = true;
        convergence_code = "converged";
      }

      add_trace(static_cast<int>(stage + 1), iterations, obj,
                relative_objective_change, relative_parameter_change,
                stat_max, stat_min, floor_current, backtracking_steps,
                step_size, swapped_before, b1, b2);
    }
  }

  if (!converged && !stalled && iterations < max_iter) {
    convergence_code = "floor_schedule_completed";
  }

  Rcpp::DataFrame trace = Rcpp::DataFrame::create(
    Rcpp::Named("start_label") = trace_label,
    Rcpp::Named("stage") = trace_stage,
    Rcpp::Named("iteration") = trace_iteration,
    Rcpp::Named("objective") = trace_objective,
    Rcpp::Named("relative_objective_change") = trace_rel_obj,
    Rcpp::Named("relative_parameter_change") = trace_rel_param,
    Rcpp::Named("stationarity") = trace_stationarity,
    Rcpp::Named("minimum_absolute_residual_product") = trace_min_abs,
    Rcpp::Named("residual_product_floor") = trace_floor,
    Rcpp::Named("backtracking_steps") = trace_backtracking,
    Rcpp::Named("step_size") = trace_step,
    Rcpp::Named("root_swap_after_cycle") = trace_swapped,
    Rcpp::Named("precision_repairs") = trace_repairs,
    Rcpp::Named("condition_number_root1") = trace_cond1,
    Rcpp::Named("condition_number_root2") = trace_cond2,
    Rcpp::Named("root1") = trace_root1,
    Rcpp::Named("root2") = trace_root2,
    Rcpp::Named("width") = trace_width
  );

  return Rcpp::List::create(
    Rcpp::Named("beta_root1") = b1,
    Rcpp::Named("beta_root2") = b2,
    Rcpp::Named("objective_trace") = trace,
    Rcpp::Named("iterations") = iterations,
    Rcpp::Named("converged") = converged,
    Rcpp::Named("convergence_code") = convergence_code,
    Rcpp::Named("backtracking_count") = total_backtracking,
    Rcpp::Named("precision_repairs") = 0,
    Rcpp::Named("root_swap_count") = root_swap_count,
    Rcpp::Named("zero_residual_encountered") = zero_residual_encountered,
    Rcpp::Named("selected_start_label") = start_label,
    Rcpp::Named("response_product_scale") = scale,
    Rcpp::Named("base_floor") = base_floor,
    Rcpp::Named("floor_schedule_absolute") = Rcpp::wrap(floors)
  );
}
