#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

namespace {
double fractional_sum(std::vector<double> y, double mass, bool reverse) {
  std::sort(y.begin(), y.end());
  if (reverse) std::reverse(y.begin(), y.end());
  const int k = static_cast<int>(std::floor(mass));
  long double sum = 0;
  for (int i = 0; i < k; ++i) sum += y[i];
  if (mass > k) sum += (mass - k) * y[k];
  return static_cast<double>(sum);
}

struct Gradient { double lower, upper; };

Gradient profile_gradient(const std::vector<double>& y, double m, double mass,
                          double q, double delta, double omega,
                          double precision, double center) {
  const int n = y.size();
  std::vector<double> distances(n);
  double scale = std::max(1.0, std::abs(m));
  long double sum_y = 0;
  for (int i = 0; i < n; ++i) {
    distances[i] = std::abs(y[i] - m);
    scale = std::max(scale, std::abs(y[i]));
    sum_y += y[i];
  }
  std::vector<double> sorted = distances;
  std::sort(sorted.begin(), sorted.end());
  const int k = static_cast<int>(std::floor(mass));
  const int rank = mass > k ? k : std::max(0, k - 1);
  const double radius = sorted[rank];
  const double error = 32 * std::numeric_limits<double>::epsilon() * scale;
  std::vector<double> boundary;
  long double inside_sum = 0;
  int inside_count = 0;
  for (int i = 0; i < n; ++i) {
    const double difference = distances[i] - radius;
    if (difference < -error) { inside_sum += y[i]; ++inside_count; }
    else if (std::abs(difference) <= error) boundary.push_back(y[i]);
  }
  std::sort(boundary.begin(), boundary.end());
  const double remaining = mass - inside_count;
  long double low = inside_sum, high = inside_sum;
  for (std::size_t i = 0; i < boundary.size(); ++i) {
    const double weight = std::min(1.0, std::max(0.0, remaining - i));
    low += weight * boundary[i];
    high += weight * boundary[boundary.size() - i - 1];
  }
  const long double offset = 4.0L * precision * m - 2.0L * precision * center -
    2.0L * omega * (q * sum_y + n * q * delta);
  return {static_cast<double>(offset + 2.0L * omega * low),
          static_cast<double>(offset + 2.0L * omega * high)};
}
}

// [[Rcpp::export(name = ".rqr_mti_profile_intercept_cpp")]]
Rcpp::List rqr_mti_profile_intercept_cpp(const Rcpp::NumericVector& observations,
    double q, double delta, double omega, double precision, double center,
    int max_iter, double parameter_tolerance, bool store_trace) {
  const int n = observations.size();
  if (n < 2 || !std::isfinite(q) || q <= 0 || q >= 1 ||
      !std::isfinite(delta) || !std::isfinite(omega) || omega <= 0 ||
      !std::isfinite(precision) || precision <= 0 || !std::isfinite(center) ||
      max_iter < 1 || !std::isfinite(parameter_tolerance) || parameter_tolerance <= 0) {
    Rcpp::stop("Invalid fixed-target intercept profile inputs.");
  }
  std::vector<double> y(observations.begin(), observations.end());
  long double sy = 0;
  double scale = 1;
  for (double v : y) {
    if (!std::isfinite(v)) Rcpp::stop("Observations must be finite.");
    sy += v;
    scale = std::max(scale, std::abs(v));
  }
  const double mass = n * q - precision / omega;
  std::vector<int> tr_iteration;
  std::vector<double> tr_m, tr_lo, tr_hi, tr_gl, tr_gh;
  double m;
  int iteration = 0;
  if (mass <= 0) {
    m = static_cast<double>((omega * q * (sy + n * delta) + precision * center) /
      (omega * n * q + precision));
  } else {
    const long double offset = omega * (q * sy + n * q * delta) + precision * center;
    double lo = static_cast<double>((offset - omega * fractional_sum(y, mass, true)) /
      (2 * precision));
    double hi = static_cast<double>((offset - omega * fractional_sum(y, mass, false)) /
      (2 * precision));
    if (!std::isfinite(lo) || !std::isfinite(hi)) {
      Rcpp::stop("The fixed-target midpoint bracket is not finite.");
    }
    m = lo + (hi - lo) / 2;
    for (iteration = 1; iteration <= max_iter; ++iteration) {
      m = lo + (hi - lo) / 2;
      const Gradient g = profile_gradient(y, m, mass, q, delta, omega, precision, center);
      if (store_trace) {
        tr_iteration.push_back(iteration); tr_m.push_back(m);
        tr_lo.push_back(lo); tr_hi.push_back(hi);
        tr_gl.push_back(g.lower); tr_gh.push_back(g.upper);
      }
      if (g.lower <= 0 && g.upper >= 0) break;
      if (hi - lo <= parameter_tolerance * std::max(scale, std::abs(m))) break;
      if (g.upper < 0) lo = m; else hi = m;
    }
    iteration = std::min(iteration, max_iter);
  }
  return Rcpp::List::create(Rcpp::Named("midpoint") = m,
    Rcpp::Named("iterations") = iteration,
    Rcpp::Named("trace") = Rcpp::DataFrame::create(
      Rcpp::Named("iteration") = tr_iteration, Rcpp::Named("midpoint") = tr_m,
      Rcpp::Named("bracket_lower") = tr_lo, Rcpp::Named("bracket_upper") = tr_hi,
      Rcpp::Named("gradient_lower") = tr_gl, Rcpp::Named("gradient_upper") = tr_gh));
}
