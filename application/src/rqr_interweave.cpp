#include <RcppArmadillo.h>
#include <cmath>
#include <vector>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

// [[Rcpp::export]]
Rcpp::List rqr_noncentered_basis_cpp(
    const arma::mat& theta,
    const arma::vec& theta0,
    const arma::cube& GG,
    const Rcpp::IntegerVector& component_dims,
    const arma::vec& q) {
  const arma::uword p = theta.n_rows;
  const arma::uword n_time = theta.n_cols;
  const int n_components = component_dims.size();
  if (p == 0 || n_time == 0 || theta0.n_elem != p ||
      GG.n_rows != p || GG.n_cols != p || GG.n_slices != n_time ||
      n_components < 1 || q.n_elem != static_cast<arma::uword>(n_components) ||
      !theta.is_finite() || !theta0.is_finite() || !GG.is_finite() ||
      !q.is_finite() || arma::any(q <= 0.0)) {
    Rcpp::stop("The C++ noncentered path-basis inputs are invalid.");
  }
  arma::uword dimension_sum = 0;
  std::vector<arma::uword> starts(n_components);
  std::vector<arma::uword> ends(n_components);
  for (int j = 0; j < n_components; ++j) {
    const int dimension = component_dims[j];
    if (dimension < 1) {
      Rcpp::stop("The C++ component dimensions must be positive.");
    }
    starts[j] = dimension_sum;
    dimension_sum += static_cast<arma::uword>(dimension);
    ends[j] = dimension_sum - 1;
  }
  if (dimension_sum != p) {
    Rcpp::stop("The C++ component dimensions do not sum to the state size.");
  }

  arma::mat standardized(p, n_time, arma::fill::zeros);
  arma::mat baseline(p, n_time, arma::fill::zeros);
  arma::cube basis(p, n_time, n_components, arma::fill::zeros);
  arma::vec previous_theta = theta0;
  arma::vec previous_baseline = theta0;
  arma::mat previous_basis(p, n_components, arma::fill::zeros);

  for (arma::uword tt = 0; tt < n_time; ++tt) {
    const arma::mat& transition = GG.slice(tt);
    const arma::vec innovation =
      theta.col(tt) - transition * previous_theta;
    baseline.col(tt) = transition * previous_baseline;
    arma::mat current_basis = transition * previous_basis;
    for (int j = 0; j < n_components; ++j) {
      const arma::span block(starts[j], ends[j]);
      standardized(block, tt) =
        innovation(block) / std::sqrt(q(j));
      current_basis(block, j) += standardized(block, tt);
      basis.slice(j).col(tt) = current_basis.col(j);
    }
    previous_theta = theta.col(tt);
    previous_baseline = baseline.col(tt);
    previous_basis = current_basis;
  }
  if (!standardized.is_finite() || !baseline.is_finite() ||
      !basis.is_finite()) {
    Rcpp::stop("The C++ noncentered path basis is nonfinite.");
  }
  return Rcpp::List::create(
    Rcpp::Named("standardized") = standardized,
    Rcpp::Named("baseline") = baseline,
    Rcpp::Named("basis") = basis
  );
}
