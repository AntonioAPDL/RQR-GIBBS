library(testthat)
library(rqrgibbs)

# The repository also carries copied tests for the pinned exdqlm reference
# implementation. Package checks exercise the native package and the
# opt-in intercept profile. Repository-script assessment tests remain in
# `make test-mti-profile`, because scripts are excluded from the R package.
# Repository-level bounded and main-simulation workflow tests are run by
# `make test-standalone-contracts`; `make test-exdqlm-rqr` retains the
# separate pinned-reference suite.
test_check(
  "rqrgibbs",
  filter = "native|mti-intercept-profile|mti-content-grid"
)
