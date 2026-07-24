library(testthat)
library(rqrgibbs)

# The repository also carries copied tests for the pinned exdqlm reference
# implementation. Package checks exercise the native package contract.
# Repository-level bounded and main-simulation workflow tests are run by
# `make test-standalone-contracts`; `make test-exdqlm-rqr` retains the
# separate pinned-reference suite.
test_check(
  "rqrgibbs",
  filter = "native"
)
