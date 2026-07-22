library(testthat)
library(rqrgibbs)

# The repository also carries copied tests for the pinned exdqlm reference
# implementation. Package checks exercise the native standalone gates only;
# `make test-exdqlm-rqr` retains the separate reference-suite contract.
test_check("rqrgibbs", filter = "native")
