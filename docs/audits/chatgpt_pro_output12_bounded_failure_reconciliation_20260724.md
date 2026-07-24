# ChatGPT Pro Output-12 bounded-launch reconciliation

## Scope and exact states

This report treats the Output-12 review as a set of claims to reproduce rather
than as execution evidence. The exact remote review packet was:

```text
branch: origin/chatgpt-pro/output12-audit-20260724
tip:    b10816bce5c06917fcd61832b7b2687803a067a0
base:   85658e9378d25b12335bf70e7de936b889ef74dd
```

The four review files were the only changes relative to the declared base.
Their byte counts and SHA-256 values matched the delivered artifact manifest.
The four review commits were integrated without merging the review branch's
older ancestry.

Output-12 found no remaining source blocker and conditionally authorized a
separate false-to-true configuration commit followed by a fresh isolated
runtime, preflight, reference suite, fail-closed confirmation check, and the
user's explicit execution gate. The exact launch source was:

```text
authorization commit:
  00d489d686b44622454333d225f5ce55e1f760a5

launch commit:
  0deebc753bdb29e541d5fcd34e39917b5d17774e

package:
  rqrgibbs 0.1.0.9011

fit schema:
  rqrgibbs_fit/1.8.0
```

The launch commit differs from the authorization commit only by restoring the
executable modes of the bounded runner and monitor wrapper. A concurrent
Overleaf merge, `6f294de8fe44dc177a4501e13a5effef32840365`, changed those two
script modes from `100755` to `100644` without changing their contents. The
merge was preserved; the mode regression was repaired explicitly.

The first authorized execution found a real schema defect and stopped before
publishing any fit. The corrected implementation is:

```text
implementation commit:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

package:
  rqrgibbs 0.1.0.9012

fit schema:
  rqrgibbs_fit/1.9.0

application tree:
  5b50eb8fcb5e4748fbdc40662c81b0657edfad38
```

Execution was then disabled again at:

```text
revocation commit:
  0d64331732fe4118e7234f6f23a851f5d98e6614

bounded_dynamic_execution_authorized:
  FALSE
```

No second 24-fit attempt was made.

Protected repositories remained read-only:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

The exdqlm compatibility test used a Git archive and an isolated build under
the ignored RQR-GIBBS cache. It did not compile, install, or load from the
protected checkout.

## Executive disposition

```text
Output-12 packet authenticity:                       PASS
Output-12 conditional authorization protocol:        FOLLOWED
Fresh authorization preflight/reference binding:     PASS
First 24-fit attempt:                                FAILED CLOSED
Completed bounded-grid fits:                         0 / 24
Published bounded-grid chain files:                  0
Fixed-W time-zero state contract:                    CORRECTED
Shared reference/execution estimand extractor:        CORRECTED
All-six-cell continuation and schema references:     PASS
Fresh corrected reference suite:                     PASS, 43 / 43
Fresh corrected four-chain benchmark:                PASS, 150 / 150
Current execution authorization:                     FALSE
Retry 24-fit bounded grid now:                        NO-GO pending review
Matched/production simulation:                       NO-GO
CAVI/ELBO:                                            DEFER
RQR-DESN:                                             DEFER
```

The failed launch did not identify a defect in the RQR loss, pseudo-AL
augmentation, blocked root-specific FFBS conditionals, component discount
construction, shared component-scale conditional, missing-observation rule, or
future-root interpretation. It exposed a retained-state and validation-contract
gap at time zero.

## Exact fail-closed result

The bound authorization runtime passed preflight, all 43 reference gates, all
eight monitor scenarios, and the no-confirmation fail-closed check. The
authorized execution then stopped on its first scheduled fit:

```text
fixture:
  fixed_W_local_level

learning-rate mode:
  fixed_rate

chain:
  1

seed:
  84201

elapsed:
  127.383 seconds

error:
  Bounded estimand schema mismatch in chain(s) 1; expected schema
  rqrgibbs_dlm_bounded_estimands/1.0.0 with 117 ordered estimands.
```

The wrapper recorded:

```text
completed fits:              0
failed fits:                 1
unstarted fits:             23
published chain RDS files:   0
final process group empty:   true
peak sampled RSS:            183,712 KiB
```

The cell-level stop rule therefore worked as intended. No later chain or cell
started, and the failure did not leave a temporary or final chain object.

## Root cause

For the fixed-W local-level fixture, the frozen estimand schema contains:

```text
4 * T training root functionals:       4 * 24 = 96
observed-data loss:                             1
terminal midpoint and separation:              2
time-zero midpoint and separation:             2
4 * H future root functionals:          4 * 4 = 16
total:                                         117
```

The fit produced 115 estimands. Fixed-W and frozen-discount FFBS correctly
integrated the initial state through the declared
`theta_0 ~ N(m0, C0)` prior but did not retain a completed draw of `theta_0`.
The former execution-only extractor conditionally omitted the two time-zero
summaries when `samp.theta0_root1` and `samp.theta0_root2` were `NULL`.
The independent expected-name constructor still required them, so the launch
failed at the correct boundary.

The earlier reference suite compared `NULL` with `NULL` in fixed-W and
discount-template continuation checks. The representative benchmark used the
component-scale fixture, whose component-scale update already completes and
retains time-zero states. Those two facts explain why both gates could pass
without exercising the missing fixed-W contract.

## Exact time-zero completion

For each root, the declared initial transition is

```text
theta_0 ~ N(m0, C0)
theta_1 | theta_0 ~ N(G1 theta_0, W1).
```

Let

```text
R1 = G1 C0 G1' + W1.
```

The correction completes an FFBS path with the exact conditional:

```text
theta_0 | theta_1 ~ N(h0, H0)

h0 = m0 + C0 G1' R1^+ (theta_1 - G1 m0)

H0 = C0 - C0 G1' R1^+ G1 C0,
```

where `R1^+` is the ordinary inverse for a positive-definite forecast
covariance and the Moore-Penrose inverse on the supported subspace when `R1`
is singular. The singular branch:

- rejects a materially indefinite forecast covariance;
- verifies that the observed innovation lies in the forecast support;
- samples from the positive-semidefinite conditional covariance by eigen
  factorization; and
- does not add jitter or silently change the covariance scale.

This completion is used for exact fixed-W and frozen-discount-template modes
only when state draws are requested. The component-scale mode retains its
existing time-zero completion because `theta_0` enters the component innovation
energy and scale conditional. Adaptive conditional discounting remains
excluded.

For fixed-W and frozen templates, the new time-zero draws are ancillary
completion draws: they are retained for full-state estimands but are not fed
back into the root-specific FFBS update. They do advance the RNG stream, so the
fit schema was bumped and exact continuation was retested rather than claiming
identity with version 1.8.0 output.

## Validation-contract correction

The duplicated execution extractor was removed. One shared helper now:

1. constructs training lower, upper, midpoint, and width draws;
2. computes observed-data RQR loss;
3. requires terminal and time-zero root-state functionals;
4. computes deterministic future conditional-mean root functionals;
5. adds learned `log(lambda)` only in learned-scale mode;
6. adds component-scale and innovation-energy estimands when applicable; and
7. requires exact ordered equality with the independently constructed fixture
   schema.

The reference suite now calls that same extractor in every one of the six
fixture/learning-rate cells. Its `6` versus `2+2+2` continuation checks require:

```text
complete finite time-zero draws
complete exact estimand schema
all saved stochastic fields bitwise equal
final checkpoints bitwise equal
three continuation-history segments with generations 0, 1, and 2
```

All six cells passed every condition.

## Exact corrected runtime

The corrected application subtree was archived, built, installed, and attested
in a new disjoint ignored runtime:

```text
source commit:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

application tree:
  5b50eb8fcb5e4748fbdc40662c81b0657edfad38

source archive SHA-256:
  2d2e50c8895360a00b9e165080608723bfc14fb9ef6215607f0d624b01ca2d9e

source package SHA-256:
  9d42c933663bf6b584130e5ace4a0561bddf4d3ac62326bb7ff8524cd18761fa

runtime tree digest:
  09ee9a6774f24aa35cf0e196e44d4cd36cb9c11294c827b80d7f6f0dace05363

runtime attestation SHA-256:
  bb2a82cc744b7f078cfb0061f9b5d662061838d33595ac1d81134e265593e435

runtime toolchain digest:
  13e4a079e76d87a6542fdc6e8718899258f541f20879677d51bb770a17f6cee9
```

Every version-5 runtime-lineage gate passed.

## Corrected validation evidence

### Preflight

```text
status:                    passed
prospective fits:          24
executed fits:              0
artifact manifest SHA-256:
  a9444ea66c4fb0c57c25eb8600df2c421a64c4e00d3cbd239034ee3742011dca
```

### Reference-only

```text
reference gates:           43 / 43
failure records:            0
all six time-zero contracts: true
all six estimand schemas:    true
all six continuations:       true
all six final checkpoints:   true
peak sampled RSS:          189,240 KiB

reference-gate SHA-256:
  0de3ccced9f813fd62244d601fc2561593f5fe47dc2c3cc6b1c86fb623527f05

reference-bundle SHA-256:
  e67279038869b91970b973af71e51eb885592d52bba10e02474065318964bd04

artifact-manifest SHA-256:
  04c77bb704e43edc1c58355778d7a198b76c3c5a3cea98e1c07ee7892c4624f5
```

### Full representative four-chain benchmark

The corrected benchmark used the shared-component-scale trend-plus-regression
fixture in normalized learned-scale mode, with the frozen 2,000 burn-in and
6,000 retained draws per chain:

```text
completed chains:             4 / 4
diagnostics:                150 / 150
time-zero estimands:          6
maximum R-hat:                1.004908
minimum bulk ESS:          1,411.395
minimum tail ESS:          2,041.284
numerical repairs:             0
forecast repairs:              0
failure rows:                  0
peak sampled RSS:          394,508 KiB
local full-chain bytes:  45,307,288, ignored

diagnostic-table SHA-256:
  35367129aeb291429fda0cb077510f0beabbf7fd4e4ee0e89685d8a9d3333369

artifact-manifest SHA-256:
  ac7652c86ccf54e8a0babcf27f0b40600d0650d15791d26c6061a36b7ba77ddf
```

### Fail-closed and monitor checks

The exact corrected runtime and reference bundle passed the no-confirmation
execute check:

```text
reference binding verified:  true
status:                      blocked_by_execution_contract
published chain RDS files:   0
```

All eight monitor/finalization fault scenarios passed. Final process groups
were empty. Sampled RSS remains telemetry rather than a kernel-hard peak.

### Package and repository checks

```text
native R/C++ suite:          passed
R CMD check --no-manual:     Status OK
make smoke:                  passed
main PDF:                    9 pages
supplement PDF:             10 pages
literature manifest:        18 local PDFs
archive-only exdqlm smoke:   passed
```

PDFs, TeX build products, isolated runtimes, and full fitted chains remain
ignored and untracked.

## Scientific interpretation

The correction does not change the generalized-Bayes target:

- the RQR posterior remains a loss update;
- the pseudo-AL representation still augments the pseudo-residual loss and is
  not an ordinary response likelihood;
- the root/state draws are not posterior-predictive response draws;
- exact fixed-joint modes still use alternating root-specific FFBS
  full-conditionals;
- same-data learned `lambda` is not automatic empirical coverage calibration;
  and
- this bounded work does not establish comparative forecasting performance.

## Go/no-go and next action

The failure mechanism is corrected and the exact corrected evidence is strong
enough for independent source review. It is not appropriate to silently retry
the grid. The committed execution flag is false, and the prior authorization
commit cannot authorize changed source.

The next action is an independent Output-13 audit of:

1. the failed-launch evidence and zero-publication claim;
2. the conditional time-zero mathematics and singular-covariance boundary;
3. the storage-dependent RNG and continuation semantics;
4. the shared reference/execution schema contract;
5. the exact corrected runtime and validation hashes; and
6. whether a new, separate false-to-true authorization commit may be created.

Only after that review may the repository create a new authorization commit,
rebuild all exact-source evidence again, satisfy the explicit confirmation
gate, and retry the six four-chain cells. Matched simulation, CAVI/ELBO, and
RQR-DESN remain deferred.
