# Independent reconciliation of ChatGPT Pro Output 6

Date: 2026-07-22
Audited implementation commit: `c866c7b901678f36fc8cce51d5eba673584522b5`
Package: `rqrgibbs 0.1.0.9006`
Fit schema: `rqrgibbs_fit/1.4.0`

## Executive verdict

Output 6 was mathematically careful and its new quadrature values were
reproducible evidence. It audited the older commit
`5ba8f76811222087826e99c37f5ac70cfab3fae5`, however, whereas the starting
commit for this reconciliation was
`3ad7d25af7292f805fbb18354e90424f20039d89`. The intervening commit had already
removed direct-source exdqlm eligibility and independently verified the Git
archive and installed runtime tree. That part of Output 6 was therefore
substantially superseded.

Four residual findings remained genuine:

1. the primary `rqrgibbs` namespace was not source-bound;
2. cumulative continuation eligibility was stored in mutable, undigested
   fields;
3. public FFBS and repair-ledger boundaries remained incomplete; and
4. the bounded pilot did not gate either sampler against the independent CDF
   references and contained unenforced resource declarations.

All four are corrected and tested. The fixed learned-scale bounded pilot
passed again under the stronger gates. This is a narrow computational-target
check. It is not evidence of empirical coverage calibration, a response
variance interpretation for lambda, response-predictive validity, or
production readiness.

The manuscript and supplement did not require a mathematical correction in
this pass. Their statements about the quartic stacked observation kernel,
sequential exact root-specific FFBS updates for fixed-joint modes, and the
working status of adaptive discount remain correct.

## Input integrity and protected repositories

The supplied local files matched the reported SHA-256 values:

| File | SHA-256 |
|---|---|
| `chatgpt_pro_output6_audit_20260722.md` | `413148d9da9cd15ba9714edbe350762e915874eee27cfeb9c9b280e3ac585b9a` |
| `output6_independent_quadrature_check.csv` | `f08f0f8fa9ae8f81fd06a29ffdd8f0d6f595d1b64e6b9dba64dbac6594ac0b17` |

The raw files remain local and ignored. The independently reported means and
CDF probabilities are tracked as a compact machine-readable validation
contract in
`application/inst/extdata/output6_independent_references.csv`, with the audit
hash recorded on every row.

The protected checkouts were read only throughout:

| Repository | Branch | Commit | State |
|---|---|---|---|
| exdqlm pinned RQR source | `feature/rqr-desn-readout-20260716` | `dffb71ee70b597d6a716ee74be1cbc99731cd453` | clean |
| Q-DESN article reference | `main` | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` | clean |

The full exdqlm checkout guard remained
`0b24b9b9136935d47510fc4fa10c324514918f69a44d6d54bb9f126cf22f332c`
before and after isolated runtime preparation and reference testing.

## Finding-by-finding disposition

| Finding | Output-6 verdict | Current disposition |
|---|---|---|
| N1: complete model/target/evolution digests | Pass | Confirmed |
| N2: checkpoint and RNG integrity | Pass | Confirmed |
| N3: cumulative continuation semantics | Partial | Corrected with a separate versioned history digest |
| N4: exdqlm runtime/source binding | Partial | Superseded by `3ad7d25`; strengthened with R/platform verification |
| N5: strict provenance | New primary gap | Corrected for the executing `rqrgibbs` namespace |
| N6: fractional DESN horizon rejection | Pass | Confirmed; unchanged |
| N7: scale-relative covariance validation | Partial | Public boundaries and metadata corrected |
| D1: runtime package/source binding | Partial | Primary and exdqlm bindings now independently gated |
| D2: small-scale covariance handling | Partial | Overflow and subnormal cases corrected |
| D3: cumulative continuation/bitwise claims | Partial | Corrected and tamper-tested through multiple generations |

### Primary runtime binding

`.rqr_provenance()` now constructs a primary repository/runtime state in
addition to external dependency states. The source package is explicitly
`RQR-GIBBS/application`, so Git uses the `HEAD:application` tree rather than
the repository root tree. The fit records:

```text
primary_runtime_package_path
primary_source_commit
primary_source_tree_digest
primary_runtime_tree_digest
primary_runtime_source_match
```

Direct-source eligibility requires that the executing namespace path equal the
primary package source path, the package/source versions agree, and the
repository is clean at the separately supplied expected commit. The actual
executing directory is hashed; it is no longer assigned a Git digest without
inspection. An isolated runtime attestation can alternatively be supplied.
Promotion requires `primary_runtime_source_match=TRUE`.

The bounded launcher no longer sets its expected commit equal to the current
HEAD. It requires `RQR_EXPECTED_PRIMARY_COMMIT` before it inspects HEAD. At the
audited commit it recorded:

```text
runtime path: /data/muscat_data/jaguir26/RQR-GIBBS/application
source path:  /data/muscat_data/jaguir26/RQR-GIBBS/application
direct path match: TRUE
runtime source match: TRUE
reproducibility eligible: TRUE
```

### exdqlm attestation

Output 6's generic-attestation reproducer applied to the previous schema. At
the starting commit for this pass, the verifier already:

- prohibited direct exdqlm source execution for promotion;
- recomputed the actual source-archive SHA-256;
- recomputed the actual installed runtime-directory SHA-256;
- required archive and runtime paths disjoint from the protected source; and
- verified that the protected checkout guard was unchanged.

This pass additionally requires the attested R version and platform to match
the executing process. The exact source-to-binary build relationship remains a
controlled local build-lineage assertion rather than a cryptographic
authenticity proof. That limitation is unavoidable without a signed,
reproducible-build infrastructure and is now described as such; it is not
treated as adversarial proof.

### Continuation-history integrity

Schema `rqrgibbs_fit/1.4.0` adds
`rqrgibbs_continuation_history/1.0.0`. Every DLM fit stores a separate
`continuation_history_contract` and SHA-256 digest. The contract contains:

- generation number and every segment record;
- current and cumulative repair status;
- complete-chain numerical exactness;
- promotion and reproducibility eligibility;
- requested and resolved backend history;
- every parent checkpoint digest; and
- a cumulative environment mismatch/override ledger.

Continuation verifies the history digest before reading redundant
`model_spec` or provenance fields, then cross-checks those fields against the
contract. A child obtains inherited status from the validated contract rather
than mutable `model_spec` entries. Tests reject changes to the cumulative
repair count, promotion flag, mismatch ledger, stored digest, checkpoint
digest, and backend metadata. Clean continuation is tested through two child
generations. Environment overrides remain durable and suppress
reproducibility, promotion, and bitwise claims.

### Numerical boundaries

The public FFBS dispatcher now validates before selecting R or C++:

- nonempty finite-or-`NA` pseudo-observations;
- finite initial mean with the correct dimension;
- finite positive observation variances;
- finite transition matrices;
- symmetric, dimension-compatible, positive-definite `C0`; and
- validated evolution covariance inputs.

Material `C0` asymmetry can no longer be silently symmetrized by either
backend. The frozen discount-template ledger now propagates the actual
`jitter_scale` and `absolute_jitter_fallback`.

R and C++ use

```text
0.5 A + 0.5 A'
```

rather than `0.5 (A+A')`, avoiding a gratuitous overflow for finite entries
near the floating-point maximum. If a positive relative jitter multiplied by
a nonzero subnormal matrix scale underflows to zero, both implementations stop
with an explicit rescaling/absolute-covariance error. The exactly-zero matrix
case retains its separately recorded absolute fallback.

Tests cover public `C0`, `m0`, and `GG` rejection in both backends,
near-maximum symmetrization, subnormal jitter underflow, actual-scale symmetry
and definiteness from `1e-300` through `1e300`, and R/C++ repair-ledger parity.

### Pilot corrections

The bounded launcher now:

- uses `estimated_relative_error`, not “relative error bound,” for
  `pracma::integral2()`;
- gates collapsed and fully augmented CDF estimates separately against all
  five independent Output-6 quadrature probabilities;
- cross-checks the custom rank-normalized diagnostics with maintained `coda`
  R-hat and ESS implementations;
- distinguishes primary reproducibility from promotion in its continuation
  table;
- requires the expected primary SHA from the environment;
- fixes common BLAS/OpenMP thread controls at one and records one worker; and
- reads `/proc/self/status` `VmHWM` and enforces the declared four-GiB
  resident-memory limit.

The `coda` cross-check is deliberately independent but not a replacement for
rank-normalized split R-hat and bulk/tail ESS. The latter remain the primary
modern gates; `coda` provides a maintained classical diagnostic sanity check.

## Stronger bounded-pilot result

Run:

```text
application/outputs/
  rqr_bounded_pilot_20260723T065017Z_c866c7b90167
```

The directory is ignored. All 15 artifact SHA-256 values were independently
recomputed and matched the manifest. The failure log is empty.

| Gate/result | Value |
|---|---:|
| Decision | PASS |
| Maximum rank-normalized split R-hat | 1.000377 |
| Minimum bulk ESS | 15,743.75 |
| Minimum tail ESS | 25,966.30 |
| Numerical repairs | 0 |
| Independent CDF gates | 10/10 sampler-reference comparisons pass |
| Maintained diagnostic rows | 14/14 pass |
| Primary runtime/source match | TRUE |
| exdqlm runtime/source match | TRUE |
| Workers | 1 |
| Peak resident memory | 0.562 GiB |
| Wall time | 8.59 minutes |
| Artifact size before manifest | 0.012 MB |

The five CDF comparisons were:

| Estimand/threshold | Independent | Collapsed | Fully augmented |
|---|---:|---:|---:|
| `lambda <= 1` | 0.347248 | 0.347375 | 0.346238 |
| lower root `<= -1.5` | 0.409810 | 0.410775 | 0.409113 |
| upper root `<= 2.5` | 0.558799 | 0.562300 | 0.563850 |
| width `<= 4` | 0.572676 | 0.569588 | 0.573050 |
| midpoint `<= 0.5` | 0.489019 | 0.491425 | 0.494300 |

Every collapsed-versus-augmented, collapsed-versus-independent, and
augmented-versus-independent CDF comparison passed its predeclared four-MCSE
gate.

## Next RQR-DLM stage

The exact next-stage configuration is now frozen at
`application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260722.R`. It
contains:

1. a fixed-`W` local-level fixture with missing observations and a future
   horizon;
2. a frozen trend--seasonal component-discount template using the
   exdqlm-compatible `df`/`dim.df` block contract; and
3. a shared component-scale trend--regression fixture with analytic
   inverse-Gamma component updates.

It fixes four chains, 2,000 burn-in iterations, 4,000 retained draws, exact
seeds, zero-repair numerics, rank-normalized R-hat/ESS gates, root-swap
activity, primary runtime binding, and two continuation generations.
`adaptive_discount` is excluded. The preflight passes at the audited clean
commit, but the multicomponent chains were not launched because their config
explicitly lacks production authorization and requires a separate execution
decision.

After those bounded fixtures pass, the remaining order is:

1. freeze the matched RQR-DLM simulation DGPs, windows, seeds, competitors,
   interval loss/coverage/width/oracle-root metrics, and response-simulation
   contracts;
2. run and audit the matched RQR-DLM study;
3. only then begin the separate CAVI/ELBO derivation and validate it against
   bounded MCMC references; and
4. defer RQR-DESN work until the RQR-DLM MCMC and matched study are stable.

## Validation matrix

The following completed successfully:

- all relevant R source and scripts parsed;
- `make test-native`;
- `make package-check` with `Status: OK`;
- `make smoke`;
- `make pdf`;
- `make supplement`;
- `make literature-manifest` for 18 local-only PDFs;
- `make test-exdqlm-rqr`;
- `RQR_EXPECTED_PRIMARY_COMMIT=<c866...> make preflight-dlm-bounded`; and
- the stronger bounded pilot at the audited commit.

No matched or production simulation was launched. No heavy model object,
generated PDF, TeX log, runtime library, manifest, or pilot output is tracked.
