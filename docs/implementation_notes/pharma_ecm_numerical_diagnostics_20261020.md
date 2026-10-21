# Pharmaceutical ECM Numerical Diagnostics

## Purpose and Preserved Results

This study distinguishes iteration stability, local stationarity, and global
minimization of a fixed-target MTI loss. It does not recalibrate MTI-ECM or
replace any pharmaceutical or simulation result in the article.

The starting publication commit was
`ba51f58ddcc7eee006ca9e0e7ecdcededb90fae3`. Calculation sources were tested
and committed at `9cbca32` before the full diagnostic run. The study used a
separate worktree on `codex/pharma-ecm-numerical-audit-20261020`. The ignored
planning tracker is `.codex_work/pharma_ecm_numerical_audit_plan_20261020.md`.
No MTI jobs were active at the initial process check. Other projects' active
processes were neither stopped nor modified.

Before/after SHA256 checks cover the published manuscripts, canonical
application files, pharmaceutical tables and figures, configuration, frozen
policy, validation configuration, and internal calibration reference. Every
protected input was unchanged. The new software is a diagnostic script
library, not a replacement package fitting API. Historical convergence flags
are preserved.

## Study Design

The versioned configuration is
`application/config/pharma_ecm_numerical_audit_20261020.json`.

1. Recompute objective and nonsmooth stationarity diagnostics for all 3004
   stored MTI intervals using their original training observations.
2. Choose at most two smallest split IDs per response, partition scheme, and
   original convergence-flag stratum. Both single chronological cases are
   retained. This gives 28 cases: 14 per response, covering random,
   chronological-forward, chronological-reverse, month-grouped, and API-grouped
   partitions. No selection uses interval width or held-out performance.
3. Reproduce each case's complete frozen candidate menu and original selected
   interval. Selected endpoints, fitted content, tilt, probability, candidate
   count, iteration count, and flag must match the canonical evidence.
4. Repeat the selected fixed-content, fixed-tilt fit with iteration caps 100,
   500, and 2000. All other controls, initialization, prior, and learning rate
   remain fixed. The longer traces must have the same initial path.
5. Compute an independent fixed-target objective minimum for each of the 2464
   original menu candidates. Compare objective values only at identical
   fitted content and tilt. Reapply the unchanged DP screen and selection
   ordering to obtain a diagnostic reference-menu comparison.

The longer ECM runs condition on the originally selected q and tilt. They
do not refit the complete menu at 500 or 2000 iterations, so their stability
alone does not establish that the full selection would be unchanged at those
budgets. The separate full-menu reference comparison addresses the possible
effect of different fixed-target solutions on candidate selection.

The comparisons use configurable numerical tolerances: relative objective
gap 1e-8 and endpoint change 1e-5 training standard deviations. These are
diagnostic comparison thresholds, not tolerance-confidence parameters or
calibration targets. Continuous differences are retained in the output.

The full run completed 28 cases and 84 selected fixed-target fits in 27.546
seconds using eight single-threaded workers. There are 39123 saved iteration
rows. Full initial menu replays also supplied 2464 baseline candidate fits;
the independent references are analytical cell searches, not extra ECM fits.
The frozen installed rqrgibbs 0.1.0.9036 runtime was copied into an isolated
ignored library. Its shared-library SHA256 agrees with the original
application runtime. R was 4.5.3. No exdqlm namespace was loaded, installed,
or compiled from an external source checkout; its configured provenance pin
is retained in the manifest.

## Independent Objective Reference

For an intercept model, let l and u be its endpoints, q its fixed fitted
content, delta its fixed mean tilt, omega its positive learning rate, and
lambda the common positive Gaussian prior precision. The exact penalized
loss used in the package is

```text
F(l,u) = omega * sum_i {rho_q((y_i-l)(y_i-u))
                       - q*delta*(l+u-2*y_i)}
         + lambda*(l^2+u^2)/2.
```

This is a loss-based generalized-posterior mode criterion, not a response
likelihood. The independent implementation is checked directly against the
installed package objective, including its linear tilt sign and prior scale.

Partition the endpoint plane using the distinct observed values and finite
exterior bounds. Within a cell, the observations strictly between the roots
are fixed. If K, S1, and S2 are their count, sum, and sum of squares, then

```text
F(l,u) = A - B*(l+u) + T*l*u + lambda*(l^2+u^2)/2,
A = omega * (q*sum(y^2) - S2 + 2*q*delta*sum(y)),
B = omega * (q*sum(y) - S1 + q*n*delta),
T = omega * (q*n - K).
```

The polynomial remains valid on cell boundaries because observations at an
endpoint have zero residual product. Each fixed-endpoint edge is strictly
convex in the other endpoint, so its minimum is a clipped scalar quadratic
minimum. The Hessian is `[[lambda,T],[T,lambda]]`. A nonsingular interior
stationary point has l=u. A positive-semidefinite singular stationary set
extends to an edge, and an indefinite interior stationary point is not an
interior minimum. Four edge minima and the diagonal stationary point therefore
cover the cell minimum. For a same-gap cell, symmetry supplies reflected
ordered candidates; collapsed endpoints are explicitly included. No arbitrary
restriction to the observed sample range is imposed.

The Gaussian penalty yields a finite sublevel bound. Let
`a=omega*q*n*delta`, and let F0 be the objective at an existing finite interval.
Since the check loss is nonnegative,

```text
F(l,u) >= lambda/2 * {(l-a/lambda)^2+(u-a/lambda)^2}
           + 2*omega*q*delta*sum(y) - a^2/lambda.
```

Consequently every global minimizer lies in the resulting explicit compact
ball; the search uses its enclosing coordinate box. All intersecting cells,
including exterior cells, are enumerated. The calculation is exhaustive in
exact arithmetic. In floating-point arithmetic, candidate ranking is checked
by direct, unsimplified loss evaluation, including the numerical tie set.
Tests include tied and constant samples, singular Hessians, exterior roots,
dense grids, and multiple Nelder--Mead searches. The latter are independent
cross-checks, not substitutes for enumeration. This reference applies only
to a fixed-rate intercept model with equal proper Gaussian precisions; it
is not a regression solver, population oracle, or tolerance-validity theorem.

## Nonsmooth Diagnostics

At a zero residual product, the check-loss slope ranges from q-1 to q.
The derivative contribution at a distinct lower endpoint affects its lower
coordinate; the analogous upper-endpoint contribution affects the upper
coordinate. Summing their lower and upper contributions gives exact
coordinatewise one-sided gradient intervals. Their distance from zero is a
local stationarity diagnostic. It is not a global-optimality assertion for
this nonconvex objective. The tests explicitly include a stationary point
with a better objective elsewhere.

The original flag uses the midpoint of these intervals at an exact knot.
That particular gradient need not vanish even when zero belongs to the
interval. Conversely, a point just away from a knot has an ordinary gradient
that can remain appreciable despite tiny endpoint displacement and tiny
objective change. To measure this, the study separately reports the nearest
observed-knot pair, its displacement in training-SD units, its objective
change, and its local stationarity distance. These are backward-error
diagnostics only. Snapped endpoints never replace saved results or silently
change closed-interval membership.

General background is available in the
[Stanford subgradient notes](https://web.stanford.edu/class/ee364b/lectures/subgradients_notes.pdf)
and the official [R optim documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html).
The cell enumeration above is derived specifically for this repository's
criterion; convex optimality statements are not transferred to the complete
MTI objective.

## Findings

| Diagnostic | Tensile Strength | Weight RSD |
| --- | ---: | ---: |
| Prespecified cases | 14 | 14 |
| Baseline relative objective gap above 1e-8 | 12 | 14 |
| Gap persists at 2000 iterations | 12 | 14 |
| Endpoint movement above 1e-5 training SD | 1 | 0 |
| Posterior screen pass/fail changes | 0 | 0 |
| Median reference-menu width / original width | approximately 1.000 | 1.253 |

Relative gaps are `(F_ECM-F_reference)/(1+abs(F_reference))`. Their medians at
the original budget are 5.34e-6 for tensile strength and 2.78e-3 for RSD.
The largest endpoint change over all cases at 2000 iterations is 2.15e-5
training SD. Thus extending the iteration budget alone does not close the
objective gaps, although the selected intervals are highly stable.

For some fixed targets, the lower-loss reference interval extends far beyond
the sample range. After reapplying the original menu and probability screen,
the RSD reference intervals are 1.9%--48.8% wider in these cases, with median
increase about 25.3%. Tensile reference-menu widths range from essentially
unchanged to about 6.5% wider. Selecting a different candidate index can reflect
an extremely small width difference; the continuous width and probability
comparisons must accompany that indicator.

These results do not establish that the original tolerance validation is
incorrect, and do not justify substituting the global reference as a superior
procedure. They show that the current finite-iteration profile is not
interchangeable with exhaustive minimization of every fixed-target loss.
Endpoint stability, local stationarity, global fixed-target minimization,
and shortest feasible outer-menu selection answer different questions.

## Decision and Next Step

Do not relax the convergence flag to make its count look better. Do not rerun
all validation samples at 2000 iterations: the targeted evidence gives no
reason to expect that alone to resolve the discrepancy. Keep the current
article evidence and calibration unchanged.

The next substantive question is mathematical and computational: which
fixed-target solution is intended when local solutions, near-kink solutions,
and a global penalized mode differ? Review the role of the empirical tilt
range, nonconvexity, weak Gaussian regularization, and residual flooring.
Any change to the estimator, solver, or candidate selection requires a
separate versioned proposal and independent synthetic validation before
replacement of the published rule. No claimed tolerance guarantee may be
inferred from the DP screen alone.

A focused independent-review prompt is prepared locally at
`.codex_work/pro_ecm_numerical_review_prompt_20261020.md`. It requests review
of the exact criterion and solution concept, rather than a broad search for
new datasets, methods, or favorable numerical settings.

## Evidence and Reproduction

Portable evidence is under
`figures/data/pharma_ecm_numerical_audit_20261020/`. It contains all saved-case
diagnostics, the selected-case list, all baseline/reference candidates,
budget summaries, compressed iteration traces, generated decisions/findings,
the frozen diagnostic configuration, session record, and hash manifests.
The 29-page PDF is generated under ignored application outputs, with one
overview page and one six-panel page per case. Fitted content and tilt are
fixed within each ECM run and are shown in headings, not fictitious traces.

```bash
# Prepare a local package library from this repository if necessary.
make pharma-mti-application-runtime
make pharma-ecm-audit-preflight
make pharma-ecm-audit-smoke
make pharma-ecm-audit-run PHARMA_ECM_AUDIT_WORKERS=8

# Reuse portable evidence without any new fitting.
make pharma-ecm-audit-report
make test-pharma-ecm-audit
```

Every new run requires a fresh output directory. Use
`PHARMA_ECM_AUDIT_RUN_DIR=<directory>` to specify it or to report/test an
existing diagnostic run. Completed evidence is exported explicitly with
`application/scripts/91_export_pharma_ecm_audit_evidence.R --run-dir=...`;
the destination must not exist. The canonical application is never exported
over or modified by this command.

Verification includes strict new unit tests, full artifact validation,
independent reference objectives and stationarity, exact trace prefix
agreement, every saved iterate's direct-DP probability, deterministic case
selection, PDF generation, and before/after publication-input hashes.
Existing C++/R ECM equivalence, objective, profile-trace, application, and
manuscript-language checks are also run. Resolved development checks caught
a runner parse error, a missing test-helper import, and CSV's untyped all-NA
column inference; none changed statistical settings or publication inputs.

| Check | Exit | Result |
| --- | ---: | --- |
| `make pharma-ecm-audit-preflight` | 0 | Frozen inputs and 28-case selection verified |
| Smoke diagnostic run, two workers | 0 | Both responses reproduced; six fixed-target budget fits |
| Full diagnostic run, eight workers | 0 | 28 cases, 84 budget fits, 2464 candidate references |
| `make test-pharma-ecm-audit` | 0 | Unit, mathematics, artifact, trace, probability, and PDF checks |
| `make pharma-ecm-audit-report` | 0 | Report regenerated from portable evidence without fitting |
| Existing ECM C++/R equivalence tests | 0 | Original backend behavior preserved |
| Existing ECM objective and monotonicity tests | 0 | Original criterion and safeguards preserved |
| Existing MTI profile-trace tests | 0 | Original tracing behavior preserved |
| Existing pharmaceutical application, table, and figure tests | 0 | Published report remains reproducible |
| Manuscript-language tests | 0 | Existing statistical-language checks preserved |
| Before/after publication-input SHA256 checks | 0 | Every protected file unchanged |

The cached raw-data unit check was also rerun after copying the same verified
public CSV into the new worktree's ignored cache. The package shared-library
hash was compared directly with the original application manifest and matched.
No new article PDF build was needed because all TeX inputs and publication
figures/tables are byte-identical to the previously compiled revision.
