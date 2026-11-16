# Fixed-Prior Precision Refinement and Reporting

This computation follows the completed fixed-prior study and its preserved
Gamma numerical repair. It retains independent Normal(0,2^2) raw-root priors,
learning rate one, the fixed DP base, and each sample's selected fitted
content and tilt. It neither estimates a prior from analysis observations
nor selects a new prior using validation or held-out outcomes.

## Numerical Refinement

Application fits are eligible only when ECM coordinate accuracy and Gibbs
mixing checks pass while the posterior-mean Monte Carlo error criterion
does not. The four chains are recomputed with their original seeds,
initialization, burn-in, observations, and posterior target. Retained budgets
are 8,000, then at most 16,000 per chain, stopping at the unchanged numerical
criterion. Every previously saved retained draw, burn-in draw, and objective
value must agree exactly with the corresponding prefix of the longer chain.

This refinement does not repeat interval selection. Candidate fits, q,
delta, prior, ECM endpoints, comparators, and original files remain unchanged.
Only Gibbs summaries and their Monte Carlo diagnostics are updated. Held-out
observations enter evaluation after sampling and prefix verification.
An accepted original fit cannot be replaced through this procedure. Other
numerical failures require separate review; no alternative interval or
replacement dataset is inserted.

New fits and receipts are saved separately. The application result index
records each original file and hash and any numerical refinement file and
hash, with both source commits. The combined application summary retains
all attempted observations. An unresolved numerical flag remains visible
and prevents the automatic result from being marked ready for review.

## Reporting Evidence

The adapter verifies the complete 72-setting, 1,000-replication simulation
design and four matched methods. It preserves raw 2.5th--97.5th width
percentiles separately for each generating distribution, sample size,
content, and method. The primary comparison remains MTI-ECM, Young--Mathew,
and Wilks. Gibbs posterior-mean results are retained as a separate assessment
of selected-target posterior uncertainty.

Application held-out content and tail omissions are recomputed using the
original observation identifiers. Their summaries remain descriptive because
partitions overlap. Numerical source identities and input hashes distinguish
the original simulations, isolated repair, application fits, and precision
refinement. Lightweight CSV evidence has portable basename-indexed hashes;
heavy posterior objects stay in ignored output directories.

The new evidence is staged under the local output directory. No article
number, figure, prior specification, or conclusion is changed automatically.
The previous diffuse-prior reporting configuration remains historical and
is not relabelled as this fixed-prior study.

## Commands

```bash
make test-mti-fixed-prior-finalization
make prepare-mti-fixed-prior-finalization
make check-mti-fixed-prior-finalization
make launch-mti-fixed-prior-finalization
```

The genuine-sample check lengthens the saved application job 6 and verifies
unchanged prefixes and original-file hashes. The launcher requires clean,
committed source, creates an exact-commit detached worktree, copies the
frozen continuation's installed packages, and records source and dependency
identities. It waits on the existing continuation's successful exit before
using its completed outputs. A lock prevents duplicate controllers.

Outputs are local under
`application/outputs/mti_fixed_prior_finalization_20261115/`: separate
precision fits, combined application results, source attribution, and staged
reporting CSVs. `make health-mti-fixed-prior-finalization` reports the current
phase from the frozen worktree. The existing external exdqlm reference is
recorded without compiling, installing, loading, or modifying its checkout.

## Scientific Interpretation

The Gaussian root prior gives a proper loss-based generalized posterior for
each fixed finite target and tilt. Fitted content and tilt remain selected
using training observations and held fixed for posterior sampling. This is
not a joint posterior over q and delta or a response likelihood. Ordered-root
summaries do not represent posterior predictive response draws.

The DP probability requirement used for ECM candidate selection does not
automatically apply to posterior-mean endpoints. Accurate Gibbs computation
therefore does not establish frequentist tolerance validity for that summary.
The complete validation and the outcome-stratified longer-chain diagnostic
cases must be interpreted before manuscript promotion. Subsequent prose,
captions, and notation follow `STYLE_PROFILE.md`.
# CSV Round-Trip Check

The first finalization stopped after all five precision extensions passed.
The merge guard compared binary fitted values with decimal CSV summaries at
zero tolerance. Differences were at the last printed digit (about 1e-16).
The summary guard now permits at most 32 machine epsilons times the value
scale. Exact RDS target hashes and saved sampling prefixes remain mandatory.
The corrected finalization imports the five verified saved extensions from
commit `9fd9ca40b5645342a8b15243f3bca14425ae4d04`; it does not resample them.

## Reporting

`125_report_mti_fixed_prior.R` provides `collect`, `render`, and `verify`
commands. Collection requires a successful completed finalization and all
simulation/application numerical checks. It preserves the original source
identities and checksums, copies only small summaries, and adds actual
posterior examples and representative application partitions. Rendering uses
the existing three-method primary figure layout and separate four-method
supplementary comparisons. Raw width percentiles remain within distribution
and design; Gibbs intervals are not passed off as eligible ECM candidates.
Original diffuse-prior reporting files remain available but are not inputs
to the new reporting command. Heavy posterior objects remain ignored.

## Posterior Example Ordering

The November 15 health check found that posterior display cases followed
task-storage order, putting Student t(3), n=500, c=.99 first. The trace and
rank captions instead named the intended Gaussian n=1000, c=.90 example.
Display selection now explicitly orders Gaussian, Student t(3), and
exponential cases. Trace and rank files retain sample identifiers and design
fields, checked against the example and caption before rendering. The
original fits, all performance summaries, and numerical diagnostic results
are unchanged. This correction requires no sampling or new calibration.
