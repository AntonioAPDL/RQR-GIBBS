# Unscaled MTI Gibbs Comparison

## Purpose

This separate experiment asks whether fixed root priors, Dirichlet-process
(DP) assumptions, and a design-specific candidate probability threshold can
improve unscaled posterior-mean intervals. It preserves the completed
radius-calibration study and the current manuscript and application results.

The configuration is `application/config/mti_gibbs_joint_20261117.json`.
The statistical functions are in `application/scripts/lib/mti_gibbs_joint.R`;
study execution and reporting are in `mti_gibbs_joint_study.R`.
Driver 133 and launcher 134 provide reproduction and detached scheduling.
Local audit notes remain under the ignored `.codex_work/` tree.

## Statistical Definition

In independently fixed response coordinates, the two raw roots have
independent Normal(0, sigma^2) priors, with sigma in {2,1}. The generalized
posterior has density proportional to the prior times the exponential of
minus the MTI loss, with learning rate one. It is not a response likelihood.
The prior location and scale do not use the analyzed observations.

For each sigma, compute and numerically check the existing finite ECM q/tilt
candidate set under that actual prior. A candidate fitted with sigma=2
cannot be reused as a sigma=1 mode. Candidate construction, initial values,
and numerical refinement follow the existing fixed-prior implementation.

For DP concentration a in {0.5,1,2} and independently specified base
Normal(0,h^2), h in {2,4,8}, compute the posterior probability that a closed
candidate interval contains at least c population probability. If N is its
observation count and H its base probability, that probability is

    Pr{Beta(N + a H, n - N + a(1-H)) >= c}.

All eighteen prior/DP combinations are evaluated by analytic DP calculations
and prior-specific ECM fits. They do not imply eighteen Gibbs chains per
sample. Gamma is the eligibility threshold applied to these mode intervals.
Its values come from exact probability selection events throughout (0,1),
on both sides of the reference gamma; there is no guessed attainment floor.
Equality at gamma is eligible. Constant-selection regions are (lo,hi]; an
interior threshold represents each region, retaining the reference threshold
when it lies in that region. Numerically indistinguishable objective ties
use twelve significant digits, then reference DP assumptions, wider threshold
regions, model identity, and descending gamma.

Among eligible candidates, the fixed ordering is width, q, absolute tilt
displacement from the central tilt, lower endpoint, then candidate index.
Use the original tilt at n=50,c=.90, and the existing square-root interior
tilt projection in other designs. Hold the selected q and projected tilt
fixed during four-chain marginal slice-within-Gibbs root sampling. This
samples the same loss-based target as the corresponding augmentation;
it does not jointly sample the DP or the data-selected q/tilt.
The reported interval consists of the means of the ordered root draws.
No radius multiplier is applied to the proposed method.

The final interval's DP probability is recomputed and reported separately.
An ECM eligibility check is not asserted to hold for different Gibbs mean
endpoints. Only repeated-sample assessment evaluates frequentist attainment.
Proper priors and DP probability checks alone do not establish a
distribution-free tolerance theorem for this procedure.

## Data and Bounded Development

The nine designs are (n,c) = (50,.90), (100,.90), (100,.95),
(500,.90), (500,.95), (500,.99), (1000,.90), (1000,.95), (1000,.99),
all with requested attainment p=.95. Keep the eight existing distributions
and their frozen generators: Normal, Student t3, exponential, asymmetric
Laplace, two-piece Normal, beta, gamma, and lognormal. This gives 72 cases.
The small-sample limitation at extreme content is retained; no unavailable
finite-sample guarantee is asserted by changing the prior.

Reuse 100 existing exploratory datasets per case. The first twelve and
remaining 88 are identified separately. Neither subset is fresh validation.
Known distribution functions are used only in development/evaluation,
never in the new-sample fitting functions. Priors selected on a simulated
panel are empirically chosen procedures with fixed priors for each future
dataset, not elicited scientific priors for a new application.

For each root SD and design, nominate at most two complete DP/gamma rules:
the narrowest worst-law mean ECM width ratio among rules with attainment
at least p in every law, and the corresponding unrestricted rule. Include
the unscaled R0 reference, giving at most five initial rules per design.
Mean widths are normalized within law to the same R0 ECM reference; raw
widths are never pooled across distributions. Minimize worst-law ratio,
then mean log ratio, with the deterministic tie rule above.

Sample only their selected Gibbs targets. Identical observations, root
prior, q, actual tilt, learning rate, numerical policy, and sampler sources
identify reusable targets. DP parameters do not enter that fixed root
posterior. Saved reference fits are reused only after source, sampling,
observation, target, receipt, and numerical-accuracy checks.

For an alternative to advance, require:

- Complete numerical calculations and no nonreturns.
- Observed actual Gibbs attainment at least p in each of the eight laws,
  both across all 100 datasets and across replications 13--100.
- Worst-law mean Gibbs width ratio at most .99 relative to the separately
  frozen, calibrated R0 interval.
- Negative upper 95% paired mean-log width-change bounds in every law,
  and width improvement in every law among replications 13--100.

The 1% width resolution follows the existing numerical policy; it is not
an attainment threshold. Paired intervals here are developmental evidence,
not post-selection confidence statements. Apparent precision from this
selection stage is not substituted for independent validation.

Where no initial alternative advances, allow one refinement of at most
two further rules from the same full ECM comparison: the next attainable
and next unrestricted choices. Reassess all 100 datasets and every law in
that design, with checked target reuse. No second refinement is automatic.
Select at most one new rule per design. A nonadvancing design has no selected
alternative and no row named `gibbs_new_unscaled`. R0 remains a named reference.

Assessment revision 2 uses the predecessor's `proposed` radius policy, checked
against both frozen selection and radius hashes. If its multiplier in a design
is s, the developmental Gibbs width ratio is divided by s, and its paired
log-width bound shifts by -log(s). Proposed endpoints and posterior draws are
unchanged. ECM nomination remains explicitly an ECM proxy: the constant
rescaling cannot change its within-design ordering. Raw R0 is retained as a
diagnostic, rather than used as the primary width threshold.

Multiple Gibbs candidates per rule remain a separately reviewed question.
The experiment records the checked ECM candidate count and review status;
it does not automatically start a much larger Gibbs profile or claim a
global minimum posterior-mean width.

## Independent Assessment

If no design advances, stop and retain the negative developmental result.
If at least one advances, freeze the complete design-specific procedure
and then evaluate 1000 fresh datasets per law in each advancing design only.
The exact task subset is frozen and hashed before its observations are generated.
There are 8,000 jobs per advancing design, at most 72,000 overall. No further calibration
panel is needed because the final observations do not change the rule.

Compare the new unscaled rule, R0 unscaled, R0 with its separately frozen
radius, the existing primary ECM rule, Young--Mathew, and Wilks. R0 is the
predecessor's already frozen *proposed target rule*, not whichever reference
looks best after validation. Its unscaled and scaled versions share draws.
The primary ECM specification remains unchanged; matched-prior mode
endpoints are separate diagnostics. The selected ECM endpoints precede
tilt projection and need not be modes at the Gibbs target's projected tilt.
At most two root targets per fresh
dataset are required before reuse/continuation, or 144,000 across the panel.

Report every law/design separately: return and attainment counts, exact
binomial intervals, attainment Monte Carlo SE, raw mean width, 2.5% and
97.5% width quantiles, true content, selected q/tilt, final DP probabilities,
and chain diagnostics. Width quantiles describe repeated-dataset variation,
not uncertainty about a mean width. Include paired width uncertainty and
attainment disagreements. Nonreturns are attainment failures; widths are
conditional on returns and show their sample count. No observation set is
replaced because a method fails. Numerical failures pause execution with
their saved diagnostics for review.

A separate one-sided simultaneous binomial lower bound protects 72
statements for the proposed method. It is not an automatic selection or
publication condition; failure to clear that stronger bound is not itself
proof of underattainment. The nominal attainment assessment and Monte Carlo
uncertainty remain primary. No general guarantee or application transfer
follows from success on the finite distribution panel.

## Reproduction and Scheduling

Use the pinned installed library, read-only:

```bash
export MTI_PRIOR_R_LIBRARY=/data/muscat_data/jaguir26/RQR-GIBBS/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library
make test-mti-gibbs-joint
MTI_JOINT_OUTPUT_ROOT=/var/tmp/jaguir26-mti-studies/joint_rehearsal make prepare-mti-gibbs-joint
MTI_JOINT_OUTPUT_ROOT=/var/tmp/jaguir26-mti-studies/joint_rehearsal MTI_JOINT_WORKERS=1 make check-mti-gibbs-joint
# After committing the source:
make launch-mti-gibbs-joint
make health-mti-gibbs-joint
```

For the controlled mode-only transition from source `99d8a31`, set
`MTI_JOINT_REUSE_ROOT` to its stopped output directory when scheduling the
new committed source. The old study must have no saved Gibbs-development fits.
The import checks package, R/RNG, generators, seed allocations, dependency,
all unchanged supporting programs, mode-fitting function expressions, task
receipts, sample hashes, actual priors, and candidate accuracy. It copies only
complete successful fits, retaining their exact bytes and original receipt
hashes in new origin records. Corruption or changed computation stops reuse.
Interrupted writes remain in the old directory and are recomputed only when
they lack a complete receipt. The source directory is never modified.
This is a specific interrupted-stage transition, not permission to relabel
old posterior calculations under a changed model.

### Precision Recovery After Development Began

Source `f0f1b7017983813620e683bd9354f2a6866c53f3` stopped after
1,320 developmental datasets: 1,319 were numerically adequate and one
contained a posterior-mean precision failure. All 7,200 mode calculations
were complete. The four-chain target requiring more precision was the
lognormal sample at n=50, c=.90, replication 19, job 1304. Its preferred
mixing checks passed; its width mean had MCSE .196 against the existing
precision limit .138. This is distinct from statistical underattainment.

The separate `mti_gibbs_joint_precision.R` retains the original sampler and
posterior target identifiers. After the original 1000--8000 schedule, only
targets with all preferred mixing checks satisfied and unresolved mean
precision can receive more computation: 32,000 retained draws per chain,
then at most 64,000 if mean precision alone still fails. The same priors,
observations, q, tilt, learning rate, starts, warmup and chain seeds are used.
Every saved warmup, retained root draw and objective prefix must reproduce
exactly. A deteriorating mixing check stops further automatic extension.
Adequate calculations are never extended, and the precision thresholds are
unchanged. Extra draws estimate the same posterior; they do not reduce its
intrinsic uncertainty. Actual budgets and original-record hashes are saved
with every extended target.

For this specific stopped source, schedule the committed recovery using:

```bash
MTI_JOINT_REUSE_STAGE=development \
MTI_JOINT_REUSE_ROOT=/var/tmp/jaguir26-mti-studies/mti_gibbs_joint_f0f1b7017983813620e683bd9354f2a6866c53f3 \
make launch-mti-gibbs-joint
```

The configured source, saved counts and failed job must match the reviewed
stop. Supporting programs, computation expressions, scientific settings,
observations and posterior target keys are checked. The analytical DP
comparison is deterministically replayed from the exact saved mode objects;
its full-precision selection must reproduce the original developmental
analysis hash, and its three saved comparison files must be byte-identical.
This replay avoids using rounded CSV or JSON columns as numerical inputs.
It makes no new nominations. Mode objects and successful development fits
are copied byte for byte with their original receipt hashes. The original
failed object and receipt are also archived unchanged. Only its inadequate
target is extended; all other targets and ECM endpoints are preserved.
New receipts record the original source and the distinct recovery source.

Restarting the recovery verifies imported fits and skips completed numerical
extensions. It never deletes or overwrites the stopped source. The remaining
5,880 initial datasets, bounded refinement and conditional fresh assessment
follow the original design. No model menu, prior, threshold-selection rule,
assurance budget, or manuscript result changes as part of this recovery.

`MTI_JOINT_OUTPUT_ROOT` is an optional local rehearsal directory; production
uses the launcher's fixed output link. The launcher freezes a clean committed checkout and the predecessor's
target-selection hash. It polls a durable completion record every five
minutes, using negligible resources while waiting. A vanished process is
not completion. A nonzero exit, missing successful summary, changed frozen
rule, or failed evidence check prevents the new study from starting.

After predecessor verification, use at most forty workers, limited by
available cores, with one numerical thread per worker. Rehearsal first
checks both root priors on two real developmental datasets, including
n=50,c=.90 and n=500,c=.99. The existing Rhat, ESS, and posterior-mean Monte
Carlo precision policy is unchanged. Retention budgets are 1000, 2000,
4000, and 8000 per chain, with 500 burn-in draws. Within-budget retries
use deterministic streams; unresolved calculations are preserved for review.

Heavy outputs go under `/var/tmp/jaguir26-mti-studies/`, outside the
space-constrained `/data` volume. Preserve a 32 GiB disk reserve and check
projected storage after every batch. `/var/tmp` is local scratch storage,
not a durable archive; export the compact evidence and required fitted
objects to durable storage before any administrative cleanup.

All task tables, source/program hashes, independent random-seed allocations,
sample hashes, installed-library hashes, and atomic per-result receipts
are retained. The exact repository commit and pinned exdqlm reference
`dffb71ee70b597d6a716ee74be1cbc99731cd453` are recorded. Nothing is built,
installed, or loaded from an external source checkout. Existing studies
and their receipts are read-only inputs.

The latest pointer is `.codex_work/mti_gibbs_joint_latest.json` in the common
repository root. It identifies the frozen checkout and actual output root.
To resume, run launcher 134 from that same frozen clean checkout; it checks
existing receipts rather than restarting completed fits. Inspect any saved
failure before deciding on a distinct revision. Never erase a failure and
resume silently under a changed statistical rule.

The compact evidence receipt verifies summaries without needing the fit
directory. `verify` accepts an explicit output directory:

```bash
Rscript application/scripts/133_assess_mti_gibbs_joint.R verify /path/to/output
```

No manuscript or result promotion is automatic. Review all cases, negative
findings, and independent uncertainty before any scientific update.

## Background

The distinction between loss-based updating and response likelihoods follows
[Bissiri, Holmes, and Walker (2016)](https://arxiv.org/abs/1306.6430).
Separating development from independent assessment and reporting Monte Carlo
uncertainty follows the simulation-design principles of
[Morris, White, and Crowther (2019)](https://doi.org/10.1002/sim.8086).
