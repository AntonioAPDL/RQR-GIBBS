# Oracle-tilt V4 prelaunch hardening audit

## Scope

This audit reviews the version-4 prospective illustration-screen execution
boundary after the source implementation closeout. It does not alter the
frozen DGPs, target definitions, priors, MCMC schedules, candidate identities,
random-number streams, recovery thresholds, ranking rule, or interpretation.
No production candidate fit or manuscript promotion is reported here.

## Findings and corrections

### Complete wrapper-manifest verification

The monitored wrapper already recorded a complete SHA-256 inventory of every
closed-bundle file except the wrapper manifest itself. Downstream input binding
verified the compact runner manifest and inspected the wrapper status fields,
but did not independently rehash the wrapper inventory. Consequently, a
post-closeout mutation of a wrapper-owned log or telemetry file would be
visible in the recorded manifest but was not itself a rejection condition.

The correction adds `otv4_verify_wrapper_manifest()`. It requires a valid,
unique, relative-path manifest; an exact two-way match with the closed-bundle
file inventory; matching byte counts; and matching SHA-256 values. Benchmark
and execute input binding, independent selector replay, and compact evidence
packaging now call this verifier. Negative tests cover changed content and an
unrecorded extra file.

### V4-aware concurrent-process exclusion

The shared-host exclusion expression covered older numbered illustration
scripts but ended at script 51 and required an absolute `RQR-GIBBS/` path.
Actual `Rscript` processes expose relative arguments such as
`--file=application/scripts/52_run_oracle_tilt_publication_v4.R`. A second V4
run could therefore evade the intended prelaunch exclusion check.

The correction recognizes the complete numbered V4 range through script 57
and both relative and absolute `--file=.../application/scripts/` forms. An
empirical fault test started a live V4 reference process and confirmed that a
concurrent resource-rehearsal request stopped before creating its output root
with exit status 2 and the expected heavy-process diagnostic.

## Validation status

At the hardening boundary:

- focused V4 tests pass after adding the two negative manifest cases;
- the V4 R sources parse;
- the shell entry points pass `bash -n`;
- the empirical concurrent-process rejection passes;
- previously closed exact-runtime preflight and reference bundles pass the new
  complete wrapper-manifest verifier;
- a full `R CMD check --no-manual` passed before the correction and is repeated
  for the final hardening commit before exact-runtime evidence is regenerated.

The interrupted pre-hardening benchmark is obsolete by construction and is not
eligible evidence. The full benchmark and resource rehearsal must be rerun from
an isolated runtime built at the final hardening commit.

## Interpretation and decision

These corrections harden orchestration and evidence integrity only. They do not
change the generalized-posterior target or make a response-likelihood,
posterior-predictive, repeated-sample, calibration, or typical-performance
claim. Production remains disabled until the exact preflight, reference,
benchmark, and full-scale resource rehearsal pass and a later commit changes
only `execution_authorized` to `true`.
