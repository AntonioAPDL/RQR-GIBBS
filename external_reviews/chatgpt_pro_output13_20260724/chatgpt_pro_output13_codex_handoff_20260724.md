# Copy-paste-ready Codex handoff after ChatGPT Pro Output-13

Independently verify this handoff and the accompanying Output-13 audit against
the exact GitHub source. Modify only `AntonioAPDL/RQR-GIBBS`. Do not change
exdqlm or the Q-DESN article repository.

## Exact reviewed states

```text
Output-12 review:
  b10816bce5c06917fcd61832b7b2687803a067a0

first authorization:
  00d489d686b44622454333d225f5ce55e1f760a5

failed authorized launch source:
  0deebc753bdb29e541d5fcd34e39917b5d17774e

time-zero correction:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

authorization revocation:
  0d64331732fe4118e7234f6f23a851f5d98e6614

evidence:
  139ba53746c4a940612ef28449c78666dba08465

package:
  rqrgibbs 0.1.0.9012

fit schema:
  rqrgibbs_fit/1.9.0
```

Protected references remain read-only:

```text
exdqlm:
  dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

## Pro decision

```text
Decision: C

Correction:                              ACCEPTED
Failed-launch evidence:                  AUTHENTIC
Zero completed/published fits:           CONFIRMED
Time-zero Gaussian completion:           CORRECT
Singular supported-subspace branch:      CORRECT
Statistical target:                      UNCHANGED
Continuation and saved-state semantics:  PASS
Shared estimand boundary:                PASS
43 reference gates:                      PASS AS TRACKED
6 continuation/schema cells:             PASS AS TRACKED
150 benchmark diagnostics:               PASS AS TRACKED
Remaining bounded launch blocker:        NONE
Current authorization flag:              FALSE
```

This is a conditional go to create one new, separate authorization commit and
rebuild exact evidence before retrying the bounded validation. It is not
permission to reuse `00d489...` or `0deebc...`, and it is not permission to run
from a false-flag source.

## Accepted findings

1. At the failed launch source, the config genuinely contained:
   - three fixtures;
   - two learning-rate modes;
   - four chains;
   - 2,000 burn-in;
   - 6,000 retained;
   - thin 1;
   - C++ backend;
   - fail numerical policy; and
   - true bounded authorization.
2. The first row—fixed-W/fixed-rate/chain 1/seed 84201—failed on the
   117-estimand schema.
3. The schema check is before RDS publication.
4. The error path immediately stops the outer loop. The other 23 rows remained
   planned and no chain was published.
5. The correct local-level count is 117; the old fit/extractor supplied 115
   because both time-zero columns were conditionally omitted.
6. The implemented conditional is exactly:
   ```text
   theta0 | theta1 ~ N(
     m0 + C0 G1' R1+ (theta1-G1 m0),
     C0 - C0 G1' R1+ G1 C0
   )
   ```
7. The Cholesky triangular solves are correctly oriented.
8. The singular branch uses the positive eigenspace, checks support, rejects
   material indefiniteness, and introduces no jitter.
9. For fixed-W and frozen templates, the new draws are ancillary completion
   draws; they are not fed back into root FFBS.
10. Component-scale time-zero states keep their existing scale-conditional
    role; adaptive discount remains excluded.
11. The new draws change RNG consumption, so the schema bump and exact
    revalidation are necessary.
12. Both time-zero states are checkpointed, restored, and swapped with root
    labels.
13. The shared actual extractor is now used in reference and execution modes.
14. All six cells require complete time-zero arrays, exact schemas, bitwise
    saved fields, equal final checkpoints, and generations 0:2.
15. The corrected evidence reports:
    ```text
    reference: 43/43
    benchmark: 150/150
    max R-hat: 1.00490775707187
    min bulk ESS: 1411.39507093261
    min tail ESS: 2041.28415820644
    repairs: 0
    forecast repairs: 0
    failures: 0
    ```
16. The flag is false at the revocation and evidence commits.

## Rejected blocker hypotheses

No reachable counterexample was found for:

```text
publication before schema validation
continuation past the failed first chain
wrong Cholesky solve orientation
wrong Moore-Penrose conditional
acceptance of an out-of-support bounded state
time-zero completion changing the declared root target
reference/execute extractor drift after the correction
```

The bounded fixed-W and frozen-template fixtures have positive-definite
first-slice forecast and conditional covariances, so the singular fallback is
not on their execution path.

## Nonblocking hardening

These can be implemented separately but must not be used to retune the bounded
protocol:

```text
direct outside-support unit test for singular R1
Monte Carlo time-zero conditional-moment test
diagnostic record of time-zero completion strategy
storage-policy check before a general API bitwise-continuation claim
```

The storage observation is general API scope. The bounded config freezes
`store_state_draws=TRUE` and its six continuation cells pass bitwise.

## Next permitted implementation action

Create one new authorization commit whose only scientific-control change is:

```text
bounded_dynamic_execution_authorized:
  FALSE -> TRUE
```

Update its exact config test in the same commit. Do not cherry-pick or reuse the
old true-flag commit.

After that commit, before the 24 fits:

1. verify clean `main` at the full new SHA;
2. build and attest a fresh isolated primary runtime;
3. run the complete package/native/check/document/literature gates;
4. run bounded preflight;
5. run all 43 reference gates and recursive artifact verification;
6. run the eight monitor/finalization fault scenarios;
7. run the no-confirmation fail-closed execute test;
8. bind execution to the new SHA, exact runtime/toolchain, and fresh reference
   manifest;
9. obtain the exact confirmation:
   ```text
   I_CONFIRM_24_BOUNDED_RQR_DLM_FITS
   ```
10. run the six four-chain cells sequentially.

Mandatory stop policy:

```text
any chain error -> stop
any RDS publication error -> stop
any provenance/repair/future gate error -> stop
any four-chain diagnostic failure -> stop before later cells
```

Forbidden:

```text
retrying a failed chain
changing seeds
extending chains after seeing diagnostics
retuning priors or thresholds
weakening R-hat/ESS gates
dropping required estimands
discarding the first failed launch from the audit trail
```

## Scope after a successful bounded run

A successful 24-fit result would complete the bounded RQR-DLM MCMC mechanics,
mixing, provenance, and continuation gate only.

It would not establish:

```text
empirical interval calibration
comparative forecasting performance
production readiness
response-predictive validity
```

Therefore:

```text
matched/production simulation:  NO-GO
CAVI/ELBO:                      DEFER
RQR-DESN:                       DEFER
```

Return after the sequential bounded run with its exact authorization/source
SHA, fresh runtime/reference identities, complete compact artifacts, all local
chain hashes, cell-by-cell diagnostics, and the first failure if any.
