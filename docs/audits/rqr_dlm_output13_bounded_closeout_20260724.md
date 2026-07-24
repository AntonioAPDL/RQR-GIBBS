# Bounded RQR-DLM launch closeout after Output 13

Date: 2026-07-24
Status: bounded validation passed; matched simulation remains unauthorized

## Decision and exact source

The independent Output-13 review accepted the time-zero state correction and
found no remaining blocker to a new exact-commit bounded launch. The review
packet is in `external_reviews/chatgpt_pro_output13_20260724/`.

The one-time authorization source was:

```text
authorization and launch commit:
  afc9c5fed14c66317b684fc9b9f6d01079c307cd

primary application tree:
  77553d928c88053229a294d0aaea99659859966e

package:
  rqrgibbs 0.1.0.9012

fit schema:
  rqrgibbs_fit/1.9.0

runtime-attestation schema:
  rqrgibbs_runtime_attestation/5.0.0
```

The launch commit changed only the bounded authorization field and its exact
configuration test relative to the accepted source. A fresh isolated runtime
was built from that full commit. Its identities were:

```text
runtime tree:
  d028a74fb593650d2809fb2152927c74d00505e71652bf63054ae7fed385dba9

runtime attestation:
  584f94c5c6a8622ca9a4929a38e4a83d5c67a6ac1e60d39508aa3a8079360542

runtime toolchain:
  f324fc2b9d8ca15c955ca6f4c74d499e0266730b381044d8f51db28f667667b1

frozen configuration:
  333d10aabf4195d03627301d97b641d11eacfeabc7a278cd5eeb5f1fc5a620fd
```

The committed authorization was revoked immediately after the run. The normal
branch is again fail closed. The completed evidence remains tied to
`afc9c5f...`; no later commit is presented as the execution source.

## Fresh launch gates

The exact launch source passed:

- repository smoke checks;
- the native R and C++ suite;
- `R CMD check --no-manual` with status OK;
- the main and supplement PDF builds;
- the 18-PDF literature manifest;
- the archive-only exdqlm reference smoke at
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`;
- bounded construction preflight;
- all 43 reference calculations;
- all six continuation, time-zero, schema, and bitwise cells;
- all eight monitor and finalizer fault cases; and
- the no-confirmation execute test, which stopped with no fitted object.

The fresh reference bundle was:

```text
reference artifact manifest:
  c5ca9a6b2701ad2c3caa032d29effd3a7c543d8bf05fa41ca5887a389386ff46

reference gates:
  0de3ccced9f813fd62244d601fc2561593f5fe47dc2c3cc6b1c86fb623527f05

reference bundle:
  418bab53c3e4d5c116fcc42e8ecde0ffc5e09b3d46423d8be8da72bf6120e39b
```

Neither protected repository was modified. The exdqlm check used a Git
archive and isolated build under the ignored RQR-GIBBS cache. The Q-DESN
article repository was read only.

## Bounded grid result

The frozen schedule contained:

```text
3 fixtures x 2 learning-rate modes x 4 chains = 24 fits
burn-in per chain:                              2,000
retained draws per chain:                       6,000
thinning:                                           1
backend:                                           C++
numerical policy:                                  fail
execution order:                             sequential
```

All 24 fits completed. There were no retries, seed changes, chain extensions,
or threshold changes. Every fit reported:

```text
exact_joint_target:              TRUE
numerical_repair_count:             0
forecast_repair_count:              0
target_numerical_eligible:        TRUE
reproducibility_eligible:         TRUE
promotion_eligible:               TRUE
```

The predeclared maintained diagnostics all passed:

| Fixture | Learning-rate mode | Diagnostics | Maximum R-hat | Minimum bulk ESS | Minimum tail ESS |
|---|---|---:|---:|---:|---:|
| fixed-W local level | fixed | 117 | 1.000209 | 9445.082 | 12118.100 |
| fixed-W local level | learned normalized | 118 | 1.000915 | 7247.279 | 9940.894 |
| frozen trend and seasonal discount | fixed | 181 | 1.000564 | 13326.980 | 13166.180 |
| frozen trend and seasonal discount | learned normalized | 182 | 1.000529 | 10110.930 | 14419.580 |
| component-scale trend and regression | fixed | 149 | 1.001650 | 1116.971 | 1657.193 |
| component-scale trend and regression | learned normalized | 150 | 1.004908 | 1411.395 | 2041.284 |

Thus 897 of 897 diagnostic rows passed the frozen gates
`R-hat <= 1.01`, `bulk ESS >= 1000`, and `tail ESS >= 1000`. Fixed-rate
lambda was checked by exact identity and was not treated as a stochastic
estimand. Root-swap frequencies ranged from 0.48925 to 0.50650 and remain a
sidecar because the algorithm proposes a global swap with probability one
half.

Every published RDS object was reopened. Its byte count, SHA-256 value,
checkpoint digest, continuation-history digest, and serialized object digest
matched the compact manifests. All checkpoints recorded 8,000 completed
iterations. Missing-response and future-root checks passed for every fit, and
every future artifact states that no response-simulation contract is implied.

## Resources and storage

The 24 fits used 4,078.65 aggregate fit seconds. Individual fits took
135.872 to 196.745 seconds. PGID-sampled peak resident memory was 474,732 KiB.
The runner exited zero, no sampled resource limit fired, and the final process
group was empty. Sampled RSS is telemetry rather than a kernel-hard peak.

The 24 full fit objects total 272,089,116 bytes and remain ignored under
`application/outputs/`. The 12,384,485-byte retained component-conditional
table and raw process monitor also remain local. Their identities are retained
in the original artifact manifest. The tracked evidence substitutes a
four-row component-scale summary and preserves the compact diagnostics,
posterior summaries, provenance, checkpoints, resource closeout, and chain
hashes.

The independently regenerated compact packet is:

```text
docs/audits/rqr_dlm_output13_bounded_20260724/

evidence manifest SHA-256:
  d2d060d60df4233cc336fd74e566be3aef2653d9ec2896a3c945a830d637d533
```

`application/scripts/11_promote_rqr_dlm_bounded_evidence.R` verifies the
complete ignored source manifest, reopens all 24 fit objects, checks the
statistical and provenance gates, and regenerates this compact packet.

## Scientific interpretation

This result completes the bounded target-mechanics, numerical, mixing,
continuation, provenance, missing-data, and future-root checks for the three
declared fixed-joint RQR-DLM modes. It supports implementing a separately
reviewed simulation runner.

It does not establish:

- empirical coverage at any nominal level;
- narrower intervals at comparable coverage;
- recovery of population RQR roots in repeated samples;
- comparative forecasting performance;
- a response likelihood or posterior-predictive response distribution;
- the calibration of the same-data learned loss scale;
- CAVI or ELBO validity; or
- RQR-DESN performance.

Those questions belong to the matched simulation protocol. No matched or
production simulation is authorized by this closeout.
