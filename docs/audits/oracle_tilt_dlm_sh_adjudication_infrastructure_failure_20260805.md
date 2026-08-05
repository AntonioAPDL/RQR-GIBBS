# DLM/SH adjudication infrastructure-failure audit

## Scope and disposition

This audit covers the first execution of the one-shot dynamic-linear
shortest-interval illustration adjudication. The job used source commit
`3ac3a05db420bf17cdeffbb41f0b6b8947b373f4`, an exact isolated runtime, the
immutable version-3 baseline, five unchanged seeds, and 12,000 retained draws
per chain. It concerns a loss-based interval-root generalized posterior. It is
not a response-likelihood analysis, a posterior-predictive response analysis,
or a repeated-sample simulation study.

The execution is invalidated as a prepublication infrastructure failure. It
produced no worker artifact, prefix comparison, MCMC diagnostic, recovery
summary, or scientific decision. It is not counted as the single statistical
adjudication allowed by the frozen protocol.

## Observed execution

The five preflight gates passed. The monitored process ran for 9,078 seconds,
used at most 3,729,320 KiB sampled RSS, six threads, five processes, and three R
processes, and ended with an empty process group. It did not time out, receive a
signal, or cross a sampled resource limit. The process trace is consistent
with two two-chain batches followed by a singleton chain. No file was present
under `worker_results/` at closeout.

| Artifact | SHA-256 |
|---|---|
| `runner.stderr.log` | `2e65a88ce504ee2625763decf946ca74833f68cc62c3f7dbdeffb7d7de246094` |
| `resource_summary.csv` | `65059192c22e7aaf4b67b1db5a27c31f402728b4b41878589ca7dffcf8c69b8b` |
| `wrapper_closeout.csv` | `d86189c7d1bd349221555dc0da22cbedcc54dc78ec0c81fd831677feaf2ce04b` |
| `wrapper_failure_log.csv` | `9729ba67ca9fa5dead50a87ca2bb49465b6d7664c05a9ad1504b6c4faf271faa` |
| `wrapper_artifact_manifest.csv` | `2e41a28dab0d201e34672b05e47a5a2045a2511367bb8bb6189bfa23b4732f61` |
| `preflight_gates.csv` | `0a2f9404b78828e137f2f1011fac475c8b3a8a693147ec89bf4c2e4039ae4f41` |
| `source_state.json` | `6561a4ab03756765c5c86d78be63fda5be9979e8c4cdb2360d48f3e3c8f25f2f` |
| `runtime_binding.json` | `fda18defb38f2819bac737e043ecbc7f8e70315e14d36d5218fcb6f4ca42cbda` |

## Root cause

The worker validator applied `otv3_prediction_storage_contract()` and compared
its logical return value with the character string
`"ordered_endpoints_only"`. A valid endpoint-only payload therefore produced
`identical(TRUE, "ordered_endpoints_only")`, which is always false. A bounded
reproducer returned:

```text
storage_helper_value=TRUE
buggy_identical_to_text=FALSE
validator_result=The adjudication worker envelope is invalid.
```

The runner validated each envelope before its atomic write, so the defect
discarded otherwise completed in-memory results. In addition, parallel batches
wrapped worker errors in `tryCatch()`, whereas the final singleton batch called
the worker directly. The final error therefore bypassed the internal
chain-specific failure ledger and reached only the outer wrapper.

## Corrective boundary

The replacement changes only software validation and orchestration:

1. require the logical endpoint-storage predicate directly;
2. validate payload dimensions, finiteness, ordering, scalar draws,
   provenance, and contract digests;
3. use one error contract for singleton and parallel batches;
4. exercise a production-shaped 12,000-draw envelope during preflight;
5. validate and compare chain 1's original 6,000-draw prefix before starting
   chains 2--5; and
6. report wrapper failure as authoritative in the health command.

The response, missingness, model matrices, fixed evolution covariance, prior,
learning rate, C++ sampler, burn-in, retained draws, thinning, initialization
profiles, seeds, oracle tilt, diagnostic gates, recovery gates, and decision
rule remain unchanged. Gate relaxation and model or DGP retuning remain
prohibited.
