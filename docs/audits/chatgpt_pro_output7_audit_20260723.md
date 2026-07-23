# ChatGPT Pro Output-7 reconciliation

Date: 2026-07-23  
Implementation commits: `4f2c523c736a4aa7c0c63bdd2e6602beee80e905`,
`3a37c5ee4297aee87d2fc4c46f9559fe296ce009`  
Package: `rqrgibbs 0.1.0.9007`  
Fit schema: `rqrgibbs_fit/1.5.0`  
Continuation schema: `rqrgibbs_continuation_history/2.0.0`  
Runtime-attestation schema: `rqrgibbs_runtime_attestation/3.0.0`  
Bounded-fixture schema: `rqrgibbs_dlm_bounded_fixtures/2.0.0`

## Scope and source handling

Output-7 was treated as a set of claims to reproduce, not as executable
authority. The RQR-GIBBS repository was the only repository mutated. The
pinned exdqlm checkout remained on
`feature/rqr-desn-readout-20260716` at
`dffb71ee70b597d6a716ee74be1cbc99731cd453`; its full protected-checkout guard
passed before and after its isolated runtime build and smoke tests. The Q-DESN
article checkout remained on `main` at
`f9f22804eff3871bb5350c8add04b7c9f4d4957b`.

The user-supplied Output-7 exports did not have the SHA-256 values printed in
the Pro response:

| Local input | Observed SHA-256 | Note |
|---|---|---|
| `chatgpt_pro_output7_audit_20260723.md` | `15c62b43318f616569eb80e9772d39001ccf24b869dc0a20f337339f2e060eed` | exported chat transcript containing the report, not the claimed standalone file |
| `output7_corrected_cdf_gate_check` | `36482fa4ba486b91c842ef0f24ccf6931c17f15e5008c6a429fd0588a6c634be` | tab-delimited and lacks a `.csv` suffix |
| `output7_corrected_independent_cdf_references.csv` | `01dddfd698632dedbbca63c6385cb12262fafd80e258127d25d84fc6543f773b` | tab-delimited despite the suffix |

These raw files remain ignored. Their numerical claims were independently
checked before promotion into tracked source.

## Finding disposition

| Output-7 finding | Disposition | Evidence or correction |
|---|---|---|
| Four root-functional CDF references were wrong | Confirmed and corrected | A new event-boundary-aware generator splits response kinks and the lower, upper, width, or midpoint event boundaries. Orders 48, 64, and 80 converge to machine precision. |
| Primary direct-source promotion bypass | Resolved for promotion | When an expected primary commit is declared, provenance now requires an isolated attestation. `pkgload::load_all()` remains usable for exploratory tests but cannot be reproducibility- or promotion-eligible. |
| exdqlm archive lineage was self-asserted | Substantially strengthened | The verifier independently compares Git and extracted-archive mode--blob--path manifests, verifies archive and built-source-package hashes, verifies the installed tree, and checks a build/install receipt. A real Git archive passes; arbitrary bytes fail. |
| Continuation history was only shallowly digested | Resolved for structural and recursive consistency | Every generation now stores parent checkpoint, segment and cumulative repairs, segment and cumulative exactness, target status, environment status, reproducibility, promotion, override status, mismatches, and backend. Validation reconstructs all recursions and the mismatch ledger. |
| FFBS numerical boundary | Retained as pass | No mathematical or numerical change was justified. Existing R/C++ boundary tests remain passing. |
| `coda` was not a modern diagnostic implementation | Corrected | `posterior 1.7.0` supplies primary rank-normalized R-hat, bulk ESS, and tail ESS. The custom implementation is a reproduction cross-check; `coda` is a classical sidecar. |
| Worker and memory claims were overstated | Corrected in scope | Thread limits must be set before R starts. The unsupported hard-coded worker claim and whole-job memory gate were removed. Main-process `VmHWM` is retained only as a labeled sidecar. |
| Frozen dynamic `C0` values were invalid | Confirmed and corrected | Trend, seasonal, and scalar regression covariances are now explicit square matrices. |
| Future evolution contracts were incomplete | Confirmed and corrected | The discount recursion is frozen through `T+H` and must reproduce its first `T` slices exactly. Component-scale future templates and future regression design are explicit. |
| Preflight did not instantiate approved objects | Confirmed and corrected | Tests and preflight share one canonical constructor that builds every component, combined model, missing response, evolution, and future object and records their digests. |
| Root-swap activity should not be a primary convergence gate | Confirmed and corrected | It is a sidecar. Primary convergence targets are label-invariant endpoints, width, midpoint, loss, learning rate, and component scales. |

## Corrected CDF references

`application/scripts/07_generate_intercept_cdf_references.R` independently
produces:

| Event | Reference |
|---|---:|
| `Pr(lambda <= 1)` | 0.347247584303805 |
| `Pr(lower root <= -1.5)` | 0.408193003274045 |
| `Pr(upper root <= 2.5)` | 0.562140568140968 |
| `Pr(width <= 4)` | 0.573003849468578 |
| `Pr(midpoint <= 0.5)` | 0.489059519337561 |

The tracked contract records the generator SHA-256
`f27a07c0747abe43451ec59d286f6b5c4c4e4a91207a1dced6d4140af8de0fba`.
A separate regeneration had maximum absolute difference zero from the tracked
values.

## Runtime and continuation corrections

The version-3 runtime builder uses an exact read-only Git archive. Before
building, it compares every archive entry with the declared Git tree by file
mode, Git blob identifier, and relative path. The attestation then records and
the verifier recomputes the source archive hash, source-package hash, installed
runtime digest, and an installation receipt over the build and install log
digests and R/platform identity.

The primary runtime is deliberately outside the source checkout at:

```text
/data/muscat_data/jaguir26/.rqr_gibbs_primary_runtime/<commit>/
```

The exdqlm runtime remains under the ignored RQR-owned cache, which is disjoint
from the protected exdqlm checkout.

The continuation-history tests now include a three-generation chain. They
mutate generations zero and one, recompute the ordinary SHA-256 history digest,
and still fail because cumulative recursion or parent-checkpoint linkage is
invalid. This is an integrity and consistency contract, not a keyed signature
against an actor able to rewrite the entire fit object.

## Dynamic construction preflight

At exact commit `3a37c5ee4297aee87d2fc4c46f9559fe296ce009`, the isolated
primary runtime and construction preflight passed:

| Fixture | State dimension | Missing | Future horizon | Evolution |
|---|---:|---:|---:|---|
| fixed-W local level | 1 | 2 | 4 | fixed `W` |
| trend plus seasonal | 5 | 0 | 4 | frozen component discount |
| trend plus regression | 3 | 1 | 3 | shared component scales |

The preflight records three fixtures, two learning-rate modes, four chains, and
24 prospective fits. It also records:

```text
production_simulation_authorized=false
bounded_dynamic_execution_authorized=false
```

Thus no dynamic MCMC fit was launched in this reconciliation.

## Corrected intercept-only pilot

The corrected bounded pilot ran at exact commit
`3a37c5ee4297aee87d2fc4c46f9559fe296ce009`:

```text
application/outputs/
  rqr_bounded_pilot_20260723T083242Z_3a37c5ee4297/
```

Decision: **PASS**.

- elapsed wall time: 8.89 minutes;
- zero nonempty failure records;
- zero numerical repairs;
- all maintained diagnostic gates passed;
- all custom-versus-`posterior` cross-checks passed;
- all mean and corrected CDF comparisons passed;
- primary and exdqlm runtime attestations matched;
- source, continuation, wall-time, thread-environment, and artifact gates
  passed;
- compact artifact size before the hash manifest: 0.016 MB.

The largest maintained R-hat was approximately `1.00014`; the smallest bulk
ESS was approximately `15,728`, and the smallest tail ESS was approximately
`25,946`, all well inside the frozen gates. These are interval-root target
checks on one deterministic fixture. They do not establish empirical coverage,
response-predictive validity, or production readiness.

## Validation matrix

The following completed successfully:

- environment smoke;
- package installation;
- bounded configuration and canonical object-construction tests;
- complete native R/C++ test suite;
- `R CMD check --no-manual` with `Status: OK`;
- main manuscript PDF;
- supplement PDF;
- event-boundary CDF regeneration and tracked-value equality;
- version-3 exdqlm isolated-runtime build and focused RQR smoke tests;
- version-3 primary isolated-runtime build;
- exact-commit non-executing dynamic construction preflight;
- corrected intercept-only bounded pilot.

## Remaining limitations and decision

The source-package hash and installation receipt close the identified
non-adversarial mislabeled-archive gap in the controlled builders, but they are
not a bit-for-bit reproducible-build proof against an actor able to replace
both runtime and attestation. Likewise, whole-process-tree peak memory and
thread counts are not yet measured by a dynamic runner; unsupported claims
were removed rather than retained.

Decision:

```text
Corrected intercept-only bounded pilot:       PASS
Source/configuration hardening:               PASS
Dynamic construction preflight:               PASS
Launch three bounded dynamic fixtures:        CONDITIONAL GO after runner review
Matched or production RQR-DLM simulation:      NO-GO
CAVI/ELBO:                                     DEFER
RQR-DESN:                                      DEFER
```

The next bounded task is to implement and review the dynamic runner against the
frozen 24-fit contract, dense or analytic conditional references, future and
missing-state checks, and `2+2+2` continuation. It should measure the complete
process tree or use scheduler/cgroup limits. Only after that runner and its
compact result schema pass review should the three fixtures execute. The
matched simulation protocol, CAVI/ELBO work, and RQR-DESN remain separate later
stages.
