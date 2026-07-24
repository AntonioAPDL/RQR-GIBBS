# ChatGPT Pro Output-9 reconciliation

Date: 2026-07-23  
Reviewed implementation commit: `2e7840388d5612f5ebe9234c80f28c650c145b9c`  
Package: `rqrgibbs 0.1.0.9009`  
Fit schema: `rqrgibbs_fit/1.7.0`  
Continuation schema: `rqrgibbs_continuation_history/4.0.0`  
Runtime-attestation schema: `rqrgibbs_runtime_attestation/5.0.0`  
Bounded-fixture schema: `rqrgibbs_dlm_bounded_fixtures/4.0.0`

## Executive decision

Output-9 was treated as an independent review, not as execution authority.
Its two concrete counterexamples were independently reproduced:

1. runtime-lineage version 4 accepted a built source package after deleting a
   committed `inst/` file; and
2. continuation-history version 3 accepted a raw repair count of `0.5` after
   integer coercion and recomputation of the ordinary digest.

Both counterexamples are rejected by the new source and tests. The remaining
justified findings were implemented in the reference suite, process monitor,
and bounded-cell runner. The resulting exact-commit reference run passed all
40 gates. A representative four-chain benchmark also completed with zero
repairs and exact provenance, while exposing two component-scale bulk-ESS
values just below the prospective threshold of 1,000.

```text
Statistical target and interpretation:          PASS
Runtime lineage version 5:                      PASS for controlled local lineage
Continuation history version 4:                 PASS
Expanded reference-only validation:             PASS, 40/40
Representative one-cell benchmark:              PASS for mechanics/resources
Prospective diagnostic gate in that benchmark:  148/150
Execute the 24 bounded fits:                     NO-GO pending independent review
Matched or production RQR-DLM simulation:       NO-GO
CAVI/ELBO:                                      DEFER
RQR-DESN:                                       DEFER
```

The benchmark is evidence about the implemented generalized-Bayes
interval-root sampler, continuation, forecasts, diagnostics, provenance, and
resource footprint. It is not evidence of empirical coverage calibration,
response-predictive validity, or comparative forecasting performance.

## Input integrity and protected repositories

The supplied files are chat/project exports rather than the standalone
downloads whose hashes appeared in the Pro response. Their observed hashes
are:

| Local input | Observed SHA-256 | Pro-reported SHA-256 |
|---|---|---|
| `chatgpt_pro_output9_audit_20260723.md` | `21583356ecac8544dd5919926451c9fecf200e7645c1aaeab9538b8a5f1f2ac6` | `a6546e4feddafd6fa16d3a23b8a0c1b5ad37e3214422e83a2ca26bd964b9046f` |
| `chatgpt_pro_output9_codex_handoff_20260723.md` | `890836db9d4d96c834d98ff00bb9933c760325bfeb1da55ae00b0a770dcb244b` | `9e0a3ede8fae29bc5bc6af91db7504a09b94f91bf156244c2f4b83936e5e2b75` |
| `chatgpt_pro_output9_findings_20260723.csv` | `1f31d1a7e64b9b7afd53243333a62cb22b949b09ba05afadc84c8ebb3915de3f` | `350c32a58978394fbdece213f8319c83069d25b4424fea11b0d37c3ad8e89264` |
| `chatgpt_pro_output9_artifact_hashes.csv` | `1064b4232be28bce7438f9da6c1c5799ad424569b2e766aef82bba5204d0125d` | `9ab0085af659b37b74308824b6b00a4ca09c14668fc6a202de9f2abab5182618` |

The handoff export arrived with a leading space in its local filename. All
four files remain ignored. Their claims were checked against source and
executions rather than accepted on the basis of the wrapper files.

Only RQR-GIBBS was modified. The protected references remained clean:

| Repository | Branch | Commit |
|---|---|---|
| exdqlm pinned reference | `feature/rqr-desn-readout-20260716` | `dffb71ee70b597d6a716ee74be1cbc99731cd453` |
| Q-DESN article reference | `main` | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` |

The exdqlm smoke suite used an immutable Git archive and an isolated runtime
under RQR-GIBBS's ignored cache. Its protected-checkout guard was identical
before and after:
`0b24b9b9136935d47510fc4fa10c324514918f69a44d6d54bb9f126cf22f332c`.
Nothing was compiled, installed, or loaded directly from the exdqlm checkout.

## Finding disposition

| Output-9 finding | Disposition | Correction or evidence |
|---|---|---|
| Strict-subset source package can pass | Confirmed and corrected | Version 5 derives the complete expected built-source file set after documented R build exclusions and compares both directions. Deleting the committed `inst/` fixture now fails. |
| Install receipt accepts ambiguous/failed installs | Confirmed and corrected | Post-command receipts record status, timestamps, exact input/output/library, logs, and hashes. Verification requires status zero, exactly one input, the exact library, and excludes partial/multiple installs. |
| Fractional continuation counters are truncated | Confirmed and corrected | Raw values must be finite, scalar, whole, nonnegative, and within integer range before coercion. Tests cover `0.5`, `-0.5`, `Inf`, and overflow in early generations and completed-iteration fields. |
| Dense FFBS references are incomplete | Corrected | Full cross-time covariance and selected adjacent-time covariance are checked with Gaussian sample-covariance standard errors. |
| Canonical missing/future/scale references are incomplete | Corrected | Actual missing indices, placeholder invariance, all three public future-root contracts, and retained two-component inverse-Gamma conditionals are checked. |
| Continuation covers only one cell | Corrected | Uninterrupted six-draw versus `2+2+2` equality is checked for all six fixture/mode cells and every saved stochastic field. |
| Process monitor is incomplete | Corrected using the accepted fallback | PGID monitoring has signal/exit traps, a final sweep, a leader-exit/descendant-survival fault test, and an explicit startup handshake. RSS remains accurately labeled sampled telemetry, not a kernel-hard ceiling. |
| Execute estimands are incomplete | Corrected | The runner diagnoses every training-time endpoint/midpoint/width, future root functionals, observed loss, time-zero and terminal label-invariant state functions, learned rate, component scales, and innovation energies. |
| Cell-level stop rule absent | Corrected | Each four-chain cell is diagnosed before the next cell; a failed execute-mode cell prevents later cells from running. |
| Reference/runtime binding incomplete | Corrected | Execute and benchmark modes rehash the complete reference directory and bind the exact source, runtime attestation/tree, toolchain, gate table, resources, bundle, and recursive artifact manifest. |
| Artifact/failure contract incomplete | Corrected | Atomic writes, explicit fit plan/status, structured failure log, compact summaries, provenance/checkpoint/chain hashes, sidecars, wrapper closeout, and recursive hashes are implemented. |

## Runtime lineage version 5

The controlled primary and exdqlm builders now:

- derive the expected source-package file set from the verified Git archive
  after documented `.Rbuildignore`, standard R build exclusions, generated
  metadata, and allowed `DESCRIPTION` transformations;
- compare the expected and built file sets in both directions;
- write build and install receipts only after command completion;
- record status, start/end timestamps, exact executable, arguments, working
  directory, one input, output, library, and log hashes;
- reject failed commands, multiple package inputs, partial installs, and
  pre-existing runtime reuse;
- bind the verified archive, exact built source package, pre-marker installed
  runtime, lineage marker, and final executing runtime; and
- retain positive real-build tests plus strict-subset, mixed-lineage,
  arbitrary-package, multiple-input, and failed-install negative tests.

The final isolated primary runtime records:

```text
source commit:
  2e7840388d5612f5ebe9234c80f28c650c145b9c
application tree:
  95766f3d8f043529bd260380405cad957eda38b9
source archive SHA-256:
  d46198f21881445507e05d929e23e10fd568bc55dd0ba9354aff299a780897d4
source package SHA-256:
  58c3dda622da6dbdfb8875bb88d10374a52790fd3bbb8b2433d800fce42aacca
expected source manifest SHA-256:
  a4ea88740570cafb3277c747c0fbb666fa39fa409b59feaccad937cfb9ce16bb
built-source manifest SHA-256:
  48a4c4d66e279dc723a25084efc2fc90293a70a00269fb1f980bd9254cc00a0d
pre-marker runtime-tree SHA-256:
  a064faa6eb11f610ddc6f34415902168feb8f0a25dc770ed1be1b3ea561f5292
final runtime-tree SHA-256:
  199637353790a0ab7d119b1e7ab49acc68a6ff88b91237227e0a4741196c9059
runtime install-receipt digest:
  b86090e357397cc70734a872c892ddd25dee1a7e6fa0e46aa07928e2c74d5e65
```

This is a controlled local-build integrity contract. It does not claim
signed provenance or bit-for-bit reproducible binaries across toolchains.

## Continuation history version 4

The maker and validator now check raw generation, repair, cumulative, and
completed-iteration fields before integer conversion. The validator continues
to reconstruct exactness, target eligibility, environment eligibility,
backend changes, overrides, cumulative repair state, mismatch ledger,
reproducibility, and promotion from per-segment raw facts.

The expanded reference suite rejects 27 digest-consistent raw and semantic
mutations. It also verifies bitwise continuation for all six fixture/mode
cells across root ordinates; full, terminal, and time-zero state paths;
learning rate; component scales; retained component conditional parameters;
checkpoint; and history.

## Runner and process evidence

The runner has four modes:

- `preflight`;
- `reference-only`;
- `benchmark-one-cell`; and
- `execute-bounded`.

The committed 24-fit authorization remains false. Benchmark authorization is
separate and was used only for the single declared fixture/mode cell.

The monitor uses the acceptable Output-9 fallback because this host exposes
legacy cgroup v1 and the cgroup root is not writable by the user. It monitors
the R process group every 0.2 seconds, installs `EXIT`, `INT`, `TERM`, and
`HUP` traps, drains descendants after the group leader exits, and performs a
final PGID sweep. A startup race discovered during validation was fixed by
requiring the new process group to become observable before monitoring begins.
Sampled RSS is telemetry with best-effort termination; it is not called a
kernel-hard peak.

## Expanded reference-only result

The exact-commit reference run is:

```text
directory:
  application/outputs/
  rqr_dlm_bounded_reference_only_20260723T234148Z_2e7840388d56
reference gates:                  40/40 pass
bounded grid fits executed:       0
peak sampled processes:           3 / 3
peak sampled threads:             4 / 4
peak sampled RSS:                 191,880 KiB / 4,194,304 KiB
hard timeout:                     not triggered
monitor fault test:               pass
final process group:              empty
artifact-manifest SHA-256:
  ce875dcff308dc25b7bb6ba82ee47ca1431c4a427f19c82fa4678209224e98d1
reference-gate SHA-256:
  b72ecb7bf109dd95605d3f89bd4e2f062130c243f788bcb03fca0ebd1d2fdb2c
reference-bundle SHA-256:
  47b2e54419a337eaa07cc90ec222f2bb9a6eca47228394c9e3d883b9b614cf66
```

The compact gate and artifact tables are tracked as
`rqr_dlm_bounded_reference_gates_20260723.csv` and
`rqr_dlm_bounded_reference_artifact_hashes_20260723.csv`.

## Representative one-cell benchmark

The benchmark used the most computationally informative declared cell:

```text
fixture:              shared component-scale trend plus regression
learning-rate mode:   learned pseudo-residual normalized
chains:               4
iterations/chain:     2,000 burn-in + 4,000 retained
backend:              C++
numerical policy:     fail
state paths:          retained locally
latent paths:         not retained
```

All four fits had zero numerical and forecast repairs, exact-joint-target
status, target numerical eligibility, runtime/source match, reproducibility
eligibility, and promotion eligibility. Chain elapsed times were 127.6 to
139.6 seconds. Full local chain objects totaled 30,299,256 bytes. The complete
output directory was approximately 34 MiB.

Resource evidence:

```text
peak sampled processes:  1 / 3
peak sampled threads:    2 / 4
peak sampled RSS:        325,080 KiB / 4,194,304 KiB
hard timeout:            not triggered
monitor fault test:      pass
artifact-manifest SHA-256:
  4068af40d8eca2dc07ad4bde0a28b42aac3a049e20de86e222122b9934ba572b
```

The benchmark generated 150 explicit rank-normalized diagnostics. All R-hat
values were at most `1.003512`, all tail ESS values were at least `1261.34`,
and 148 diagnostics met all prospective gates. Two component-scale
diagnostics had bulk ESS below 1,000:

| Estimand | R-hat | Bulk ESS | Tail ESS |
|---|---:|---:|---:|
| log regression-component scale | `1.000803` | `962.08` | `1261.34` |
| regression-component innovation energy | `1.000294` | `971.87` | `1330.98` |

The benchmark correctly labels diagnostics as descriptive rather than a
benchmark pass gate. These results do not justify weakening the prospective
ESS threshold. They should be independently reviewed to decide whether the
eventual bounded grid should use a longer retained schedule or another
predeclared response.

Root-swap fractions were 0.487 to 0.506, but remain a sidecar because the
sampler deliberately proposes a global label swap with probability one half.

Tracked compact benchmark evidence includes fit audit, all 150 diagnostics,
resource summary, artifact hashes, and hashes of the ignored chain files. The
chain objects themselves remain ignored.

## Validation matrix

Completed successfully:

- parse checks, shell syntax, and `git diff --check`;
- environment smoke;
- full native R/C++ tests;
- strict-subset and receipt/runtime-lineage negative tests;
- fractional/overflow continuation negative tests;
- `R CMD check --no-manual` with `Status: OK`;
- main and supplement PDF builds;
- literature manifest for 18 local PDFs;
- pinned exdqlm RQR smoke tests from an immutable archive-built runtime;
- canonical preflight;
- process-group fault injection;
- expanded reference-only validation, 40/40;
- one-cell four-chain benchmark with complete compact artifacts; and
- execute-mode negative test with the correct reference binding and
  confirmation phrase.

The execute-mode negative test returned status 1,
`status=blocked_by_execution_contract`, and created zero chain files because
the committed 24-fit authorization is false.

## Remaining decision

The code is ready for another independent review of the exact implementation
commit, the 40-gate reference bundle, the monitor evidence, and the one-cell
benchmark. It is not appropriate to enable the 24-fit flag yet.

The next review should decide:

1. whether runtime lineage version 5 and continuation history version 4 close
   the concrete Output-9 counterexamples;
2. whether the 40-gate reference suite is sufficient for the bounded grid;
3. whether the PGID sampled fallback is acceptable on this Jerez host;
4. whether the two near-threshold component-scale bulk ESS values require a
   longer frozen schedule before the 24-fit grid; and
5. whether the compact artifacts and per-cell fail-fast behavior are
   sufficient to issue a conditional go.

No matched/production simulation, CAVI/ELBO work, or RQR-DESN work is
authorized by this reconciliation.
