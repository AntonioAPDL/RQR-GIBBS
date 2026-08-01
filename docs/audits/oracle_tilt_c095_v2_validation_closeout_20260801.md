# Validation closeout for the version-2 95% oracle-tilt illustrations

## Decision

The version-2 fixed-design and dynamic-linear illustrations passed their
prospectively frozen validation contract and are eligible for manuscript use.
All 27 planned chains completed, all six family--target cells received strict
pass status, all 558 monitored MCMC diagnostics passed, and every fit recorded
zero numerical repairs. The promoted artifacts are compact summaries of two
single frozen data sets. They are not a repeated-sample simulation study,
evidence of coverage calibration, or posterior-predictive response output.

After packaging the evidence, execution was disabled again in the tracked
configuration. The configuration stored with the evidence remains the exact
authorized configuration used by the run; its SHA-256 is recorded below.

## Immutable source and runtime

| Item | Frozen value |
|---|---|
| Scientific source commit | `fec979f927c9039cf778ac09aef139ebd6761e8e` |
| Package version | `0.1.0.9030` |
| Configuration SHA-256 | `e037461a3adf3a98065af9718daf636e6fbbd54ec00cc2fd71c9404f6ba50587` |
| Runtime tree digest | `d7890468c42d046697a11fe6784e7f08ac26a72c4764d5adb9ecca37931158b9` |
| Runtime-attestation SHA-256 | `c6311ef1b8ad3ccd82be54fb902e809ef7070ff83a90f08a3050f93a515edd62` |
| R version | `R 4.5.3` |
| Platform | `x86_64-redhat-linux-gnu` |
| Execute interval | `2026-08-01 02:00:51--06:32:19 UTC` |

The run used a clean standalone clone pinned to the complete source SHA and an
isolated package runtime built from that commit's Git archive. Source/runtime
matching passed. The exdqlm and Q-DESN reference repositories were not
modified, compiled, installed, or loaded.

## Frozen scientific contract

The content is `c=0.95`. Innovations are generated from
`AL_0.80(location=0, scale=1)` and affinely standardized to mean zero and
variance one. The resulting skewness is approximately `-1.797617`. Ordinary
RQR, equal-tailed (ET), and shortest-contiguous (SH) tilts are calculated from
exact population quantiles and truncated first moments:

| Target | Tilt | Innovation width | Expected rarer-tail count, static | Expected rarer-tail count, DLM |
|---|---:|---:|---:|---:|
| RQR | `0.000000` | `4.470979` | `14.499` | `14.233` |
| ET | `0.053258` | `3.866596` | `30.000` | `29.450` |
| SH | `0.108986` | `3.632859` | `12.000` | `11.780` |

No Cornish--Fisher approximation is used. The fixed design has `n=1200`, an
empirical orthogonal quadratic basis, and common ridge variance one selected
by a frozen data-independent prior-predictive rule. The DLM has `T=1200`, 22
responses missing in two fixed 11-time windows, `C0=diag(1,0.25)`, and the
exact grid-step covariance for one continuous-time local-linear evolution over
the physical horizon `[0,1]`, with intensities `q_level=0.04` and
`q_slope=0.09`.

## Staged validation

Preflight passed its design-rank, orthogonality, prior-selection, rare-tail,
missingness, and fixed-horizon covariance checks. Reference-only validation
passed all 12 gates. The largest dense-conditional mean error was
`7.52e-15`, the largest marginal-covariance error was `1.17e-14`, R/C++ mean
and covariance differences were zero at the recorded precision, the
missing-measurement omission check passed, and both implementations required
zero covariance repairs.

The two one-chain SH benchmarks passed before the full run:

| Family | Endpoint RMSE / oracle width | Mean-width ratio | Repairs | Result |
|---|---:|---:|---:|---|
| Fixed design | `0.03952` | `0.95443` | `0` | pass |
| Dynamic linear roots | `0.02305` | `1.01356` | `0` | pass |

The definitive execution used four static chains and five dynamic chains per
target, with 6,000 retained draws per chain. Two complete invariant Gibbs
transitions were composed between retained fixed-design draws; the DLM used
one complete transition per iteration. The strict diagnostic extrema were:

| Diagnostic | Worst value | Required bound | Cell |
|---|---:|---:|---|
| Rank-normalized R-hat | `1.00377` | `<= 1.01` | DLM / SH |
| Bulk ESS | `1529.59` | `>= 1000` | DLM / SH |
| Tail ESS | `2256.64` | `>= 1000` | DLM / SH |
| MCSE / posterior SD | `0.02609` | `<= 0.05` | DLM / SH |

## Recovery summary

| Family | Target | Endpoint RMSE / oracle width | Mean-width ratio | Realized content | Joint endpoint inclusion |
|---|---|---:|---:|---:|---:|
| Fixed design | RQR | `0.06160` | `1.01819` | `0.95417` | `1.00000` |
| Fixed design | ET | `0.03079` | `0.95399` | `0.95667` | `1.00000` |
| Fixed design | SH | `0.03891` | `0.95388` | `0.95417` | `1.00000` |
| Dynamic roots | RQR | `0.05456` | `1.03453` | `0.95501` | `0.99917` |
| Dynamic roots | ET | `0.03371` | `0.98060` | `0.95586` | `1.00000` |
| Dynamic roots | SH | `0.02329` | `1.01355` | `0.94992` | `1.00000` |

Realized content describes the two frozen data sets only and was not used to
select seeds, tune priors, or estimate repeated-sample calibration.

## Resource and process audit

The monitored execute wrapper completed in 16,291 seconds. Peak sampled RSS
was 12,050,248 KiB under the 12,582,912-KiB ceiling. This is only about 4.2%
headroom, so a future run with larger retained objects should not reuse the
same memory limit without a new benchmark. At most three R processes were
sampled: the parent and two chain workers. The complete process group reached
five processes and six threads because bounded read-only provenance/toolchain
helpers were included. The declared envelopes were seven processes and eight
threads. No timeout or resource trigger occurred, and the final process-group
sweep was empty.

## Promoted compact evidence

The allowlisted evidence is in `figures/data/oracle_tilt_c095_v2/`. It contains
CSV/JSON summaries and hashes but no fitted R objects, full chains, runtime
libraries, logs, or generated TeX products.

| Artifact | SHA-256 |
|---|---|
| Evidence receipt | `83bdf695e3053c5d3106d850bd95295b52e2ec3a048f655d4b3450b5316264d5` |
| Evidence manifest | `e7c056e94c77dde211fac8a737c375bf01af4852db7475bc84c518044081e71c` |
| Source wrapper artifact manifest | `9edd47767740e5d80b632f7ef6b1cace913258e5323663bc19ad42103617b385` |

The figure generator verifies every manifest byte count and SHA-256 before
rendering and fails unless the receipt states exact population tilts, no
Cornish--Fisher use, and manuscript-evidence eligibility. It also now requires
strict-pass status for all six cells.

## Interpretation and final disposition

These results validate the declared fixed-rate generalized-posterior scans on
the two frozen illustrative data sets. The asymmetric-Laplace law is the DGP
for the illustrations and supplies exact population functionals; it is not
asserted as the fitted response likelihood. The orange intervals and ribbons
summarize interval-root draws, not simulated responses. The evidence supports
the revised article figures and their narrowly scoped implementation claims.
It does not authorize claims about empirical calibration, comparative
forecasting performance, automatic tilt selection, or a response-predictive
distribution.
