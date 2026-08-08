# RQR-DLM joint-ESS affected-wave launch receipt

Date: 2026-08-05

Scope: fail-closed computational validation; no scientific simulation output
is authorized or promoted by this launch.

## Immutable source and runtime

- implementation commit:
  `73f9918deb91539f06ced88c7803877a3065f42f`;
- source branch: `codex/rqr-dlm-joint-ess-promotion-20260805`;
- launch-checkout branch: local `main` at the exact implementation commit;
- package: `rqrgibbs 0.1.0.9032`;
- fit schema: `rqrgibbs_fit/1.18.0`;
- correction contract: `rqrgibbs_dlm_main_correction/1.15.0`;
- archived application-tree digest:
  `78f2092d48167515fd2d11d784c52cb6174d7802`;
- primary runtime-attestation SHA-256:
  `34e3d7b6f7c9bc7bc097da49b1d4b44a88c5d83f949c6dd92e25508de50b6b9c`;
- CRAN `exdqlm 1.1.0` attestation SHA-256:
  `cb27bc019030ebab93ec9d89550ea548ef41c4804109ee2cd5d7f788425f4423`;
- CRAN `quantreg 6.1` attestation SHA-256:
  `1a1f35f98d6273f29feeca64e5dd4293de59b4253556276369f3208dbe517d02`.

The primary package was built and installed from an exact Git archive in a
disjoint runtime root. The comparator runtimes are isolated CRAN installations.
No `exdqlm` or Q-DESN source checkout was compiled, loaded, or modified.

## Completed source gates

| Gate | Result |
|---|---|
| Full confirmatory contract suite | Pass |
| Native sampler and R/C++ transition suite | Pass |
| R and shell syntax checks | Pass |
| Whitespace and patch-integrity checks | Pass |
| `R CMD build` | Pass |
| `R CMD check --no-manual` | `Status: OK` |
| Unauthorized execution probe | Rejected before output creation |
| Isolated exact-commit runtime build | Pass |

The package check used R 4.5.3 on x86_64 AlmaLinux 8.10. Its build and check
trees remain under ignored `application/cache/` storage.

## Frozen transition decision

The tracked joint-elliptical candidate closeout authenticated 44 successful
jobs and 54 artifacts. All jobs used the exact joint target and zero numerical
repairs. The selected rows passed all 233 frozen diagnostics:

| Method | Selected policy | Selected diagnostics |
|---|---|---:|
| M10 | `joint_ess1_x1` | 92/92 |
| M11 | `joint_ess1_x2` | 141/141 |

The 22 failures among the full 932 candidate diagnostics belong only to
rejected alternatives. Development outputs are not reused as affected-wave or
scientific outputs.

## Background validation chain

- coordinator PID and PGID: `3655490`;
- launch stage at handoff: `s10_guard`;
- S10 guard: M10/S10 replication 77 and M11/S10 replication 166, four chains
  per case, eight jobs total;
- conditional affected wave: exact canonical S05/S06 skewed local-level wave,
  35 replication tasks and 278 method evaluations in eight fixed worker slots;
- task retries: zero;
- reseeding: prohibited;
- numerical policy: fail;
- per-worker numerical threads: one;
- per-worker process-group ceiling: 1.5 GiB RSS and four operating-system
  threads;
- worker timeout: 72 hours.

The coordinator is fail-closed. It will not prepare or execute the affected
wave unless every S10 guard diagnostic passes with exact-target status, zero
repairs, and the frozen resource ceiling. A worker or diagnostic failure stops
the chain. The launch flag remains `FALSE`, and the 8,400-task scientific
simulation has not been started.

## Promotion decision after completion

Passing this affected wave is necessary but not sufficient for the main study.
The next bounded step is to authenticate its compact closeout and rerun the
complete exact-promotion ladder from a fresh isolated runtime. Only after all
Gaussian, skewed, comparator, fixed-design, forecast, continuation,
seed-binding, resource, package, TeX, and literature gates pass may a separate
flag-only authorization commit be considered. The main study must use a fresh
run ID and must not consume candidate or development fits.
