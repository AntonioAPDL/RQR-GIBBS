# RQR-DLM M11 multicomponent recovery plan

## Decision

The main simulation remains unauthorized. The next executable unit is a
development-only, 48-fit comparison of four exact target-preserving transition
kernels. It is deliberately narrower than the failed affected-wave gate and
uses no scientific simulation metrics. A candidate may be promoted to a new
affected-wave gate only if it passes every fixed diagnostic on all three
predeclared cases.

This is the smallest defensible correction. More burn-in, a lucky reseed, a
selective extension of the failed chains, or weaker thresholds would not
diagnose the observed transition failure and would invalidate the promotion
logic.

## Authenticated failure

Implementation `73f9918deb91539f06ced88c7803877a3065f42f` stopped at
the S10 guard before the S05/S06 affected wave. All eight fits succeeded under
the declared exact joint target, used zero numerical repairs, and remained
below the 1.5-GiB worker ceiling. M10 passed 47/47 diagnostics. M11 passed
32/48.

The leading M11 failure was the second evolution scale:

| quantity | observed value | frozen gate |
|---|---:|---:|
| rank-normalized R-hat for `log_q_2` | 1.1616 | at most 1.01 |
| bulk ESS for `log_q_2` | 4.15 | at least 400 |
| tail ESS for `log_q_2` | 22.08 | at least 400 |
| MCSE/SD for `log_q_2` | 0.493 | at most 0.08 |

Profiles A/B occupied a `log_q_2` location near -3.00, while profiles C/D
occupied a location near -3.28. Each group was internally stable across the
first and second retained halves. The separation propagated to width, upper
root, loss, and future-root functions. Late-window R-hat remained about 1.16
for 4,500 retained draws and 1.15 for 2,250 retained draws. This is evidence of
a coupled multicomponent transition problem, not a short-lived burn-in tail.

The earlier candidate selector exercised only one-component S05 cases. It
therefore could validate the first scale but could not expose failure of a
second component scale. The new higher-dimensional guard worked as intended;
the candidate-selection design, not the fail-closed rule, was incomplete.

Compact authenticated evidence is in
`docs/audits/rqr_dlm_s10_guard_failure_20260806/`. Heavy chain objects remain
ignored and are represented by SHA-256 and byte-count manifests.

## Exact transition correction

The existing centered/noncentered interweaving updates each component of
`log(q)` by a scalar stepping-out slice sampler while holding the standardized
state innovations fixed. That coordinate kernel is exact, but a coordinate
move can be inefficient when the conditional scale geometry is oblique or
when movement between state/scale regions requires coordinated changes.

The recovery adds an optional random-direction slice update on the complete
`log(q)` vector. At state `x`, it draws a spherical direction `d` independently
of the state and applies the existing exact scalar stepping-out/shrinkage
transition to

```text
t -> log pi(x + t d | standardized root paths, v, lambda, y).
```

The direction distribution is symmetric, and the line update leaves the
target restricted to that line invariant. Mixing over the independent
directions therefore preserves the same multivariate conditional target. The
move changes neither the loss target nor any prior, seed, schedule, or
diagnostic threshold. It supplements rather than replaces the coordinate ASIS
updates.

This mechanism is preferable to another unexamined ASIS repetition because
the failed quantity is component-specific and the one- versus two-ASIS history
did not improve monotonically. A second joint state elliptical-slice cycle is
retained as a separate comparator because the chain separation also appears in
root functions; it tests whether the limiting dependence is primarily in the
state block rather than the scale block.

## Fixed candidate and case matrix

Candidates are ordered by the amount and locality of added work. The first
candidate passing every case is selected.

| order | candidate | directional scale update | joint state ESS cycles |
|---:|---|---:|---:|
| 1 | `baseline_joint1_coordinate` | 0 | 1 |
| 2 | `directional1_joint1` | 1 | 1 |
| 3 | `joint2_coordinate` | 0 | 2 |
| 4 | `directional1_joint2` | 1 | 2 |

All candidates retain symmetric rootwise partial collapse, two collapsed
cycles, two centered/noncentered cycles, three coordinate slice sweeps per
cycle, the existing fixed M10/M11 role schedules, and the frozen seeds.

The cases were fixed before observing any new candidate diagnostics:

| method | S10 replication | role | chains/profiles |
|---|---:|---|---|
| M11 | 166 | failed preselected sentinel | 4 / A--D |
| M11 | 167 | independent preselected companion sentinel | 4 / A--D |
| M10 | 77 | previously passing fixed-rate guard | 4 / A--D |

This gives `4 candidates x 3 cases x 4 chains = 48 fits`. Replications 166 and
167 were selected by the frozen M11 sentinel stream before the response data
were inspected. Replication 77 is the frozen M10 sentinel already shown to
pass; it protects against fixing learned-rate mixing by degrading the
fixed-rate transition.

## Gates and evidence

Each fit must:

- finish without retry or reseeding;
- use the exact attested primary runtime built from the committed source;
- report the declared exact joint target and zero numerical repairs;
- remain within the frozen per-worker memory ceiling;
- retain identical model, target, and evolution digests across candidates for
  the same method, replication, and chain; and
- report exact coordinate, directional, and joint transition telemetry.

For each candidate/case four-chain cell, every existing diagnostic must pass:

- rank-normalized R-hat at most 1.01;
- bulk and tail ESS at least 400;
- MCSE/SD at most 0.08; and
- the complete per-time, terminal, future-root, loss, learning-rate, and
  component-scale schema already used by the S10 guard.

The runner also records compact chain locations, first/second-half means,
autocorrelations at lags 1, 5, 10, 25, and 50, effective draws per second,
transition evaluation/shrink counts, target identities, job resources, and a
recursive artifact manifest. Full fits and scalar chains remain under ignored
`application/cache/` storage. No coverage, width-performance, or comparative
simulation result from this exercise is eligible for the article.

## Promotion sequence after the comparison

1. If no candidate passes all three cases, stop and retain the failure bundle.
   Do not expand the schedule automatically.
2. If one or more pass, choose the first candidate in the fixed order and
   produce a compact authenticated closeout.
3. Commit a new fail-closed implementation that makes the selected kernel the
   M11 policy. Build a fresh isolated runtime from that exact commit.
4. Rerun the complete S10 M10/M11 guard from a fresh ignored root.
5. Only after the guard passes, rerun the complete affected S05/S06 wave and
   all exact promotion gates from fresh roots.
6. Only after every promotion gate, package/native/TeX/literature check, and
   artifact audit passes may a separate flag-only authorization commit be
   considered.
7. The main simulation must always start under a new run ID. No failed or
   development chain is reusable as a scientific output.

## Operational correction

The failed coordinator stopped correctly but left a stale `running` status.
The recovery coordinators now write status atomically, record exact PID/PGID,
install EXIT/INT/TERM/HUP traps, and publish a terminal exit status. Health
checks use `/proc` and exact process-group membership instead of name-based
`pgrep`, preventing the query itself or an unrelated shell from appearing as a
live simulation.

The exdqlm and Q-DESN repositories remain protected and untouched.
