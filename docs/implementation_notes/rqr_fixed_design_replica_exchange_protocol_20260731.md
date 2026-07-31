# Exact replica exchange for fixed-design RQR

Date: 2026-07-31  
Scope: M03 fixed-design ridge RQR only  
Status: implementation and development-validation protocol; fail-closed

## Motivation

The frozen S03 replication 117 separates one overdispersed chain from the other
three under the current fixed-design Gibbs sampler. Burn-in of 3,000, retention
of 6,000, and two complete Gibbs kernels per recorded iteration all fail. The
failure is therefore not addressed by composing more copies of the same local
kernel. An exact mode-bridging transition is required before M03 can re-enter
promotion.

## Tempered targets

For the fixed learning rate `omega_R`, ridge prior density `p_0(beta_1,
beta_2)`, and total RQR loss `L(beta_1,beta_2)`, replica `j` targets

```text
pi_j(beta_1,beta_2) proportional to
  p_0(beta_1,beta_2) exp{-t_j omega_R L(beta_1,beta_2)},
```

where `1 = t_1 > t_2 > ... > t_J > 0`. The existing pseudo-AL Gibbs update at
learning rate `t_j omega_R` leaves this tempered marginal target invariant.
The response is not assigned an ordinary likelihood; tempering acts on the
generalized-Bayes loss update.

After every declared number of local transitions, alternating adjacent pairs
propose to exchange their beta states. For adjacent temperatures `t_i > t_k`
and loss energies `E(x) = omega_R L(x)`, the log Metropolis ratio is

```text
(t_i - t_k) {E(x_i) - E(x_k)}.
```

The common ridge prior cancels. Latent pseudo-AL scales are auxiliary: the swap
acts on the marginal beta states, and every following local transition begins
with a complete conditional latent refresh. Only the `t_1=1` cold replica is
retained. Hence the retained draws have the unchanged declared RQR target.

The first implementation is deliberately restricted to fixed-rate, zero-tilt,
ridge RQR with no stored latent draws. Learned loss scale, RHS priors, nonzero
mean tilt, dynamic state paths, and continuation require separate derivations
and are rejected at the public boundary.

## Frozen development comparison

The exact authorization seed ledger, S03 replication 117, guard replications
13, 90, and 185, and profiles A--D are fixed before the comparison. All
candidates use burn-in 500, 1,500 retained cold draws, one local Gibbs kernel
per replica per iteration, and adjacent swaps every iteration.

| Candidate | Inverse-temperature ladder | Replica cost per chain |
|---|---|---:|
| REX4 | 1, .45, .20, .09 | 8,000 |
| REX5 | 1, .55, .30, .165, .09 | 10,000 |
| REX6 | 1, .63, .40, .25, .16, .10 | 12,000 |
| REX8 | 1, .72, .52, .37, .27, .19, .13, .09 | 16,000 |

The cold replica uses the chain's original profile and reviewed ledger state.
Hot replicas use the deterministic repeating profile sequence A, C, D, B. The
single original method stream is consumed in a fixed serial order; no seed is
replaced or selected after seeing diagnostics.

A candidate is eligible only when every frozen hard and guard diagnostic passes
the unchanged sentinel thresholds, every adjacent edge is attempted and
accepted at least once in every fit, the cold position visits more than one
replica label, the exact target flag is true, and numerical repairs are zero.
Among eligible candidates, the minimum declared replica-transition cost wins,
with candidate order as the final tie breaker. Loss, coverage, width, or other
scientific performance is not a selection criterion.

If no ladder passes, the correction stops. The thresholds, starts, seeds, or
case set are not weakened, and the confirmatory run remains unauthorized.

## Required validation before integration

1. Disabled replica exchange is bitwise identical to the ordinary sampler.
2. Temperature and initialization contracts reject malformed input.
3. The swap ratio satisfies the forward/reverse detailed-balance identity.
4. Cold draws remain labeled as a fixed-joint exact generalized posterior.
5. Every swap edge has recorded attempts, accepts, and acceptance rates.
6. The full four-chain hard/guard comparison passes from clean committed
   source.
7. M08 is rerun separately after correcting its forecast-seed harness.
8. Any selected correction is applied uniformly to the declared M03 method
   role, budgeted by replicas times local transitions, and then subjected to a
   fresh complete wave-2 gate and exact isolated-runtime promotion.

No development output becomes scientific or promotion output.
