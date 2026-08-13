# MTI-ECM Monotonicity and Scope

Date: 2026-08-12
Status: implementation theorem support, not publication-level proof promotion

## Theorem Roles

`ECM-TARGET`
: Defines the fixed loss-based objective minimized by ECM. This is implemented
and audited in tests by comparing `mti_loss()` plus ridge penalties
with the stored ECM objective.

`ECM-MOMENT`
: Derives the inverse latent-scale moment
`E(V_i^{-1}|.) = 1/[q(1-q)|e_i|]` for nonzero residual product. This is
implemented and unit-tested. It is distinct from the VB latent mean.

`ECM-CM`
: Gives the two conditional Gaussian root systems. This is implemented through
the shared root-system helper and unit-tested against the explicit matrix
equations.

`ECM-MONO`
: Exact ECM ascent applies only away from zero residual products and without
finite safeguards. The implemented default is a safeguarded generalized ECM/MM
algorithm; therefore the code enforces exact observed-objective monotonicity by
backtracking and stores the trace. No unsafeguarded global convergence theorem
is claimed.

`ECM-MULTISTART`
: The objective is generally nonconvex in the two root blocks. The software
uses deterministic multi-start and selects the smallest exact objective among
completed starts. This is an implementation guarantee, not a global optimum
theorem.

`ECM-ACTION`
: ECM endpoints are fixed-target generalized-posterior modes. They are not
formal tolerance actions and do not inherit scan or split-spacing validity.

## Current Proof Status

| Result | Status | Repository role |
|---|---|---|
| Fixed-target objective definition | `IMPLEMENTED-AUDITED` | Determines the ECM loss and stored objective components. |
| Inverse GIG moment | `IMPLEMENTED-AUDITED` | Determines ECM weights. |
| Conditional root systems | `IMPLEMENTED-AUDITED` | Shared algebra for MCMC and ECM. |
| Safeguarded monotonicity | `IMPLEMENTED-AUDITED-COMPUTATIONAL` | Objective trace is checked after accepted cycles. |
| Global convergence | `NOT-CLAIMED` | Nonconvex/nonsmooth target; multi-start is diagnostic. |
| Tolerance validity of ECM endpoints | `REJECTED` | Formal tolerance action remains empirical order-statistic based. |
| VB equivalence | `REJECTED` | VB uses `E(V)`, ECM uses `E(V^{-1})`. |

## Manuscript Scope

Allowed wording:

```text
For fixed content, tilt, and learning rate, ECM computes a deterministic
generalized-posterior mode using exact inverse latent-scale moments and two
conditional Gaussian root solves.
```

Disallowed wording:

```text
ECM calibrates tolerance.
ECM is maximum-likelihood EM for a response model.
ECM intervals are posterior predictive intervals.
ECM and VB are equivalent.
```

## Remaining Work

- Prove or reject stronger nonsmooth/global convergence claims.
- Extend deterministic fitting to regression-family tolerance candidates only
  after a separate sample-splitting or finite-candidate calibration theorem.
- Add any future VB/CAVI only as a distinct approximation to the fixed
  generalized-posterior target.
