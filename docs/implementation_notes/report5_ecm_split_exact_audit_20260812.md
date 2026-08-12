# Report5 ECM/Split-Exact Audit

Date: 2026-08-12
Local report: `report5.md`
Report SHA-256: `65fbc98f64d255c7813ac76e230ec95cb11b825ae22081f7a89ebcee5d5cf2da`
Implementation branch: `feature/mt-rqr-ecm-split-exact-tcsp-20260812`
Starting local HEAD: `4ca59085630ead755f5f09f8013dc287a993f71e`

## Repository Diagnosis

The repository already separated MT-RQR generalized posteriors from response
likelihoods and already treated the TCSP empirical shortest window as the
formal tolerance action. The report5 additions were therefore integrated as
new computation and validation layers, not as a rewrite of the statistical
target.

The decisive finding from report5 was that the next deterministic engine should
be ECM, not a renamed VB approximation. The current VB code uses a latent
first moment `E(V)`. The derived ECM root systems require the inverse moment
`E(V^{-1})`. Those two algorithms optimize/approximate different objects and
must remain separately labeled.

## Accepted Changes

- Add fixed-target MT-RQR-ECM for ridge/Gaussian fixed designs.
- Factor shared root Gaussian algebra so MCMC draw mode and ECM inverse-moment
  mode are tested against the same equations.
- Add explicit safeguards for zero residual products and exact
  observed-objective backtracking.
- Add deterministic multi-start and selected-start objective accounting.
- Extend TCSP univariate fitting with optional `fit_ecm` while preserving the
  empirical formal action.
- Change full-range `q=1` wrapper behavior to return the empirical range action
  with ECM/MCMC unavailable reasons instead of throwing away the action.
- Add pilot-selected exact-spacing TCSP with independent pilot placement and
  main-sample Beta spacing calibration.
- Add smoke validation scripts/configs for ECM and split exact-spacing.
- Add documentation, theorem-scope notes, and manuscript wording.

## Deferred

- Exact scan recursion.
- Same-sample shortest-window plug-in stability.
- Posterior/ECM action transfer.
- Regression-family tolerance calibration.
- RHS-NS, learned-rate, dynamic, adaptive, component-scale, and VB/CAVI ECM.
- Heavy/full validation launch.

## Rejected Interpretations

- ECM endpoints are tolerance intervals.
- ECM is response-likelihood EM.
- ECM and current VB are equivalent.
- Pilot ECM beats empirical sorting for an intercept-only pilot by default.
- The split exact-spacing construction proves conditional regression tolerance.

## Validation Plan Update

The next executable stage is not a full validation launch. It is:

```bash
make test-ecm
make test-tcsp
make rqr-ecm-validation-smoke
make tcsp-split-exact-validation-smoke
```

The moderate pilots in the new configs may be run after smoke artifacts are
reviewed. Full-pilot or confirmatory execution remains blocked until explicitly
authorized.
