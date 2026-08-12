# TCSP-MT-RQR Proof Ledger

Prepared: 2026-08-11

Scope: scan-calibrated tolerance-calibrated shortest-path MT-RQR (TCSP-MT-RQR)
for the current branch-level manuscript revision. Tests and software contracts
do not by themselves promote a theorem.

Allowed statuses:

- `PROVED-AND-AUDITED`
- `DERIVED-PENDING`
- `IMPLEMENTED-AUDIT-PENDING`
- `BLOCKING`
- `LATER`
- `REJECTED`

| ID | Claim | Status | Current repository position |
|---|---|---|---|
| T-ACTION | Software action matches the theorem action: the first global minimum-width closed order-statistic window \([Y_{(j)},Y_{(j+k-1)}]\). | `IMPLEMENTED-AUDIT-PENDING` | `rqr_tcsp_shortest_window()` scans integer closed windows directly, uses a deterministic first-tie rule, and tests brute-force agreement. |
| T-SCAN-1 | Finite-sample scan theorem for \(k_{n,c,\alpha}\) under the canonical closed window \([Y_{(j)},Y_{(j+k-1)}]\). | `BLOCKING` | Definition is in the manuscript and software; proof/audit is not promoted. |
| T-SCAN-2 | Critical-count numerical certification. | `IMPLEMENTED-AUDIT-PENDING` | Monte Carlo uses a simultaneous Massart-DKW empirical-CDF lower band over the simulated Uniform scan-statistic distribution; the separate DKW fallback is conservative but not exact scan recursion. |
| T-FEAS | Feasibility and boundary-count handling for \(k\le n\), including the \(q=1\) empirical range action and posterior infeasibility at \(q=1\). | `IMPLEMENTED-AUDIT-PENDING` | Infeasible retained counts fail closed; \(q=1\) remains an empirical action but the MT-RQR sampler is rejected before calling `rqr_mcmc_fit()`. The q=1 error condition now preserves the empirical action object. |
| T-OPT-1 | Exact sample-wise width optimality in the scan-certified class. | `IMPLEMENTED-AUDIT-PENDING` | Integer action minimizes empirical width among closed \(k\)-observation windows. This is class-restricted and not global optimality over all tolerance procedures. |
| T-CT-1 | Full content-tilt coordinate theorem for arbitrary \(q\): existence, uniqueness of tilt coordinate under admissibility, boundary cases, and affine equivariance. | `DERIVED-PENDING` | Fixed-content quantile-window identification is in the manuscript/supplement. The arbitrary-\(q\) coordinate surface and boundary audit remain pending. |
| T-CT-2 | Expansion closure, containment wedge, and nested content path conditions. | `DERIVED-PENDING` | Used for path interpretation and continuation diagnostics, but not promoted as a completed theorem. |
| T-SH-1 | Shortest-path existence, uniqueness, and nesting. | `DERIVED-PENDING` | Population geometry is described with proof gates; nonunique windows remain serialized as ties. |
| T-SH-2 | Endpoint, width, and tilt derivatives. | `DERIVED-PENDING` | Used only as continuation guidance and diagnostics. |
| T-FAIL | Boundary, atom, transformation, and multimodal counterexamples. | `DERIVED-PENDING` | The manuscript states scope limits: ties/atoms, response-scale dependence, nonunique shortest windows, and disconnected highest-density regions require separate handling. A unified counterexample appendix is not complete. |
| T-MIXED | Shortest-location mixed rates. | `LATER` | No theorem or simulation claim promoted. |
| T-CONTENT-IF | Content influence function. | `LATER` | Not part of the current manuscript claims. |
| T-ORTH | Shortest-selection content orthogonality. | `LATER` | Not part of the current manuscript claims. |
| T-QASY | Asymptotic content target. | `LATER` | Not promoted beyond initialization/diagnostic discussion. |
| T-ORACLE-CAL | Selected-action tolerance-oracle well-posedness and calibration consistency for the reported action \(A_{n,q}\). | `LATER` | Not claimed. Any selected-action oracle used in validation is an empirical simulation reference, not a proved finite-sample universal calibration rule. |
| T-WIDTH | First-order width price for content buffer or tilt error. | `DERIVED-PENDING` | Mentioned only qualitatively with proof gates. |
| T-OPT-2 | First-order MT-RQR optimality for the shortest member of the content-tilt surface. | `DERIVED-PENDING` | Existing MT-RQR geometry supports the target, but tolerance promotion is pending. |
| T-REGRET | Width regret relative to the selected-action tolerance oracle. | `LATER` | Validation may report oracle width ratios descriptively; no oracle-regret theorem is promoted. |
| T-SCAN-ASY | Scan-buffer asymptotics and the scan uniformity premium. | `LATER` | Current scan calibration is conservative; no first-order scan-oracle efficiency claim is made. |
| T-GB-PROP | Fixed-target generalized-posterior propriety for every enabled TCSP posterior branch. | `DERIVED-PENDING` | Proper Gaussian/ridge fixed-rate branches are supported by current arguments; broader priors and learned-rate tilted paths remain gated. |
| T-GB-PLUGIN | Same-sample shortest-tilt plug-in stability for generalized posteriors. | `LATER` | Fixed-target theory treats \(q,\delta\) as frozen. Same-sample shortest-tilt posterior uncertainty is conditional/descriptive until this theorem is proved or a sample-split alternative is added. |
| T-LOCAL | Continuation correctness. | `IMPLEMENTED-AUDIT-PENDING` | Local path diagnostics are recorded, and every formal action is globally verified. |
| T-GB-ACTION | Posterior action equivalence. | `BLOCKING` | Posterior mean/median endpoints are not claimed to inherit exact scan validity. |
| T-SANDWICH | Endpoint uncertainty after fixed target selection. | `DERIVED-PENDING` | Existing static uncertainty scope applies only under its stated iid fixed-dimensional conditions. |
| T-REG | Finite-family marginal regression tolerance. | `LATER` | No regression-family tolerance theorem is promoted. |
| T-COND-SCOPE | Conditional-tolerance limitation for regression claims. | `LATER` | The manuscript prohibits pointwise conditional tolerance language from one marginal calibration rule. |

Current canonical action:

\[
[Y_{(\widehat j)},Y_{(\widehat j+k-1)}],\qquad
\widehat j=\min\argmin_j \{Y_{(j+k-1)}-Y_{(j)}\}.
\]

Current unsupported interpretations:

- posterior credibility equals tolerance confidence;
- \(\omega\) is a tolerance factor;
- posterior mean endpoints inherit exact scan validity;
- interval-root draws are posterior-predictive response draws;
- the Monte Carlo scan calibration is exact.

Current claim gates:

- Finite-sample distribution-free univariate tolerance wording requires
  `T-ACTION`, `T-SCAN-1`, `T-SCAN-2`, and `T-FEAS`.
- Minimum-width wording is currently limited to the realized empirical
  scan-certified class and depends on `T-OPT-1`; global minimum-width
  tolerance-procedure wording is rejected.
- First-order asymptotic tolerance wording requires `T-MIXED`,
  `T-CONTENT-IF`, `T-ORTH`, and `T-QASY`.
- First-order minimum-width/oracle wording additionally requires
  `T-WIDTH`, `T-OPT-2`, `T-ORACLE-CAL`, `T-REGRET`, and where scan is used as
  the candidate action, `T-SCAN-ASY`.
- Generalized-posterior endpoint uncertainty may be reported only as
  fixed-target or conditional descriptive uncertainty unless `T-GB-PROP`,
  `T-GB-PLUGIN`, and `T-SANDWICH` are completed. A posterior mean or median
  interval cannot replace the empirical tolerance action without
  `T-GB-ACTION` or direct action calibration.
