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
| T-SCAN-1 | Finite-sample scan theorem for \(k_{n,c,\alpha}\) under the canonical closed window \([Y_{(j)},Y_{(j+k-1)}]\). | `BLOCKING` | Definition is in the manuscript and software; proof/audit is not promoted. |
| T-SCAN-2 | Critical-count numerical certification. | `IMPLEMENTED-AUDIT-PENDING` | Monte Carlo uses a one-sided Clopper-Pearson lower bound; DKW fallback is conservative but not exact scan recursion. |
| T-ACTION | Software action matches the theorem action. | `IMPLEMENTED-AUDIT-PENDING` | `rqr_tcsp_shortest_window()` scans integer closed windows directly and tests brute-force agreement. |
| T-SH-1 | Shortest-path existence, uniqueness, and nesting. | `DERIVED-PENDING` | Population geometry is described with proof gates; nonunique windows remain serialized as ties. |
| T-SH-2 | Endpoint, width, and tilt derivatives. | `DERIVED-PENDING` | Used only as continuation guidance and diagnostics. |
| T-OPT-1 | Exact scan-class width optimality. | `IMPLEMENTED-AUDIT-PENDING` | Integer action minimizes empirical width among closed \(k\)-observation windows. |
| T-MIXED | Shortest-location mixed rates. | `LATER` | No theorem or simulation claim promoted. |
| T-CONTENT-IF | Content influence function. | `LATER` | Not part of the current manuscript claims. |
| T-ORTH | Shortest-selection content orthogonality. | `LATER` | Not part of the current manuscript claims. |
| T-QASY | Asymptotic content target. | `LATER` | Not promoted beyond initialization/diagnostic discussion. |
| T-WIDTH | First-order width price for content buffer or tilt error. | `DERIVED-PENDING` | Mentioned only qualitatively with proof gates. |
| T-OPT-2 | First-order MT-RQR optimality for the shortest member of the content-tilt surface. | `DERIVED-PENDING` | Existing MT-RQR geometry supports the target, but tolerance promotion is pending. |
| T-LOCAL | Continuation correctness. | `IMPLEMENTED-AUDIT-PENDING` | Local path diagnostics are recorded, and every formal action is globally verified. |
| T-GB-ACTION | Posterior action equivalence. | `BLOCKING` | Posterior mean/median endpoints are not claimed to inherit exact scan validity. |
| T-SANDWICH | Endpoint uncertainty after fixed target selection. | `DERIVED-PENDING` | Existing static uncertainty scope applies only under its stated iid fixed-dimensional conditions. |
| T-REG | Finite-family marginal regression tolerance. | `LATER` | No regression-family tolerance theorem is promoted. |

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
