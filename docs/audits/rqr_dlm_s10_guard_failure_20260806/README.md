# Failed S10 multicomponent guard closeout

The fail-closed guard used implementation commit
`73f9918deb91539f06ced88c7803877a3065f42f`. All eight fits completed,
targeted the declared exact joint distribution, used zero numerical repairs,
and remained below the 1.5-GiB worker ceiling. All 14 input artifacts and
108,816,763 input bytes were authenticated.

M10 passed all 47 diagnostics. M11 passed 32 of 48 diagnostics. Its 16
failures were led by the regression-component scale `log_q_2` (rank-normalized
R-hat 1.1616, bulk ESS 4.2, and tail ESS 22.1) and propagated to interval width,
upper-root, loss, and future-root summaries. The four chains separated into
two stable location groups, and late-window diagnostics did not resolve the
separation. This is not a numerical-repair or resource failure.

The conditional S05/S06 affected wave did not start. No result in this bundle
is reusable as a scientific simulation output, and the main launch remains
unauthorized.

The coordinator stopped at the failed guard, as intended, but its status file
remained `running` because the first coordinator schema lacked a terminal
error trap. The recovery implementation must correct that operational status
defect before another background launch.

Heavy per-chain RDS objects remain ignored. This directory contains only the
compact failure evidence, late-window diagnostics, job/resource status, and
the complete input/output hash lineage needed for review.
