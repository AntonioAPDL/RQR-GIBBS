# Legacy local-artifact cleanup

## Scope

This audit records a conservative cleanup of ignored RQR-GIBBS run artifacts
on Jerez. It covers terminal, superseded, or explicitly nonpromotable local
outputs only. No tracked source, current manuscript evidence, protected
reference repository, active process, package runtime, or unrelated project
artifact was removed.

The cleanup manifest is
`docs/audits/legacy_local_artifact_cleanup_manifest_20260805.csv`. Its location
roots are:

- `PRIMARY`: `/data/muscat_data/jaguir26/RQR-GIBBS`;
- `PROCESS`: the external worktree
  `oracle_tilt_v3_process_isolation_20260801`; and
- `PROMOTION`: the external worktree
  `rqr_gibbs_main_promotion_20260728`.

## Promotion boundary

The manuscript continues to use the tracked version-2 illustration evidence
under `figures/data/oracle_tilt_c095_v2/`. That bundle represents source commit
`fec979f927c9039cf778ac09aef139ebd6761e8e`, all 27 completed chains, and six
strict-passing cells. It was not modified.

Version 3 was never promoted. Its exact baseline at
`99a088fbdd7c3f3ed18f99197294038f62dbfe41` had five strict cells and one
marginal DLM shortest-interval failure. The bounded adjudication at
`a3b39b394c6aa928eb38e9ed461281cdf743d00b` completed successfully as a
computational workflow but did not change that promotion decision:

| Adjudication gate | Result |
|---|---:|
| Completed chains | 5/5 |
| Retained draws | 60,000 total |
| Baseline-prefix comparisons | 15/15 bitwise identical |
| Numerical repairs | 0 |
| Hard-integrity gates | pass |
| Strict MCMC diagnostics | pass |
| Width-contrast relative error | 0.2026225 |
| Frozen maximum | 0.2000000 |
| Heterogeneity gate | fail |
| Final disposition | `descriptive_review_required` |
| Automatic promotion | false |

The additional draws therefore strengthened the conclusion that the remaining
gap is an illustration/model-fit limitation rather than Monte Carlo error. The
recovery protocol authorizes no further automatic execution. Compact CSV/JSON
summaries, prefix comparisons, diagnostics, resource telemetry, source/runtime
identity, and closeouts remain in the ignored PROCESS worktree; only its five
large worker objects were removed.

## Pre-deletion safety checks

Before deletion, the following checks passed for every manifest row:

1. the path existed as a directory and was not a symbolic link;
2. its canonical path was below one of the three declared local-output roots;
3. its observed byte count matched the frozen cleanup manifest;
4. Git reported zero tracked files and confirmed the path was ignored;
5. no RQR-GIBBS service, process, or open file remained; and
6. compact evidence or a tracked terminal-run audit existed where the raw
   object had scientific or operational relevance.

The stopped version-3 adjudication wrapper had status zero, an empty final
process group, no timeout or sampled resource-limit event, and a passing
wrapper closeout. Active NDLM processes belonged to a different project and
were excluded by path and process checks.

## Removed and retained material

The cleanup removed 31,189,630,976 bytes (29.05 GiB) across 15 exact targets.
The largest classes were superseded version-3 attempts, nonpromoted
version-3/adjudication worker objects, and worker payloads from the two
terminal confirmatory runs already covered by tracked closeout audits.

The following were deliberately retained:

- tracked version-1 and version-2 compact illustration evidence;
- compact version-3 and adjudication summaries, diagnostics, manifests, and
  closeouts;
- nonworker metadata and resource logs for terminal RQR-DLM runs;
- all current RQR-DLM candidate runs in external validation worktrees;
- source configurations, seeds, protocols, tests, and exact commit identities;
- isolated package runtimes and attestations; and
- all artifacts outside RQR-GIBBS.

After cleanup, the RQR-GIBBS local worktree collection fell from approximately
22.95 GB to 3.80 GB, the primary ignored output root from 9.99 GB to 1.80 GB,
and the primary ignored run root from 3.87 GB to 17.7 MB. Available `/data`
space increased from approximately 353 GB to 382 GB.

Removed raw objects are not recoverable locally. They can be regenerated from
their recorded commits, configurations, seeds, and runtime contracts if a
future scientific reason justifies the cost. Historical artifact manifests
that list pruned raw objects should now be read as immutable records of the
completed run, not as manifests that can still be reverified against every
local payload.

The adjudication health helper now reports completed chains from the immutable
closeout separately from retained worker artifacts. Consequently, intentional
post-closeout pruning cannot make a completed run appear unfinished.

## Preserved-evidence checksums

Selected compact evidence was hashed before cleanup:

| Object | SHA-256 |
|---|---|
| Version-2 tracked evidence receipt | `83bdf695e3053c5d3106d850bd95295b52e2ec3a048f655d4b3450b5316264d5` |
| Version-2 tracked closeout | `479d6ff0a99ffa1b9253b7b5cafdcb3ace792e1e0489dea5828a02d5448746ba` |
| Version-3 baseline source state | `7c1e3de29e8f70c36e90451c1616d08f88168aade6dc6c75c11b22f2e68bddf6` |
| Version-3 baseline wrapper manifest | `d7af04556b7350535a77b279eb488b13054f9397caf640f3a4d47fd85dd35849` |
| Adjudication source state | `7429d1b90381cab03dd51418c7989b6c815e2349a8e0c4b57f7f1c76b0be5712` |
| Adjudication fit summary | `2f5487688023c24e73a82cae6c18b082de1668d634148d6b231c89bf02eecbef` |
| Adjudication decision | `ce3eab006e273b135383180f3d5f73c69d9150b00f75a65734c5442dfbfe5b8d` |
| Adjudication prefix comparison | `0b43c7f386cb3eae631283369f1655e1e6cb8dc5e85d704488a1a7ed5cac4b0d` |
| Adjudication closeout | `7b768c7736cf65b6221aa03d6824c3f178deb0380114c89ae15c35a2cc3dbb10` |
| Adjudication wrapper closeout | `715eb3d661c1e8b5d003b160890500d275d248ee07fb10160cc1431b84a4924c` |
