# Independent V4 preauthorization review request

Use read-only access to the public repository and review branch supplied with
this request. Do not modify or push to RQR-GIBBS, exdqlm, or the Q-DESN article
repository.

## Exact source boundary

The authoritative scientific source is:

```text
repository: AntonioAPDL/RQR-GIBBS
commit:     4b38e6a556e6848c7ed6e077dad0d22a00367382
```

The review branch adds only this compact packet. Verify that its parent is the
authoritative source commit and that it does not change scientific source,
configuration, manuscript, or execution code.

Read completely:

```text
docs/implementation_notes/oracle_tilt_c095_v4_seed_screen_protocol_20260805.md
docs/audits/oracle_tilt_c095_v4_prelaunch_hardening_20260806.md
docs/audits/oracle_tilt_c095_v4_preauthorization_review_20260807/README.md
docs/audits/oracle_tilt_c095_v4_preauthorization_review_20260807/preauthorization_reconciliation.md
docs/audits/oracle_tilt_c095_v4_preauthorization_review_20260807/preauthorization_validation_matrix.csv
```

Treat repository reports, closeouts, and the reconciliation as claims to
audit, not proof by assertion.

## Required audit

Audit at least:

1. the generalized-Bayes interval-root interpretation and prohibited response-
   predictive or typical-performance claims;
2. exact population-oracle tilt construction and absence of Cornish--Fisher
   substitution in the campaign;
3. target-shared data, prospective candidate streams, seed uniqueness, and
   family-level selection without realized-content or aesthetic ranking;
4. fixed-design and DLM target correctness, priors, initialization, MCMC
   schedules, and the special DLM/SH retained-draw schedule;
5. source/runtime/config binding and the isolated-runtime acceptance boundary;
6. runner and wrapper manifest completeness and downstream verification;
7. heavy-process exclusion for relative and absolute V4 script paths;
8. preflight and reference gate completeness;
9. benchmark numerical, recovery, resource, and interpretation evidence;
10. the 18-process full-scale resource topology, endpoint-only storage,
    resource headroom, logs, lifecycle, and final process-group state;
11. the fact that the compact packet omits 15 GiB of synthetic arrays and thus
    does not let the reviewer independently rehash those raw bytes;
12. fail-closed production authorization and the requirement to regenerate all
    exact-bound prerequisites after the flag-only commit;
13. production diagnostics, failure/stop semantics, resumability, selector
    replay, evidence packaging, and absence of automatic manuscript promotion.

## Required decision

Return exactly one of:

```text
FLAG-ONLY AUTHORIZATION: GO
```

or

```text
FLAG-ONLY AUTHORIZATION: NO-GO
```

A GO means only that a later commit may change
`execution_authorized=false` to `true`. It does not authorize reusing the old
bundles, skipping postauthorization validation, promoting a candidate, or
making manuscript claims.

## Deliverables

Return one ZIP containing:

- a complete audit report;
- a concise implementation handoff;
- a CSV finding-disposition table;
- a machine-readable launch-decision JSON;
- an internal artifact-hash manifest for those deliverables.

Report the outer ZIP SHA-256 separately. Do not include fitted objects, package
runtimes, generated PDFs, or raw simulation/rehearsal arrays.

