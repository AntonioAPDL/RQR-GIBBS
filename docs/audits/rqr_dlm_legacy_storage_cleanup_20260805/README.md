# RQR-DLM legacy local-storage cleanup

## Scope and decision

This audit records a selective cleanup of ignored RQR-DLM computational
artifacts on Jerez. The cleanup covered only terminal failed runs,
superseded development comparisons, superseded validation caches, and their
duplicate preflight bundles. None of these artifacts was eligible for
scientific promotion or continuation. Existing tracked failure closeouts,
diagnostic summaries, Git source, configurations, and scripts remain the
authoritative compact record.

The cleanup did not alter the generalized-Bayes target, source code,
configuration, seeds, thresholds, manuscript, or any protected repository.
No tracked file was removed. The deleted bytes are not recoverable locally;
they can be regenerated only by rerunning the corresponding committed source
and frozen contracts.

## Safety boundary

Before deletion, the audit required:

- no live R or Rscript process whose command referenced `rqr_dlm`;
- no open handle under a proposed RQR-DLM deletion scope;
- zero tracked files below every proposed root;
- canonical absolute paths below an explicit allowlist;
- exclusion of the completed joint-elliptical recovery bundle;
- exclusion of the reviewed maximum seed-ledger checkout;
- exclusion of the attested exdqlm CRAN 1.1.0 runtime; and
- exclusion of every oracle/static-study directory and protected repository.

The protected joint-transition bundle authenticated 54 of 54 manifest
entries before and after cleanup. Its recursive artifact-manifest SHA-256 is
`6551e96f940dd0ddccf6ea3adc6e42902162cf3d288fd802a1c911fc53679b02`.
The protected maximum seed ledger retained SHA-256
`3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`,
and the protected exdqlm runtime attestation retained SHA-256
`5697dc0b4f06dc2d346c4580b3e16d096780529cee0d9fe430bcf55afd2d98c9`.

## Disposition

The initial inventory contained 130 ignored legacy roots, 7,066 files, and
9,697,296,384 allocated bytes (9.031 GiB). A concurrent workspace cleanup was
detected because three roots changed after inventory; the deletion guard
stopped before acting. That independent change had already reduced the three
roots by 4,126,527,488 allocated bytes while retaining their compact files.

After reconciliation, this pass hashed and removed exactly 299 remaining
heavy files:

- 279 obsolete RDS chain/evidence files totaling approximately 3.062 GiB;
- 20 duplicate legacy `seed_ledger_maximum.csv` files totaling approximately
  1.632 GiB; and
- 5,040,242,626 logical bytes in total.

The filesystem reported 5,040,930,816 additional available bytes across the
selective deletion. The 130 legacy roots now occupy 529,838,080 allocated
bytes (0.493 GiB) and retain 6,318 compact files. No file matching the cleanup
rule remains under those roots. Relative to the initial inventory, the roots
were reduced by 9,167,458,304 allocated bytes (8.537 GiB), including the
independently detected prior cleanup.

`deleted_file_ledger.csv` records the exact path, logical byte count, and
SHA-256 of every file removed by this pass. `root_ledger.csv` records every
audited root, its initial, reconciled, and final allocation, its retained file
count, and its disposition. `artifact_hashes.csv` authenticates this compact
cleanup record.

The retained historical artifact manifests may name heavy files that are no
longer present. They remain records of the original completed bundles but are
not claims that those raw bundles remain locally rehashable. This cleanup
ledger is the explicit local-availability supersession record.

## Protected current RQR-DLM state

The joint-elliptical comparison at source commit
`2901770ec25fb6042cbc2c8227478a31bdb0dc1a` completed 44 of 44 jobs, with all
fits successful, the exact target retained, zero numerical repairs, no retry
or reseeding, and both M10 and M11 obtaining an eligible predeclared
candidate. Its 118 MB ignored bundle remains intact because it is the direct
input to the next compact closeout and affected-wave integration. The launch
authorization flag remains false, and no 8,400-task main simulation was
started by this cleanup.
