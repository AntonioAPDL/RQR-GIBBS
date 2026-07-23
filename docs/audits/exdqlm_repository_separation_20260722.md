# exdqlm repository-separation hardening

Date: 2026-07-22

## Scope

This audit freezes the boundary between the standalone RQR-GIBBS project and
all exdqlm publication, CRAN, feature-branch, and local worktree activity. The
new RQR-DLM implementation belongs to RQR-GIBBS. exdqlm supplies a pinned
historical implementation reference for RQR-DESN and RHS-family adapters; it is
not a writable implementation target for this project.

## Required boundary

- No RQR command may edit, clean, build, install, commit, checkout, fetch, pull,
  or push an exdqlm repository.
- The pinned checkout must be on
  `feature/rqr-desn-readout-20260716` at
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
- External Git inspection runs with `GIT_OPTIONAL_LOCKS=0`.
- Compilation and installation occur only under the ignored
  `application/cache/exdqlm_runtime/` tree.
- Runtime-backed RQR-DESN and RHS adapters require an isolated runtime
  attestation. A namespace loaded directly from the source checkout is not
  reproducibility-eligible.

## Implementation

`application/scripts/lib/pinned_exdqlm_runtime.R` provides the shared boundary
guard. It records the branch, commit, Git tree, status, refs, local Git
configuration, and a content-and-metadata manifest of every working-tree entry
outside `.git`. The manifest includes ignored files, so a rebuilt object or
shared library is detectable even when ordinary `git status` remains empty.

`application/scripts/04_prepare_pinned_exdqlm_runtime.R`:

1. captures the protected-checkout guard;
2. creates `git archive` output in the RQR-owned cache;
3. extracts, builds, and installs only inside that cache;
4. verifies that the checkout guard is unchanged;
5. writes a version-2 runtime attestation binding the source commit/tree,
   checkout guard, archive path/checksum, runtime path, and installed-tree
   digest.

`application/scripts/02_smoke_rqr_exdqlm_branch.R` no longer calls
`pkgload::load_all()` on exdqlm. It selects the isolated library before loading
the namespace, verifies the version-2 attestation, extracts the focused tests
from the attested archive into a temporary cache directory, runs them there,
and verifies the checkout guard again.

The RQR package provenance contract sets `require_isolated_runtime = TRUE`
whenever an adapter declares a runtime package. Consequently, direct
source-path binding cannot make an RQR-DESN or RHS-backed fit reproducibility-
or promotion-eligible. The adapter entry points additionally refuse a loaded
namespace whose package path contains Git checkout metadata, preventing RQR
from executing the adapter through `pkgload::load_all()` on exdqlm.

## Authoritative CRAN source

CRAN exdqlm 1.1.0 remains an external reference. This hardening neither vendors
nor modifies that release, and it does not update any exdqlm main, CRAN, or
publication branch. Interface comparison should use an official source archive
extracted into a temporary or RQR-ignored workspace.

## Validation gates

The change is complete only if:

- native provenance tests reject a tampered source archive and a tampered
  installed-package digest;
- `make prepare-exdqlm-runtime` reports an unchanged protected checkout;
- `make test-exdqlm-rqr` passes from the archive-attested runtime;
- native tests and package checks pass; and
- before/after external branch, commit, tracked status, and full checkout guard
  are identical.
