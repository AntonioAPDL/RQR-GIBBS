# RQR-DLM skewed candidate preflight correction

The first development-only candidate comparison launched from commit
`4a2f3c51ad0f52a48a19614e88da44f177580520` was stopped during its first
health check.  It is not statistical or promotion evidence.

The invalid plan assigned four-chain sentinel roles to passing guard
replications that were not selected as sentinels in the reviewed maximum seed
ledger.  Twelve of 93 planned jobs published failure records, all with an
`Unknown RNG task key` error for chains 2--4.  No job succeeded, no comparison
manifest was created, and the remaining 81 jobs were terminated by stopping
the detached process group.  The ignored output and log roots are retained
locally only as operational evidence and will not be resumed or reused.

The correction has three parts.

1. Four-chain guards are restricted to method--DGP--replication combinations
   in the frozen maximum sentinel map.  They retain their original reviewed
   chain streams.
2. Candidate preflight now enumerates every method and forecast RNG task key
   required by every job and rejects the plan unless all keys exist in the
   exact reviewed seed ledger.  It also regenerates every fixed DGP case during
   preflight to validate its DGP streams without fitting a model.
3. The guard-selection description now distinguishes a fully passing guard
   from a method-passing seeded guard.  This is necessary for M09, whose only
   executed S05 sentinel guard passed M09 but shared a replication task with a
   failure in another method.

The relaunch must use a fresh ignored output root and a new clean source
commit.  Thresholds, method targets, diagnostic draw counts, hard cases,
seeds, and the confirmatory authorization flag remain unchanged.
