# Pharmaceutical Source and Production Structure

The source analysis in `application/config/pharma_source_structure_20261026.json`
extends the description of the existing pharmaceutical application. It does
not change the adopted MTI procedure, its calibration, any response values,
the 1502 historical partitions, or the 9012 stored method evaluations.

## Source and Measurement Interpretation

The original laboratory file and the version-1 Figshare process archive are
verified by byte count and SHA256. Only `Process/23.csv` is extracted. Its
694893 process records give one campaign identifier for each of the 187
laboratory batches. The derived mapping and small descriptive summaries are
tracked; the archive and extracted process file stay under ignored cache.

The analysis records 36 Product-23 film-coated average-hardness values above
their recorded maxima and 55 core averages below their recorded minima. It
reproduces the tensile response from average hardness and maximum dimensions
using the source's numerical factor 3.14. These checks raise a definition
question; they do not establish which field should be changed. Every source
measurement remains intact. The physical units, measurement windows, and
comparability of the summaries await author clarification. The paper therefore
uses the reported tensile metric as a qualified historical illustration.

## Partition Interpretation

All 187 batches map to 28 campaigns. Every historical random partition shares
campaigns between training and held-out batches. A separate retrospective
summary retains all 63 existing month-grouped partitions with no shared
campaign, alongside the full 250 month partitions. Of those 63 partitions,
58 still share API lots. Neither shared identifiers nor their separation proves
dependence or independence. The stored results are reused without refitting.
Width summaries, finite-data content, and omissions are reported together.
The held-out attainment fractions do not estimate tolerance confidence.

Forward/reverse labels now refer to recorded batch order. Ambiguous source
date strings do not establish calendar chronology. The old internal scheme
identifiers remain unchanged so historical memberships reproduce exactly.

## Prepared, Unevaluated Design

Graph components identify batches linked by a chosen set of production
identifiers. Keeping components intact allows 15868 distinct n=100 training
sets when separating both campaigns and API lots. The existing exact
whole-group sampler prepares 250 unique partitions with seed 202610260.
The manifest explicitly marks them as not fitted while response definitions
remain unresolved. No fitting option exists in the source-analysis command.

Requiring separation of campaigns and every recorded material lot leaves four
components of 36, 42, 48, and 61 batches. No subset sums to 100. The implementation
rejects impossible allocations rather than splitting groups or substituting a
nearby calibration setting. Future use needs a declared batch- or campaign-level
population and explicit remaining-dependence limitations.

## Reproduction

With public downloads explicitly enabled:

```bash
make pharma-source-prepare PHARMA_SOURCE_DOWNLOAD=true
make test-pharma-source
```

Alternatively, set `PHARMA_SOURCE_LAB` and `PHARMA_SOURCE_ARCHIVE` to verified
local original files. No network access is needed to regenerate tables or
verify the committed small evidence:

```bash
make pharma-source-tables
make verify-pharma-source
```

`verify-pharma-source` checks artifact hashes and regenerates tables in a
temporary directory to compare against committed outputs. It does not rewrite
the manuscript tables. `theory-tables` includes the new descriptive table and
prose numbers. Tests cover malformed joins, missing/duplicate identifiers,
outcome-independent grouping, exact sizes, shared identifiers, impossible
allocations, unchanged historical evidence, nonreturn accounting, and corrupt
source/evidence rejection.

## Remaining Scientific Questions

Author inquiries are prepared locally and have not been sent. A confirmed
correction would require a new source/evidence version and a focused impact
assessment. An unresolved definition should lead to an explicitly descriptive
or shortened application, not automatic numerical repair. No new simulation
study, method tuning, or needle-shield small-sample extension is authorized
by this source-analysis update.
