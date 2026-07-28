# arXiv preprint submission checklist

Date: 2026-07-27

This note records the current source-preparation contract for the standalone
RQR-GIBBS preprint. It is intentionally limited to manuscript and arXiv
packaging hygiene; it does not authorize or summarize any in-progress
confirmatory simulation.

## Recommended arXiv metadata

- **Primary category:** `stat.ME` is the best current fit because the article
  develops statistical methodology for interval functionals, generalized
  Bayes computation, regularized regression, and dynamic time-series roots.
- **Possible cross-list:** `stat.CO` if the submission emphasizes the Gibbs,
  FFBS, and reproducible-computation contribution; `stat.TH` if the final
  abstract emphasizes the population characterization and Bayesian inference
  theory more heavily.
- **License:** choose only after confirming advisor and target-journal
  preferences. `CC BY` maximizes reuse; the arXiv perpetual non-exclusive
  license is the conservative option when journal policy is uncertain.

## Source package

The main article uses `pdflatex`, `natbib`, and tracked PNG figures. The source
package should include `main.tex`, `main.bbl`, `refs.bib`, the table input, and
the three main-text PNG figures. It should not include local PDFs, logs, aux
files, caches, fitted models, or simulation outputs.

Build and package the main source with:

```bash
make arxiv-source
```

The resulting zip and SHA-256 file are written under ignored
`application/cache/arxiv_preprint_<stamp>/`.

The supplement currently builds as a separate TeX document. If the supplement
is posted with the arXiv submission, upload it as a separate source/ancillary
item or use Overleaf's arXiv export after verifying that the generated
supplement PDF and source correspond to the same Git commit.

## Pre-submission checks

Run these before uploading:

```bash
make test-theory-figures
make test-theory-tables
make pdf
make supplement
make arxiv-source
```

Then inspect:

- no undefined references or citations in `main.log`;
- no missing included graphics or table inputs;
- `main.bbl` exists and corresponds to the current `refs.bib`;
- all included figures use relative paths and are present in the source zip;
- the rendered PDF uses the fixed July 2026 date rather than `\today`;
- the article does not report unapproved simulation results or imply a
  response-likelihood/posterior-predictive interpretation.

## Current evidence language

For the pre-results preprint, the safe manuscript stance is:

1. population theory and loss geometry are stated as mathematical results;
2. ordinary-RQR implementation status is stated exactly;
3. nonzero-tilt Gibbs, VB/CAVI, data-driven tilt selection, and matched
   nonlinear-readout evidence remain future methodological work;
4. the RQR-DLM confirmatory simulation, while running or under audit, should
   not be summarized as scientific evidence until its fail-closed gates pass
   and the resulting compact artifacts are committed.
