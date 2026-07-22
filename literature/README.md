# Literature Workspace

Put local PDFs in `literature/pdfs/`. That directory is ignored by git.

Run:

```bash
make literature-manifest
```

to create local checksum files:

```text
literature/SHA256SUMS
literature/pdf_manifest.csv
```

Commit BibTeX entries in `refs.bib` only when the manuscript cites them.

