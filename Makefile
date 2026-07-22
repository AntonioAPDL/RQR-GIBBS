R ?= Rscript
PDFLATEX ?= pdflatex
BIBTEX ?= bibtex
LATEXMK ?= latexmk

.PHONY: pdf supplement all-pdf smoke test-exdqlm-rqr literature-manifest clean-tex

pdf:
	@if command -v $(LATEXMK) >/dev/null 2>&1; then \
		$(LATEXMK) -pdf -interaction=nonstopmode main.tex; \
	else \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(BIBTEX) main || true; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
	fi

supplement:
	@if command -v $(LATEXMK) >/dev/null 2>&1; then \
		$(LATEXMK) -pdf -interaction=nonstopmode rqr-gibbs-supplement.tex; \
	else \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
		$(BIBTEX) rqr-gibbs-supplement || true; \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
	fi

all-pdf: pdf supplement

smoke:
	$(R) application/scripts/00_validate_environment.R

test-exdqlm-rqr:
	$(R) application/scripts/02_smoke_rqr_exdqlm_branch.R

literature-manifest:
	$(R) application/scripts/01_build_literature_manifest.R

clean-tex:
	rm -f *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.synctex.gz *.toc *.run.xml *.bcf

