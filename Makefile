R ?= Rscript
PDFLATEX ?= pdflatex
BIBTEX ?= bibtex
LATEXMK ?= latexmk

.PHONY: pdf supplement all-pdf smoke package-install prepare-primary-runtime prepare-exdqlm-runtime test-native package-check test-exdqlm-rqr bounded-pilot preflight-dlm-bounded reference-dlm-bounded test-dlm-monitor benchmark-dlm-bounded-one-cell execute-dlm-bounded literature-manifest clean-tex

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

package-install:
	R CMD INSTALL --preclean application

prepare-exdqlm-runtime:
	$(R) application/scripts/04_prepare_pinned_exdqlm_runtime.R

prepare-primary-runtime:
	$(R) application/scripts/04_prepare_primary_runtime.R

test-native: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native", reporter = "summary")'

package-check:
	R CMD build application
	R CMD check --no-manual rqrgibbs_0.1.0.9012.tar.gz

test-exdqlm-rqr: package-install prepare-exdqlm-runtime
	$(R) application/scripts/02_smoke_rqr_exdqlm_branch.R

bounded-pilot: prepare-primary-runtime prepare-exdqlm-runtime
	OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
	VECLIB_MAXIMUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
	$(R) application/scripts/05_run_rqr_bounded_pilot.R

preflight-dlm-bounded: prepare-primary-runtime
	application/scripts/08_run_rqr_dlm_bounded_validation.sh preflight

reference-dlm-bounded: prepare-primary-runtime
	application/scripts/08_run_rqr_dlm_bounded_validation.sh reference-only

test-dlm-monitor:
	application/scripts/10_test_rqr_dlm_monitor_wrapper.sh

benchmark-dlm-bounded-one-cell: prepare-primary-runtime
	application/scripts/08_run_rqr_dlm_bounded_validation.sh benchmark-one-cell

execute-dlm-bounded: prepare-primary-runtime
	application/scripts/08_run_rqr_dlm_bounded_validation.sh execute-bounded

literature-manifest:
	$(R) application/scripts/01_build_literature_manifest.R

clean-tex:
	rm -f *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.synctex.gz *.toc *.run.xml *.bcf
