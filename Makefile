R ?= Rscript
PDFLATEX ?= pdflatex
BIBTEX ?= bibtex
LATEXMK ?= latexmk
PACKAGE_NAME := $(shell sed -n 's/^Package:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_VERSION := $(shell sed -n 's/^Version:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_TARBALL := $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz

.PHONY: pdf supplement all-pdf smoke package-install prepare-primary-runtime prepare-exdqlm-runtime prepare-exdqlm-cran-runtime prepare-quantreg-cran-runtime test-native test-standalone-contracts package-check test-exdqlm-rqr bounded-pilot preflight-dlm-bounded reference-dlm-bounded test-dlm-monitor benchmark-dlm-bounded-one-cell execute-dlm-bounded preflight-dlm-main oracle-reference-dlm-main tiny-end-to-end-dlm-main diagnostic-pilot-preflight-dlm-main preflight-dlm-confirmatory oracle-reference-dlm-confirmatory failclosed-dlm-confirmatory failclosed-dlm-confirmatory-wave test-dlm-confirmatory-monitor literature-manifest clean-tex

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

test-standalone-contracts: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "dlm-bounded|dlm-main|dlm-confirmatory", reporter = "summary")'

package-check:
	R CMD build application
	R CMD check --no-manual $(PACKAGE_TARBALL)

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

prepare-exdqlm-cran-runtime:
	$(R) application/scripts/12_prepare_exdqlm_cran_runtime.R

prepare-quantreg-cran-runtime:
	$(R) application/scripts/12_prepare_quantreg_cran_runtime.R

preflight-dlm-main: package-install
	$(R) application/scripts/13_run_rqr_dlm_main_simulation_references.R preflight

oracle-reference-dlm-main: package-install
	$(R) application/scripts/13_run_rqr_dlm_main_simulation_references.R oracle-reference

tiny-end-to-end-dlm-main: package-install
	$(R) application/scripts/13_run_rqr_dlm_main_simulation_references.R tiny-end-to-end

diagnostic-pilot-preflight-dlm-main: package-install prepare-exdqlm-cran-runtime prepare-quantreg-cran-runtime
	$(R) application/scripts/13_run_rqr_dlm_main_simulation_references.R diagnostic-pilot-preflight

preflight-dlm-confirmatory:
	application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh preflight application/outputs/rqr_dlm_main_simulation_20260724/preflight

oracle-reference-dlm-confirmatory:
	application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh oracle-reference application/outputs/rqr_dlm_main_simulation_20260724/oracle-reference

failclosed-dlm-confirmatory:
	@! $(R) application/scripts/15_run_rqr_dlm_confirmatory_simulation.R execute-confirmatory application/outputs/rqr_dlm_main_simulation_20260724/forbidden-execution

failclosed-dlm-confirmatory-wave:
	@! $(R) application/scripts/17_launch_rqr_dlm_confirmatory_wave.R execute-confirmatory application/outputs/rqr_dlm_main_simulation_20260724/preflight/execution_wave_plan_maximum.csv invalid-wave application/outputs/rqr_dlm_main_simulation_20260724/forbidden-wave

test-dlm-confirmatory-monitor:
	application/scripts/16_test_rqr_dlm_confirmatory_monitor.sh

literature-manifest:
	$(R) application/scripts/01_build_literature_manifest.R

clean-tex:
	rm -f *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.synctex.gz *.toc *.run.xml *.bcf
