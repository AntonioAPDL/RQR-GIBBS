R ?= Rscript
PDFLATEX ?= pdflatex
BIBTEX ?= bibtex
LATEXMK ?= latexmk
PACKAGE_NAME := $(shell sed -n 's/^Package:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_VERSION := $(shell sed -n 's/^Version:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_TARBALL := $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz
THEORY_FIGURE_DIR ?= application/cache/rqr_theory_figures
ORDINARY_V1_NATIVE_FILTER := native-(package-integration|beta-prior|rhs-ns|fixed-design-v1|ordinary-v1-(boundary-audit|f01-quadrature|materializer|protected-dlm-companion|reference-cells|validation-runner)|desn-design|desn-fit-v1|desn-future-contract)

.PHONY: pdf supplement all-pdf theory-figures test-theory-figures smoke package-document package-install prepare-primary-runtime prepare-exdqlm-runtime prepare-exdqlm-cran-runtime prepare-quantreg-cran-runtime test-native test-ordinary-v1 ordinary-v1-ci ordinary-v1-package-check materialize-ordinary-v1-desn preflight-ordinary-v1 reference-ordinary-v1 test-ordinary-v1-dlm-companion guard-bundle-ordinary-v1-dlm-companion bundle-ordinary-v1-dlm-companion guard-benchmark-ordinary-v1 benchmark-ordinary-v1-one-cell test-ordinary-v1-monitor guard-execute-ordinary-v1 execute-ordinary-v1-bounded test-standalone-contracts package-check test-exdqlm-rqr bounded-pilot preflight-dlm-bounded reference-dlm-bounded test-dlm-monitor benchmark-dlm-bounded-one-cell execute-dlm-bounded preflight-dlm-main oracle-reference-dlm-main tiny-end-to-end-dlm-main diagnostic-pilot-preflight-dlm-main preflight-dlm-confirmatory oracle-reference-dlm-confirmatory validate-dlm-main-wave1-correction validate-dlm-main-wave1-comparator validate-dlm-main-horizon-fixed-design failclosed-dlm-confirmatory failclosed-dlm-confirmatory-wave test-dlm-confirmatory-monitor literature-manifest clean-tex

theory-figures:
	$(R) figures/generate_rqr_theory_figures.R --output-dir=$(THEORY_FIGURE_DIR)

test-theory-figures:
	$(R) figures/test_rqr_theory_figure_oracles.R

pdf: theory-figures
	@if command -v $(LATEXMK) >/dev/null 2>&1; then \
		$(LATEXMK) -pdf -interaction=nonstopmode main.tex; \
	else \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(BIBTEX) main || true; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
	fi

supplement: theory-figures
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

package-document:
	$(R) -e 'Rcpp::compileAttributes("application")'
	$(R) -e 'roxygen2::roxygenise("application")'

package-install:
	R CMD INSTALL --preclean application

prepare-exdqlm-runtime:
	$(R) application/scripts/04_prepare_pinned_exdqlm_runtime.R

prepare-primary-runtime:
	$(R) application/scripts/04_prepare_primary_runtime.R

test-native: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native", reporter = "summary")'

# CI-only helper: package-install compiles from the checkout and may leave
# ignored compiler products. Do not invoke this target in a shared local
# checkout; the ordinary-v1 workflow runs it in an ephemeral runner.
test-ordinary-v1: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "$(ORDINARY_V1_NATIVE_FILTER)", reporter = "summary")'

# Ephemeral-CI entry point. This target installs only the standalone package
# and runs bounded unit/reference fixtures. It does not prepare protected
# external runtimes or invoke any simulation launcher.
ordinary-v1-ci: test-ordinary-v1 test-ordinary-v1-monitor

# Full source-package check remains explicit because it also runs the broader
# native regression suite. It contains no simulation execution target.
ordinary-v1-package-check: package-check

# Design materialization only. The script requires clean main at the exact
# reviewed commit recorded in the frozen configuration and writes solely to
# ignored cache/output roots. It never fits a readout or launches MCMC.
materialize-ordinary-v1-desn: prepare-primary-runtime prepare-exdqlm-runtime
	@test "$${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "" || \
		{ echo "Set RQR_EXPECTED_PRIMARY_COMMIT to the exact clean-main SHA."; exit 2; }
	$(R) application/scripts/28_materialize_rqr_ordinary_v1_desn_design.R

preflight-ordinary-v1: prepare-primary-runtime
	application/scripts/26_run_rqr_ordinary_v1_validation.sh preflight

reference-ordinary-v1: materialize-ordinary-v1-desn
	application/scripts/26_run_rqr_ordinary_v1_validation.sh reference-only

test-ordinary-v1-dlm-companion:
	RQR_DLM_COMPANION_SOURCE_ONLY=YES $(R) -e \
		'testthat::test_file("application/tests/testthat/test-rqr-native-ordinary-v1-protected-dlm-companion.R", reporter = "summary")'

guard-bundle-ordinary-v1-dlm-companion:
	@test "$${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "" || \
		{ echo "Set RQR_EXPECTED_PRIMARY_COMMIT to the exact clean-main SHA."; exit 2; }
	@for variable in \
		RQR_DLM_REFERENCE_DIR RQR_DLM_M01_DIR RQR_DLM_M02_DIR \
		RQR_DLM_HORIZON_M03_DIR RQR_ORDINARY_V1_DLM_COMPANION_OUTPUT_DIR; do \
		eval "value=\$$$${variable}"; \
		test "$$value" != "" || \
			{ echo "Set $$variable for the protected-DLM companion."; exit 2; }; \
	done

bundle-ordinary-v1-dlm-companion: guard-bundle-ordinary-v1-dlm-companion
	$(R) application/scripts/30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R \
		"$${RQR_EXPECTED_PRIMARY_COMMIT}" \
		"$${RQR_DLM_REFERENCE_DIR}" \
		"$${RQR_DLM_M01_DIR}" \
		"$${RQR_DLM_M02_DIR}" \
		"$${RQR_DLM_HORIZON_M03_DIR}" \
		"$${RQR_ORDINARY_V1_DLM_COMPANION_OUTPUT_DIR}"

guard-benchmark-ordinary-v1:
	@test "$${RQR_ORDINARY_V1_BENCHMARK_CONFIRM:-}" = \
		"I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK" || \
		{ echo "Set the exact ordinary-v1 benchmark confirmation."; exit 2; }

benchmark-ordinary-v1-one-cell: guard-benchmark-ordinary-v1 materialize-ordinary-v1-desn
	application/scripts/26_run_rqr_ordinary_v1_validation.sh benchmark-one-cell

test-ordinary-v1-monitor:
	application/scripts/27_test_rqr_ordinary_v1_monitor.sh

# This target remains fail-closed while ordinary_v1_execute_enabled is FALSE
# in the reviewed tracked configuration.
guard-execute-ordinary-v1:
	@test "$${RQR_ORDINARY_V1_CONFIRM:-}" = "YES" || \
		{ echo "Set RQR_ORDINARY_V1_CONFIRM=YES."; exit 2; }
	@test -d "$${RQR_ORDINARY_V1_REFERENCE_DIR:-}" || \
		{ echo "Set RQR_ORDINARY_V1_REFERENCE_DIR to the reviewed bundle."; exit 2; }
	@test -d "$${RQR_ORDINARY_V1_BENCHMARK_DIR:-}" || \
		{ echo "Set RQR_ORDINARY_V1_BENCHMARK_DIR to the reviewed bundle."; exit 2; }
	@test -d "$${RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR:-}" || \
		{ echo "Set RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR."; exit 2; }
	@test -d "$${RQR_ORDINARY_V1_DLM_COMPANION_DIR:-}" || \
		{ echo "Set RQR_ORDINARY_V1_DLM_COMPANION_DIR."; exit 2; }

execute-ordinary-v1-bounded: guard-execute-ordinary-v1 materialize-ordinary-v1-desn
	application/scripts/26_run_rqr_ordinary_v1_validation.sh execute-bounded

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

validate-dlm-main-wave1-correction: package-install
	$(R) application/scripts/22_validate_rqr_dlm_wave1_corrections.R

validate-dlm-main-wave1-comparator: package-install
	$(R) application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R

validate-dlm-main-horizon-fixed-design: package-install
	$(R) application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R

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
