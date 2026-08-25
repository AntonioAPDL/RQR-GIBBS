R ?= Rscript
PDFLATEX ?= pdflatex
BIBTEX ?= bibtex
LATEXMK ?= latexmk
PACKAGE_NAME := $(shell sed -n 's/^Package:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_VERSION := $(shell sed -n 's/^Version:[[:space:]]*//p' application/DESCRIPTION)
PACKAGE_TARBALL := $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz
RQR_PACKAGE_LIBRARY ?=
RQR_PACKAGE_INSTALL_LIBRARY_ARG := $(if $(strip $(RQR_PACKAGE_LIBRARY)),--library=$(RQR_PACKAGE_LIBRARY),)
THEORY_FIGURE_DIR ?= figures/generated
THEORY_TABLE_DIR ?= tables
ORACLE_TILT_ILLUSTRATION_CONFIG ?= application/config/oracle_tilt_illustrations_20260728.json
ORACLE_TILT_ILLUSTRATION_DIR ?= application/outputs/oracle_tilt_illustrations
ORACLE_TILT_FORENSIC_CONFIG ?= application/config/oracle_tilt_forensics_20260730.json
ORACLE_TILT_PUBLICATION_CONFIG ?= application/config/oracle_tilt_c095_publication_20260731.json
ORACLE_TILT_PUBLICATION_DIR ?= application/outputs/oracle_tilt_c095_publication
ORACLE_TILT_PUBLICATION_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095
ORACLE_TILT_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095_v5_exact_delta
ORACLE_TILT_RUN_DIR ?=
ORACLE_TILT_V2_CONFIG ?= application/config/oracle_tilt_c095_publication_v2_20260731.json
ORACLE_TILT_V2_DIR ?= application/outputs/oracle_tilt_c095_publication_v2
ORACLE_TILT_V2_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095_v2
ORACLE_TILT_V2_RUN_DIR ?=
ORACLE_TILT_V3_CONFIG ?= application/config/oracle_tilt_c095_publication_v3_20260801.json
ORACLE_TILT_V3_DIR ?= application/outputs/oracle_tilt_c095_publication_v3
ORACLE_TILT_V3_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095_v3
ORACLE_TILT_V3_RUN_DIR ?=
ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIG ?= application/config/oracle_tilt_c095_dlm_sh_adjudication_recovery_20260805.json
ORACLE_TILT_DLM_SH_ADJUDICATION_DIR ?= application/outputs/oracle_tilt_dlm_sh_adjudication
ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR ?=
ORACLE_TILT_DLM_SH_BASELINE_DIR ?=
ORACLE_TILT_V3_NONPROMOTION_EVIDENCE_DIR ?= docs/audits/oracle_tilt_c095_v3_nonpromotion_evidence_20260805
ORACLE_TILT_V3_ACCEPTANCE_POLICY ?= application/config/oracle_tilt_c095_v3_revised_illustration_acceptance_20260805.json
ORACLE_TILT_V4_CONFIG ?= application/config/oracle_tilt_c095_publication_v4_seed_screen_20260805.json
ORACLE_TILT_V4_DIR ?= application/outputs/oracle_tilt_c095_publication_v4_seed_screen
ORACLE_TILT_V4_RUN_DIR ?=
ORACLE_TILT_V4_SELECTOR_DIR ?=
ORACLE_TILT_V4_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095_v4_selected
ORACLE_TILT_V5_CONFIG ?= application/config/oracle_tilt_c095_v5_exact_delta_20260810.json
ORACLE_TILT_V5_DIR ?= application/outputs/oracle_tilt_c095_v5_exact_delta
ORACLE_TILT_V5_RUN_DIR ?=
ORACLE_TILT_V5_EVIDENCE_DIR ?= figures/data/oracle_tilt_c095_v5_exact_delta
ORACLE_MEAN_TILT_VALIDATION_CONFIG ?= application/config/oracle_mean_tilt_validation_v1.json
ORACLE_MEAN_TILT_VALIDATION_DIR ?= application/outputs/oracle_mean_tilt_validation_v1
ORACLE_MEAN_TILT_VALIDATION_RUN_DIR ?=
ORACLE_MEAN_TILT_VALIDATION_EVIDENCE_DIR ?= figures/data/oracle_mean_tilt_validation_v1
TCSP_VALIDATION_CONFIG ?= application/config/tcsp_validation_v1.json
TCSP_VALIDATION_DIR ?= application/outputs/tcsp_validation_v1
TCSP_VALIDATION_RUN_DIR ?=
TCSP_VALIDATION_AUDIT_RUN_DIR ?= application/outputs/tcsp_validation_v1/pilot_codex_20260811
TCSP_VALIDATION_AUDIT_DIR ?= docs/audits/tcsp_validation_pilot_20260811
TCSP_VALIDATION_AUDIT_REPLACE ?= true
RQR_ECM_VALIDATION_CONFIG ?= application/config/rqr_ecm_validation_v1.json
RQR_ECM_VALIDATION_DIR ?= application/outputs/rqr_ecm_validation_v1
RQR_ECM_VALIDATION_SMOKE_DIR ?= $(RQR_ECM_VALIDATION_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
TCSP_SPLIT_EXACT_VALIDATION_CONFIG ?= application/config/tcsp_split_exact_validation_v1.json
TCSP_SPLIT_EXACT_VALIDATION_DIR ?= application/outputs/tcsp_split_exact_validation_v1
TCSP_SPLIT_EXACT_VALIDATION_SMOKE_DIR ?= $(TCSP_SPLIT_EXACT_VALIDATION_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_VALIDATION_CONFIG ?= application/config/rqr_bayes_uq_validation_v1.json
RQR_BAYES_UQ_VALIDATION_DIR ?= application/outputs/rqr_bayes_uq_validation_v1
RQR_BAYES_UQ_VALIDATION_SMOKE_DIR ?= $(RQR_BAYES_UQ_VALIDATION_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_VALIDATION_MODERATE_DIR ?= $(RQR_BAYES_UQ_VALIDATION_DIR)/moderate_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_VALIDATION_RUN_DIR ?=
RQR_BAYES_UQ_MAIN_CONFIG ?= application/config/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820.json
RQR_BAYES_UQ_MAIN_DIR ?= application/outputs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820
RQR_BAYES_UQ_MAIN_SMOKE_DIR ?= $(RQR_BAYES_UQ_MAIN_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_MAIN_RUN_DIR ?=
RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT ?= application/runs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820
RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT ?= 40
RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS ?= 60
RQR_BAYES_UQ_REFINED_CONFIG ?= application/config/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820.json
RQR_BAYES_UQ_REFINED_DIR ?= application/outputs/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820
RQR_BAYES_UQ_REFINED_SMOKE_DIR ?= $(RQR_BAYES_UQ_REFINED_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_REFINED_COMPLETED_RUN_DIR ?= application/runs/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820/wave_confirmatory_refined_dgps_20260820T221539Z
RQR_BAYES_UQ_REFINED_RUN_DIR ?= $(RQR_BAYES_UQ_REFINED_COMPLETED_RUN_DIR)
RQR_BAYES_UQ_REFINED_WAVE_RUN_ROOT ?= application/runs/rqr_bayes_uq_validation_main_3method_refined_dgps_20260820
RQR_BAYES_UQ_REFINED_WAVE_RUN_ID ?= wave_confirmatory_refined_dgps_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_REFINED_WAVE_MAX_CONCURRENT ?= 40
RQR_BAYES_UQ_REFINED_WAVE_POLL_SECONDS ?= 60
RQR_BAYES_UQ_SKEWSTRESS_CONFIG ?= application/config/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820.json
RQR_BAYES_UQ_SKEWSTRESS_DIR ?= application/outputs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820
RQR_BAYES_UQ_SKEWSTRESS_SMOKE_DIR ?= $(RQR_BAYES_UQ_SKEWSTRESS_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_SKEWSTRESS_COMPLETED_RUN_DIR ?= application/runs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820/wave_confirmatory_skewstress_dgps_20260821T005632Z
RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR ?= $(RQR_BAYES_UQ_SKEWSTRESS_COMPLETED_RUN_DIR)
RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ROOT ?= application/runs/rqr_bayes_uq_validation_main_3method_skewstress_dgps_20260820
RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ID ?= wave_confirmatory_skewstress_dgps_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_SKEWSTRESS_WAVE_MAX_CONCURRENT ?= 40
RQR_BAYES_UQ_SKEWSTRESS_WAVE_POLL_SECONDS ?= 60
RQR_BAYES_UQ_FOLLOWUP_CONFIG ?= application/config/rqr_bayes_uq_followup_20260816.json
RQR_BAYES_UQ_FOLLOWUP_DIR ?= application/outputs/rqr_bayes_uq_followup_20260816
RQR_BAYES_UQ_FOLLOWUP_SMOKE_DIR ?= $(RQR_BAYES_UQ_FOLLOWUP_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_FOLLOWUP_RUN_DIR ?=
RQR_BAYES_UQ_FOLLOWUP_WAVE_RUN_ROOT ?= application/runs/rqr_bayes_uq_followup_20260816
RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE ?= small_sample_95
RQR_BAYES_UQ_FOLLOWUP_WAVE_MAX_CONCURRENT ?= 40
RQR_BAYES_UQ_FOLLOWUP_WAVE_POLL_SECONDS ?= 60
RQR_BAYES_UQ_PRIMARY_RUN_DIR ?= $(RQR_BAYES_UQ_SKEWSTRESS_COMPLETED_RUN_DIR)
RQR_BAYES_UQ_ADAPTIVE_TCSP_RUN_DIR ?= application/runs/rqr_bayes_uq_validation_tcsp_adaptive_targeted_20260820/wave_confirmatory_tcsp_adaptive_targeted_20260820T071540Z
RQR_BAYES_UQ_ARTICLE_COMPOSITE_DIR ?= application/outputs/rqr_bayes_uq_validation_article_adaptive_tcsp_20260820
RQR_BAYES_UQ_PRIMARY_SUMMARY ?= $(RQR_BAYES_UQ_PRIMARY_RUN_DIR)/bayes_uq_validation_summary.csv
RQR_BAYES_UQ_PRIMARY_BY_N_CONTENT ?= $(RQR_BAYES_UQ_PRIMARY_RUN_DIR)/bayes_uq_validation_summary.csv
RQR_BAYES_UQ_PRIMARY_RESULTS ?= $(RQR_BAYES_UQ_PRIMARY_RUN_DIR)/bayes_uq_validation_results.csv
RQR_BAYES_UQ_MTI_ECM_RUN_DIR ?= application/runs/rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825/wave_confirmatory_20260825T072935Z
RQR_BAYES_UQ_MTI_ECM_RESULTS ?= $(RQR_BAYES_UQ_MTI_ECM_RUN_DIR)/bayes_uq_validation_results.csv
RQR_BAYES_UQ_MTI_ECM_POLICY ?= application/config/mti_ecm_adaptive_cell_policy_20260825.csv
RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG ?= application/config/rqr_bayes_uq_validation_mti_ecm_adaptive_strict_calibration_20260825.json
RQR_BAYES_UQ_MTI_ECM_STRICT_DIR ?= application/outputs/rqr_bayes_uq_validation_mti_ecm_adaptive_strict_calibration_20260825
RQR_BAYES_UQ_MTI_ECM_STRICT_SMOKE_DIR ?= $(RQR_BAYES_UQ_MTI_ECM_STRICT_DIR)/smoke_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ROOT ?= application/runs/rqr_bayes_uq_validation_mti_ecm_adaptive_strict_calibration_20260825
RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ID ?= wave_confirmatory_strict_mti_ecm_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_MAX_CONCURRENT ?= 40
RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_POLL_SECONDS ?= 60
RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR ?=
RQR_BAYES_UQ_MTI_ECM_STRICT_RESULTS ?= $(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR)/bayes_uq_validation_results.csv
RQR_BAYES_UQ_MTI_ECM_STRICT_POLICY ?= application/config/mti_ecm_adaptive_cell_strict_policy_20260825.csv
RQR_BAYES_UQ_MTI_ECM_STRICT_POLICY_DIAGNOSTICS ?= application/config/mti_ecm_adaptive_cell_strict_policy_20260825_diagnostics.csv
RQR_MTI_ECM_TRACE_CONFIG ?= application/config/rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825.json
RQR_MTI_ECM_TRACE_OUTPUT_DIR ?= application/outputs/mti_ecm_trace_diagnostics/trace_current_winner_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_MTI_ECM_TRACE_REPS_PER_CELL ?= 3
RQR_MTI_ECM_TRACE_WORKERS ?= 8
RQR_MTI_ECM_TRACE_REPORT_PDF ?= $(RQR_MTI_ECM_TRACE_OUTPUT_DIR)/figures/mti_ecm_trace_convergence_report.pdf
RQR_BAYES_UQ_PRIMARY_YM_RESULTS ?=
RQR_BAYES_UQ_SMALL95_RESULTS ?=
RQR_BAYES_UQ_SCAN_CALIBRATION ?= $(RQR_BAYES_UQ_PRIMARY_RUN_DIR)/scan_calibration_summary.csv
RQR_TOLERANCE_ADJUDICATION_MAIN_RUN_DIR ?=
RQR_TOLERANCE_ADJUDICATION_ECM_RUN_DIR ?= application/runs/rqr_bayes_uq_followup_20260816/wave_ecm200_audit_20260817T005025Z
RQR_TOLERANCE_ADJUDICATION_SMALL95_RUN_DIR ?= application/runs/rqr_bayes_uq_followup_20260816/wave_small_sample_95_20260817T005145Z
RQR_TOLERANCE_ADJUDICATION_PAPER90_RUN_DIR ?= application/runs/rqr_bayes_uq_followup_20260816/wave_paper_matched_90_20260817T005119Z
RQR_TOLERANCE_ADJUDICATION_DIR ?= application/outputs/tolerance_validation_adjudication/adjudication_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_DIR ?= application/outputs/tolerance_mti_gibbs_diagnostics/gibbs_diagnostics_$(shell date -u +%Y%m%dT%H%M%SZ)
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_CHAINS ?= 4
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_WORKERS ?= 4
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_BURN ?= 500
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_DRAWS ?= 1000
RQR_TOLERANCE_GIBBS_DIAGNOSTIC_SOURCE ?= application/outputs/tolerance_mti_gibbs_diagnostics/gibbs_diagnostics_long_20260818T013452Z/gibbs_estimator_diagnostics.csv
RQR_DLM_AFFECTED_PRIMARY_ATTESTATION ?=
RQR_DLM_AFFECTED_EXDQLM_ATTESTATION ?=
RQR_DLM_AFFECTED_QUANTREG_ATTESTATION ?=
RQR_DLM_AFFECTED_OUTPUT_ROOT ?=
RQR_DLM_RECOVERY_PRIMARY_ATTESTATION ?=
RQR_DLM_RECOVERY_OUTPUT_ROOT ?=
RQR_DLM_RECOVERY_CONTROL_ROOT ?=
RQR_DLM_RECOVERY_WORKERS ?= 8
RQR_DLM_DIAGNOSTIC_LAUNCH_INPUTS ?=
RQR_DLM_DIAGNOSTIC_RUN_ID ?=
RQR_DLM_DIAGNOSTIC_RUN_ROOT ?=
RQR_DLM_DIAGNOSTIC_LOG_ROOT ?=
RQR_DLM_M02_CANARY_OUTPUT_ROOT ?=

.PHONY: pdf supplement all-pdf theory-figures theory-tables tolerance-validation-adaptive-article-inputs model-illustration-figures test-theory-figures test-theory-tables test-manuscript-language arxiv-source smoke package-install prepare-primary-runtime prepare-exdqlm-runtime prepare-exdqlm-cran-runtime prepare-quantreg-cran-runtime test-native test-native-mean-tilt test-oracle-tilt-illustrations test-oracle-tilt-forensics test-oracle-tilt-publication test-oracle-tilt-publication-v2 test-oracle-tilt-publication-v3 test-oracle-tilt-publication-v4 test-oracle-tilt-dlm-sh-adjudication test-oracle-tilt-campaign-closeout test-oracle-tilt-v2-workflow oracle-tilt-illustrations oracle-tilt-illustrations-dry-run oracle-tilt-forensics-preflight oracle-tilt-forensics-execute oracle-tilt-publication-preflight oracle-tilt-publication-execute oracle-tilt-package-evidence oracle-tilt-v2-preflight oracle-tilt-v2-reference oracle-tilt-v2-benchmark oracle-tilt-v2-execute oracle-tilt-v2-package-evidence oracle-tilt-v3-preflight oracle-tilt-v3-reference oracle-tilt-v3-benchmark oracle-tilt-v3-resource-rehearsal oracle-tilt-v3-acceptance oracle-tilt-v3-execute oracle-tilt-v3-package-evidence oracle-tilt-v3-package-nonpromotion-evidence oracle-tilt-v3-promote-revised-evidence oracle-tilt-v4-preflight oracle-tilt-v4-reference oracle-tilt-v4-benchmark oracle-tilt-v4-resource-rehearsal oracle-tilt-v4-launch oracle-tilt-v4-select oracle-tilt-v4-package-evidence oracle-tilt-dlm-sh-adjudication-preflight oracle-tilt-dlm-sh-adjudication-execute oracle-tilt-dlm-sh-reconcile-evidence test-standalone-contracts package-check test-exdqlm-rqr bounded-pilot preflight-dlm-bounded reference-dlm-bounded test-dlm-monitor benchmark-dlm-bounded-one-cell execute-dlm-bounded preflight-dlm-main oracle-reference-dlm-main tiny-end-to-end-dlm-main diagnostic-pilot-preflight-dlm-main preflight-dlm-confirmatory oracle-reference-dlm-confirmatory validate-dlm-main-wave1-correction validate-dlm-main-wave1-comparator validate-dlm-main-wave2-correction validate-dlm-main-wave2-comparator validate-dlm-main-horizon-fixed-design preflight-dlm-main-wave2-m03-m08-stress validate-dlm-main-wave2-m03-m08-stress validate-dlm-main-wave2-m03-m08-full validate-dlm-main-resource-envelope failclosed-dlm-confirmatory failclosed-dlm-confirmatory-wave test-dlm-confirmatory-monitor launch-dlm-affected-wave-validation health-dlm-affected-wave-validation launch-dlm-multicomponent-recovery health-dlm-multicomponent-recovery launch-dlm-diagnostic-aware health-dlm-diagnostic-aware literature-manifest clean-tex
.PHONY: test-oracle-tilt-publication-v5 oracle-tilt-v5-preflight oracle-tilt-v5-reference oracle-tilt-v5-benchmark oracle-tilt-v5-execute oracle-tilt-v5-package-evidence
.PHONY: test-oracle-mean-tilt-validation oracle-mean-tilt-validation-preflight oracle-mean-tilt-validation-reference oracle-mean-tilt-validation-benchmark oracle-mean-tilt-validation-sentinel oracle-mean-tilt-validation-execute-wave oracle-mean-tilt-validation-collect oracle-mean-tilt-validation-precision oracle-mean-tilt-validation-package
.PHONY: test-ecm test-tcsp test-tcsp-validation test-tcsp-validation-audit test-bayes-uq test-tolerance-adjudication test-mpi-mti-naming rqr-ecm-validation-smoke tcsp-split-exact-validation-smoke rqr-bayes-uq-validation-smoke rqr-bayes-uq-validation-moderate launch-rqr-bayes-uq-overnight health-rqr-bayes-uq-validation rqr-bayes-uq-main-smoke prepare-rqr-bayes-uq-main-waves launch-rqr-bayes-uq-main launch-rqr-bayes-uq-main-dpm-companion health-rqr-bayes-uq-main collect-rqr-bayes-uq-main stop-rqr-bayes-uq-main rqr-bayes-uq-refined-smoke prepare-rqr-bayes-uq-refined-waves launch-rqr-bayes-uq-refined health-rqr-bayes-uq-refined collect-rqr-bayes-uq-refined stop-rqr-bayes-uq-refined rqr-bayes-uq-skewstress-smoke prepare-rqr-bayes-uq-skewstress-waves launch-rqr-bayes-uq-skewstress health-rqr-bayes-uq-skewstress collect-rqr-bayes-uq-skewstress stop-rqr-bayes-uq-skewstress rqr-bayes-uq-followup-smoke prepare-rqr-bayes-uq-followup-waves launch-rqr-bayes-uq-followup health-rqr-bayes-uq-followup collect-rqr-bayes-uq-followup stop-rqr-bayes-uq-followup tolerance-validation-adjudication tolerance-mti-gibbs-diagnostics mti-ecm-trace-diagnostics mti-ecm-trace-diagnostic-figures mti-ecm-trace-report test-mti-ecm-trace-diagnostics mti-bayes-uq-main-smoke prepare-mti-bayes-uq-main-waves launch-mti-bayes-uq-main health-mti-bayes-uq-main collect-mti-bayes-uq-main stop-mti-bayes-uq-main tcsp-validation-preflight tcsp-validation-tiny tcsp-validation-pilot tcsp-validation-full-pilot tcsp-validation-health tcsp-validation-audit
.PHONY: mti-ecm-strict-calibration-smoke prepare-mti-ecm-strict-calibration-waves launch-mti-ecm-strict-calibration health-mti-ecm-strict-calibration collect-mti-ecm-strict-calibration stop-mti-ecm-strict-calibration build-mti-ecm-strict-policy
.PHONY: validate-dlm-m02-diagnostic-canary

tolerance-validation-adaptive-article-inputs:
	$(R) application/scripts/81_compose_adaptive_tcsp_article_inputs.R --baseline-run-dir=$(RQR_BAYES_UQ_PRIMARY_RUN_DIR) --adaptive-run-dir=$(RQR_BAYES_UQ_ADAPTIVE_TCSP_RUN_DIR) --output-dir=$(RQR_BAYES_UQ_ARTICLE_COMPOSITE_DIR) --overwrite=true

theory-figures:
	$(R) figures/generate_rqr_theory_figures.R --output-dir=$(THEORY_FIGURE_DIR)
	$(R) figures/generate_tolerance_validation_primary_figure.R --primary-results=$(RQR_BAYES_UQ_PRIMARY_RESULTS) --mti-ecm-results=$(RQR_BAYES_UQ_MTI_ECM_RESULTS) --young-mathew-results=$(RQR_BAYES_UQ_PRIMARY_YM_RESULTS) --scenario-range-csv=$(THEORY_TABLE_DIR)/tolerance_validation_article_scenario_ranges.csv --output-dir=$(THEORY_FIGURE_DIR)
	$(R) figures/generate_tolerance_validation_width_figure.R --width-csv=$(THEORY_TABLE_DIR)/tolerance_validation_article_dgp_width_ranges.csv --output-dir=$(THEORY_FIGURE_DIR)

theory-tables:
	$(R) tables/generate_mean_tilt_cf_mini_study_table.R --output-dir=$(THEORY_TABLE_DIR)
	$(R) tables/generate_tolerance_validation_summary_table.R --summary-csv=$(RQR_BAYES_UQ_PRIMARY_SUMMARY) --results-csv=$(RQR_BAYES_UQ_PRIMARY_RESULTS) --young-mathew-results=$(RQR_BAYES_UQ_PRIMARY_YM_RESULTS) --output-dir=$(THEORY_TABLE_DIR)
	$(R) tables/generate_tolerance_validation_stratified_table.R --summary-csv=$(RQR_BAYES_UQ_PRIMARY_BY_N_CONTENT) --results-csv=$(RQR_BAYES_UQ_PRIMARY_RESULTS) --young-mathew-results=$(RQR_BAYES_UQ_PRIMARY_YM_RESULTS) --output-dir=$(THEORY_TABLE_DIR)
	$(R) tables/generate_tolerance_validation_followup_tables.R --output-dir=$(THEORY_TABLE_DIR)
	$(R) tables/generate_tolerance_validation_scenario_tables.R --primary-results=$(RQR_BAYES_UQ_PRIMARY_RESULTS) --mti-ecm-results=$(RQR_BAYES_UQ_MTI_ECM_RESULTS) --mti-ecm-policy-csv=$(RQR_BAYES_UQ_MTI_ECM_POLICY) --young-mathew-results=$(RQR_BAYES_UQ_PRIMARY_YM_RESULTS) --small95-results=$(RQR_BAYES_UQ_SMALL95_RESULTS) --scan-calibration-csv=$(RQR_BAYES_UQ_SCAN_CALIBRATION) --output-dir=$(THEORY_TABLE_DIR)
	$(R) tables/generate_tolerance_validation_gibbs_diagnostic_table.R --diagnostics-csv=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_SOURCE) --output-dir=$(THEORY_TABLE_DIR)

model-illustration-figures:
	$(R) figures/generate_oracle_tilt_model_figures.R --evidence-dir=$(ORACLE_TILT_EVIDENCE_DIR) --output-dir=$(THEORY_FIGURE_DIR) --table-dir=$(THEORY_TABLE_DIR)

test-theory-figures:
	$(R) figures/test_rqr_theory_figure_oracles.R
	$(R) figures/test_tolerance_validation_primary_figure.R
	$(R) figures/test_tolerance_validation_width_figure.R

test-mti-ecm-trace-diagnostics:
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-mti-ecm-profile-trace.R
	$(R) figures/test_mti_ecm_convergence_diagnostic_figures.R
	$(R) figures/test_mti_ecm_trace_convergence_report.R

mti-ecm-trace-diagnostics:
	$(R) application/scripts/83_collect_mti_ecm_trace_diagnostics.R --config=$(RQR_MTI_ECM_TRACE_CONFIG) --results-csv=$(RQR_BAYES_UQ_MTI_ECM_RESULTS) --policy-csv=$(RQR_BAYES_UQ_MTI_ECM_POLICY) --output-dir=$(RQR_MTI_ECM_TRACE_OUTPUT_DIR) --replications-per-cell=$(RQR_MTI_ECM_TRACE_REPS_PER_CELL) --workers=$(RQR_MTI_ECM_TRACE_WORKERS)

mti-ecm-trace-diagnostic-figures:
	$(R) figures/generate_mti_ecm_convergence_diagnostic_figures.R --trace-dir=$(RQR_MTI_ECM_TRACE_OUTPUT_DIR)

mti-ecm-trace-report:
	$(R) figures/generate_mti_ecm_trace_convergence_report.R --trace-dir=$(RQR_MTI_ECM_TRACE_OUTPUT_DIR) --output-pdf=$(RQR_MTI_ECM_TRACE_REPORT_PDF)

test-theory-tables:
	$(R) tables/test_mean_tilt_cf_mini_study_table.R
	$(R) tables/test_tolerance_validation_summary_table.R
	$(R) tables/test_tolerance_validation_stratified_table.R
	$(R) tables/test_tolerance_validation_followup_tables.R
	$(R) tables/test_tolerance_validation_scenario_tables.R
	$(R) tables/test_tolerance_validation_gibbs_diagnostic_table.R

test-manuscript-language:
	$(R) application/scripts/63_validate_manuscript_bayesian_language.R

test-mpi-mti-naming:
	$(R) application/scripts/73_validate_mpi_mti_naming_migration.R

test-ecm: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-ecm-latent-moments.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-ecm-root-system.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-ecm-objective.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-ecm-monotonicity.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-ecm-contract.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-split-exact.R

test-tcsp: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-action-contract.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-feasibility.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-fixed-target-mcmc.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-ecm-contract.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-split-exact.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-path-continuation.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-scan-calibration.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-shortest-window.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-theory-wiring.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-competitor-wrappers.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-validation-study.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-validation-pilot-audit.R

test-tcsp-validation: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-validation-study.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-competitor-wrappers.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-validation-pilot-audit.R

test-tcsp-validation-audit: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-validation-pilot-audit.R

rqr-ecm-validation-smoke: package-install
	$(R) application/scripts/67_validate_rqr_ecm_fixed_target.R --mode=smoke --config=$(RQR_ECM_VALIDATION_CONFIG) --output-dir=$(RQR_ECM_VALIDATION_SMOKE_DIR)

tcsp-split-exact-validation-smoke: package-install
	$(R) application/scripts/68_validate_tcsp_split_exact.R --mode=smoke --config=$(TCSP_SPLIT_EXACT_VALIDATION_CONFIG) --output-dir=$(TCSP_SPLIT_EXACT_VALIDATION_SMOKE_DIR)

test-bayes-uq: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-direct-dp.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-dpm.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-plugin-scope.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-main-validation-config.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-skewstress-dgps.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-bayes-uq-followup-config.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-tolerance-validation-adjudication.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-tolerance-mti-gibbs-diagnostics.R

test-tolerance-adjudication: package-install
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-tolerance-validation-adjudication.R
	$(R) application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-tolerance-mti-gibbs-diagnostics.R

rqr-bayes-uq-validation-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_VALIDATION_CONFIG) --output-dir=$(RQR_BAYES_UQ_VALIDATION_SMOKE_DIR)

rqr-bayes-uq-validation-moderate: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=moderate --config=$(RQR_BAYES_UQ_VALIDATION_CONFIG) --output-dir=$(RQR_BAYES_UQ_VALIDATION_MODERATE_DIR)

tolerance-validation-adjudication: package-install
	@test -n "$(strip $(RQR_TOLERANCE_ADJUDICATION_MAIN_RUN_DIR))" || { echo "Set RQR_TOLERANCE_ADJUDICATION_MAIN_RUN_DIR to the completed primary validation run." >&2; exit 64; }
	$(R) application/scripts/76_adjudicate_tolerance_validation_results.R --main-run-dir=$(RQR_TOLERANCE_ADJUDICATION_MAIN_RUN_DIR) --ecm-run-dir=$(RQR_TOLERANCE_ADJUDICATION_ECM_RUN_DIR) --small95-run-dir=$(RQR_TOLERANCE_ADJUDICATION_SMALL95_RUN_DIR) --paper90-run-dir=$(RQR_TOLERANCE_ADJUDICATION_PAPER90_RUN_DIR) --output-dir=$(RQR_TOLERANCE_ADJUDICATION_DIR)

tolerance-mti-gibbs-diagnostics: package-install
	$(R) application/scripts/77_run_tolerance_mti_gibbs_diagnostics.R --output-dir=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_DIR) --chains=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_CHAINS) --workers=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_WORKERS) --n-burn=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_BURN) --n-mcmc=$(RQR_TOLERANCE_GIBBS_DIAGNOSTIC_DRAWS)

launch-rqr-bayes-uq-overnight: package-install
	bash application/scripts/70_launch_rqr_bayes_uq_overnight.sh

health-rqr-bayes-uq-validation:
	@test -n "$(strip $(RQR_BAYES_UQ_VALIDATION_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_VALIDATION_RUN_DIR to one completed Bayesian UQ validation run." >&2; exit 64; }
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=health-check-read-only --run-dir=$(RQR_BAYES_UQ_VALIDATION_RUN_DIR)

rqr-bayes-uq-main-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_MAIN_CONFIG) --output-dir=$(RQR_BAYES_UQ_MAIN_SMOKE_DIR)

prepare-rqr-bayes-uq-main-waves: package-install
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=prepare --mode=confirmatory --config=$(RQR_BAYES_UQ_MAIN_CONFIG) --run-root=$(RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT)

launch-rqr-bayes-uq-main: package-install
	RQR_BAYES_UQ_MAIN_CONFIG=$(RQR_BAYES_UQ_MAIN_CONFIG) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT=$(RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT) \
	RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=$(RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT) \
	RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS=$(RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS) \
	bash application/scripts/72_launch_rqr_bayes_uq_main_waves.sh

launch-rqr-bayes-uq-main-dpm-companion: package-install
	RQR_BAYES_UQ_OVERNIGHT_MODE=dpm_companion \
	RQR_BAYES_UQ_VALIDATION_CONFIG=$(RQR_BAYES_UQ_MAIN_CONFIG) \
	RQR_BAYES_UQ_VALIDATION_RUN_ROOT=application/runs/rqr_bayes_uq_validation_main_20260813 \
	RQR_BAYES_UQ_VALIDATION_LOG_ROOT=application/logs/rqr_bayes_uq_validation_main_20260813 \
	bash application/scripts/70_launch_rqr_bayes_uq_overnight.sh

health-rqr-bayes-uq-main:
	@test -n "$(strip $(RQR_BAYES_UQ_MAIN_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MAIN_RUN_DIR to one completed Bayesian UQ main run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=health --run-dir=$(RQR_BAYES_UQ_MAIN_RUN_DIR)

collect-rqr-bayes-uq-main:
	@test -n "$(strip $(RQR_BAYES_UQ_MAIN_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MAIN_RUN_DIR to one Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=collect --run-dir=$(RQR_BAYES_UQ_MAIN_RUN_DIR)

stop-rqr-bayes-uq-main:
	@test -n "$(strip $(RQR_BAYES_UQ_MAIN_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MAIN_RUN_DIR to one Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=stop --run-dir=$(RQR_BAYES_UQ_MAIN_RUN_DIR)

rqr-bayes-uq-refined-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_REFINED_CONFIG) --output-dir=$(RQR_BAYES_UQ_REFINED_SMOKE_DIR)

prepare-rqr-bayes-uq-refined-waves: package-install
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=prepare --mode=confirmatory --config=$(RQR_BAYES_UQ_REFINED_CONFIG) --run-root=$(RQR_BAYES_UQ_REFINED_WAVE_RUN_ROOT) --run-id=$(RQR_BAYES_UQ_REFINED_WAVE_RUN_ID)

launch-rqr-bayes-uq-refined: package-install
	OMP_NUM_THREADS=1 \
	OPENBLAS_NUM_THREADS=1 \
	MKL_NUM_THREADS=1 \
	VECLIB_MAXIMUM_THREADS=1 \
	NUMEXPR_NUM_THREADS=1 \
	RQR_BAYES_UQ_MAIN_CONFIG=$(RQR_BAYES_UQ_REFINED_CONFIG) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT=$(RQR_BAYES_UQ_REFINED_WAVE_RUN_ROOT) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ID=$(RQR_BAYES_UQ_REFINED_WAVE_RUN_ID) \
	RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=$(RQR_BAYES_UQ_REFINED_WAVE_MAX_CONCURRENT) \
	RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS=$(RQR_BAYES_UQ_REFINED_WAVE_POLL_SECONDS) \
	bash application/scripts/72_launch_rqr_bayes_uq_main_waves.sh

health-rqr-bayes-uq-refined:
	@test -n "$(strip $(RQR_BAYES_UQ_REFINED_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_REFINED_RUN_DIR to one refined Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=health --run-dir=$(RQR_BAYES_UQ_REFINED_RUN_DIR)

collect-rqr-bayes-uq-refined:
	@test -n "$(strip $(RQR_BAYES_UQ_REFINED_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_REFINED_RUN_DIR to one refined Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=collect --run-dir=$(RQR_BAYES_UQ_REFINED_RUN_DIR)

stop-rqr-bayes-uq-refined:
	@test -n "$(strip $(RQR_BAYES_UQ_REFINED_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_REFINED_RUN_DIR to one refined Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=stop --run-dir=$(RQR_BAYES_UQ_REFINED_RUN_DIR)

rqr-bayes-uq-skewstress-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_SKEWSTRESS_CONFIG) --output-dir=$(RQR_BAYES_UQ_SKEWSTRESS_SMOKE_DIR)

prepare-rqr-bayes-uq-skewstress-waves: package-install
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=prepare --mode=confirmatory --config=$(RQR_BAYES_UQ_SKEWSTRESS_CONFIG) --run-root=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ROOT) --run-id=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ID)

launch-rqr-bayes-uq-skewstress: package-install
	OMP_NUM_THREADS=1 \
	OPENBLAS_NUM_THREADS=1 \
	MKL_NUM_THREADS=1 \
	VECLIB_MAXIMUM_THREADS=1 \
	NUMEXPR_NUM_THREADS=1 \
	RQR_BAYES_UQ_MAIN_CONFIG=$(RQR_BAYES_UQ_SKEWSTRESS_CONFIG) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ROOT) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ID=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_RUN_ID) \
	RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_MAX_CONCURRENT) \
	RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS=$(RQR_BAYES_UQ_SKEWSTRESS_WAVE_POLL_SECONDS) \
	bash application/scripts/72_launch_rqr_bayes_uq_main_waves.sh

health-rqr-bayes-uq-skewstress:
	@test -n "$(strip $(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR to one skew-stress Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=health --run-dir=$(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR)

collect-rqr-bayes-uq-skewstress:
	@test -n "$(strip $(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR to one skew-stress Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=collect --run-dir=$(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR)

stop-rqr-bayes-uq-skewstress:
	@test -n "$(strip $(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR to one skew-stress Bayesian UQ wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=stop --run-dir=$(RQR_BAYES_UQ_SKEWSTRESS_RUN_DIR)

rqr-bayes-uq-followup-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) --output-dir=$(RQR_BAYES_UQ_FOLLOWUP_SMOKE_DIR)

prepare-rqr-bayes-uq-followup-waves: package-install
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=prepare --mode=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE) --config=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) --run-root=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_RUN_ROOT)

launch-rqr-bayes-uq-followup: package-install
	RQR_BAYES_UQ_MAIN_CONFIG=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_RUN_ROOT) \
	RQR_BAYES_UQ_MAIN_WAVE_MODE=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE) \
	RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MAX_CONCURRENT) \
	RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_POLL_SECONDS) \
	bash application/scripts/72_launch_rqr_bayes_uq_main_waves.sh

health-rqr-bayes-uq-followup:
	@test -n "$(strip $(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_FOLLOWUP_RUN_DIR to one Bayesian UQ follow-up wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=health --mode=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE) --config=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) --run-dir=$(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR)

collect-rqr-bayes-uq-followup:
	@test -n "$(strip $(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_FOLLOWUP_RUN_DIR to one Bayesian UQ follow-up wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=collect --mode=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE) --config=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) --run-dir=$(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR)

stop-rqr-bayes-uq-followup:
	@test -n "$(strip $(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_FOLLOWUP_RUN_DIR to one Bayesian UQ follow-up wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=stop --mode=$(RQR_BAYES_UQ_FOLLOWUP_WAVE_MODE) --config=$(RQR_BAYES_UQ_FOLLOWUP_CONFIG) --run-dir=$(RQR_BAYES_UQ_FOLLOWUP_RUN_DIR)

mti-bayes-uq-main-smoke: rqr-bayes-uq-main-smoke

prepare-mti-bayes-uq-main-waves: prepare-rqr-bayes-uq-main-waves

launch-mti-bayes-uq-main: launch-rqr-bayes-uq-main

health-mti-bayes-uq-main: health-rqr-bayes-uq-main

collect-mti-bayes-uq-main: collect-rqr-bayes-uq-main

stop-mti-bayes-uq-main: stop-rqr-bayes-uq-main

mti-ecm-strict-calibration-smoke: package-install
	$(R) application/scripts/69_validate_rqr_bayes_uq.R --mode=smoke --config=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) --output-dir=$(RQR_BAYES_UQ_MTI_ECM_STRICT_SMOKE_DIR)

prepare-mti-ecm-strict-calibration-waves: package-install
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=prepare --mode=confirmatory --config=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) --run-root=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ROOT) --run-id=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ID)

launch-mti-ecm-strict-calibration: package-install
	RQR_BAYES_UQ_MAIN_CONFIG=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ROOT) \
	RQR_BAYES_UQ_MAIN_WAVE_RUN_ID=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_RUN_ID) \
	RQR_BAYES_UQ_MAIN_WAVE_MODE=confirmatory \
	RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_MAX_CONCURRENT) \
	RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS=$(RQR_BAYES_UQ_MTI_ECM_STRICT_WAVE_POLL_SECONDS) \
	bash application/scripts/72_launch_rqr_bayes_uq_main_waves.sh

health-mti-ecm-strict-calibration:
	@test -n "$(strip $(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR to one strict MTI-ECM wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=health --mode=confirmatory --config=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) --run-dir=$(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR)

collect-mti-ecm-strict-calibration:
	@test -n "$(strip $(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR to one strict MTI-ECM wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=collect --mode=confirmatory --config=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) --run-dir=$(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR)

stop-mti-ecm-strict-calibration:
	@test -n "$(strip $(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR to one strict MTI-ECM wave run." >&2; exit 64; }
	$(R) application/scripts/71_manage_rqr_bayes_uq_main_waves.R --action=stop --mode=confirmatory --config=$(RQR_BAYES_UQ_MTI_ECM_STRICT_CONFIG) --run-dir=$(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR)

build-mti-ecm-strict-policy: package-install
	@test -n "$(strip $(RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR))" || { echo "Set RQR_BAYES_UQ_MTI_ECM_STRICT_RUN_DIR to one completed strict MTI-ECM wave run." >&2; exit 64; }
	$(R) application/scripts/82_build_mti_ecm_adaptive_policy.R --results=$(RQR_BAYES_UQ_MTI_ECM_STRICT_RESULTS) --output=$(RQR_BAYES_UQ_MTI_ECM_STRICT_POLICY) --diagnostics-output=$(RQR_BAYES_UQ_MTI_ECM_STRICT_POLICY_DIAGNOSTICS) --policy-id=mti_ecm_adaptive_cell_strict_20260825 --selection=cell --method-pattern=^mti_ecm_adaptive_strict_screen_

tcsp-validation-preflight: package-install
	$(R) application/scripts/64_run_tcsp_validation_study.R --mode=preflight --config=$(TCSP_VALIDATION_CONFIG) --output-dir=$(TCSP_VALIDATION_DIR)/preflight

tcsp-validation-tiny: package-install
	$(R) application/scripts/64_run_tcsp_validation_study.R --mode=tiny --config=$(TCSP_VALIDATION_CONFIG) --output-dir=$(TCSP_VALIDATION_DIR)/tiny

tcsp-validation-pilot: package-install
	$(R) application/scripts/64_run_tcsp_validation_study.R --mode=pilot --config=$(TCSP_VALIDATION_CONFIG) --output-dir=$(TCSP_VALIDATION_DIR)/pilot

tcsp-validation-full-pilot: package-install
	$(R) application/scripts/64_run_tcsp_validation_study.R --mode=full_pilot --config=$(TCSP_VALIDATION_CONFIG) --output-dir=$(TCSP_VALIDATION_DIR)/full_pilot

tcsp-validation-health:
	@test -n "$(strip $(TCSP_VALIDATION_RUN_DIR))" || { echo "Set TCSP_VALIDATION_RUN_DIR to one completed TCSP validation run." >&2; exit 64; }
	$(R) application/scripts/64_run_tcsp_validation_study.R --mode=health-check-read-only --run-dir=$(TCSP_VALIDATION_RUN_DIR)

tcsp-validation-audit:
	@test -n "$(strip $(TCSP_VALIDATION_AUDIT_RUN_DIR))" || { echo "Set TCSP_VALIDATION_AUDIT_RUN_DIR to one completed TCSP validation run." >&2; exit 64; }
	$(R) application/scripts/65_audit_tcsp_validation_pilot.R --run-dir=$(TCSP_VALIDATION_AUDIT_RUN_DIR) --output-dir=$(TCSP_VALIDATION_AUDIT_DIR) --replace=$(TCSP_VALIDATION_AUDIT_REPLACE)

pdf: theory-figures theory-tables
	@if command -v $(LATEXMK) >/dev/null 2>&1; then \
		$(LATEXMK) -pdf -interaction=nonstopmode main.tex; \
	else \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(BIBTEX) main || true; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
		$(PDFLATEX) -interaction=nonstopmode main.tex; \
	fi

supplement: theory-figures model-illustration-figures
	@if command -v $(LATEXMK) >/dev/null 2>&1; then \
		$(LATEXMK) -pdf -interaction=nonstopmode rqr-gibbs-supplement.tex; \
	else \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
		$(BIBTEX) rqr-gibbs-supplement || true; \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
		$(PDFLATEX) -interaction=nonstopmode rqr-gibbs-supplement.tex; \
	fi

all-pdf: pdf supplement

arxiv-source: pdf
	bash application/scripts/31_prepare_arxiv_source.sh

smoke:
	$(R) application/scripts/00_validate_environment.R

package-install:
	@if [ -n "$${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ] && [ -z "$(strip $(RQR_PACKAGE_LIBRARY))" ]; then \
		echo "Refusing to install into an attested runtime; set RQR_PACKAGE_LIBRARY to a disjoint test library." >&2; \
		exit 65; \
	fi
	@if [ -n "$(strip $(RQR_PACKAGE_LIBRARY))" ]; then mkdir -p "$(RQR_PACKAGE_LIBRARY)"; fi
	R CMD INSTALL --preclean --clean $(RQR_PACKAGE_INSTALL_LIBRARY_ARG) application

prepare-exdqlm-runtime:
	$(R) application/scripts/04_prepare_pinned_exdqlm_runtime.R

prepare-primary-runtime:
	$(R) application/scripts/04_prepare_primary_runtime.R

test-native: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native", reporter = "summary")'

test-native-mean-tilt: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native-mean-tilt", reporter = "summary")'

test-oracle-tilt-illustrations: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-illustrations.R", reporter = "summary")'

test-oracle-tilt-forensics: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-forensics.R", reporter = "summary")'

test-oracle-tilt-publication: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-publication.R", reporter = "summary")'

test-oracle-tilt-publication-v2: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-publication-v2.R", reporter = "summary")'

test-oracle-tilt-publication-v3: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-publication-v3.R", reporter = "summary")'

test-oracle-tilt-publication-v4: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-publication-v4.R", reporter = "summary")'

test-oracle-tilt-publication-v5: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-interval-oracle.R", reporter = "summary"); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-v5.R", reporter = "summary"); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-v5-promotion.R", reporter = "summary")'

test-oracle-tilt-dlm-sh-adjudication: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-dlm-sh-adjudication.R", reporter = "summary")'

test-oracle-tilt-campaign-closeout:
	$(R) -e 'testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-campaign-closeout.R", reporter = "summary")'

test-oracle-tilt-v2-workflow:
	application/scripts/42_test_oracle_tilt_v2_workflow.sh

oracle-tilt-illustrations-dry-run: package-install
	$(R) application/scripts/32_run_oracle_tilt_illustrations.R --config=$(ORACLE_TILT_ILLUSTRATION_CONFIG) --output-dir=$(ORACLE_TILT_ILLUSTRATION_DIR)/dry-run --dry-run

oracle-tilt-illustrations: package-install
	$(R) application/scripts/32_run_oracle_tilt_illustrations.R --config=$(ORACLE_TILT_ILLUSTRATION_CONFIG)

oracle-tilt-forensics-preflight: package-install
	$(R) application/scripts/33_run_oracle_tilt_forensics.R --mode=preflight --config=$(ORACLE_TILT_FORENSIC_CONFIG)

oracle-tilt-forensics-execute: package-install
	$(R) application/scripts/33_run_oracle_tilt_forensics.R --mode=execute --config=$(ORACLE_TILT_FORENSIC_CONFIG)

oracle-tilt-publication-preflight: package-install
	$(R) application/scripts/34_run_oracle_tilt_publication.R --mode=preflight --config=$(ORACLE_TILT_PUBLICATION_CONFIG)

oracle-tilt-publication-execute:
	$(R) application/scripts/34_run_oracle_tilt_publication.R --mode=execute --config=$(ORACLE_TILT_PUBLICATION_CONFIG) --output-dir=$(ORACLE_TILT_PUBLICATION_DIR)

oracle-tilt-package-evidence:
	@test -n "$(strip $(ORACLE_TILT_RUN_DIR))" || { echo "Set ORACLE_TILT_RUN_DIR to one completed run." >&2; exit 64; }
	$(R) application/scripts/35_package_oracle_tilt_evidence.R --run-dir=$(ORACLE_TILT_RUN_DIR) --output-dir=$(ORACLE_TILT_PUBLICATION_EVIDENCE_DIR) --replace

oracle-tilt-v2-preflight:
	RQR_ORACLE_TILT_V2_OUTPUT_DIR=$(ORACLE_TILT_V2_DIR)/preflight application/scripts/41_run_oracle_tilt_publication_v2.sh preflight

oracle-tilt-v2-reference:
	RQR_ORACLE_TILT_V2_OUTPUT_DIR=$(ORACLE_TILT_V2_DIR)/reference-only application/scripts/41_run_oracle_tilt_publication_v2.sh reference-only

oracle-tilt-v2-benchmark:
	RQR_ORACLE_TILT_V2_OUTPUT_DIR=$(ORACLE_TILT_V2_DIR)/benchmark application/scripts/41_run_oracle_tilt_publication_v2.sh benchmark

oracle-tilt-v2-execute:
	RQR_ORACLE_TILT_V2_OUTPUT_DIR=$(ORACLE_TILT_V2_DIR)/execute application/scripts/41_run_oracle_tilt_publication_v2.sh execute

oracle-tilt-v2-package-evidence:
	@test -n "$(strip $(ORACLE_TILT_V2_RUN_DIR))" || { echo "Set ORACLE_TILT_V2_RUN_DIR to one completed v2 execute run." >&2; exit 64; }
	$(R) application/scripts/41_package_oracle_tilt_v2_evidence.R --run-dir=$(ORACLE_TILT_V2_RUN_DIR) --output-dir=$(ORACLE_TILT_V2_EVIDENCE_DIR) --replace

oracle-tilt-v3-preflight:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/preflight application/scripts/43_run_oracle_tilt_publication_v3.sh preflight

oracle-tilt-v3-reference:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/reference-only application/scripts/43_run_oracle_tilt_publication_v3.sh reference-only

oracle-tilt-v3-benchmark:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/benchmark application/scripts/43_run_oracle_tilt_publication_v3.sh benchmark

oracle-tilt-v3-resource-rehearsal:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/resource-rehearsal application/scripts/43_run_oracle_tilt_publication_v3.sh resource-rehearsal

oracle-tilt-v3-acceptance:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/acceptance application/scripts/45_launch_oracle_tilt_v3_acceptance.sh

oracle-tilt-v3-execute:
	RQR_ORACLE_TILT_V3_OUTPUT_DIR=$(ORACLE_TILT_V3_DIR)/execute application/scripts/43_run_oracle_tilt_publication_v3.sh execute

oracle-tilt-v3-package-evidence:
	@test -n "$(strip $(ORACLE_TILT_V3_RUN_DIR))" || { echo "Set ORACLE_TILT_V3_RUN_DIR to one completed v3 execute run." >&2; exit 64; }
	$(R) application/scripts/43_package_oracle_tilt_v3_evidence.R --run-dir=$(ORACLE_TILT_V3_RUN_DIR) --output-dir=$(ORACLE_TILT_V3_EVIDENCE_DIR) --replace

oracle-tilt-v3-package-nonpromotion-evidence:
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_BASELINE_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_BASELINE_DIR." >&2; exit 64; }
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR." >&2; exit 64; }
	$(R) application/scripts/50_package_oracle_tilt_v3_nonpromotion_evidence.R --baseline-dir=$(ORACLE_TILT_DLM_SH_BASELINE_DIR) --adjudication-dir=$(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR) --output-dir=$(ORACLE_TILT_V3_NONPROMOTION_EVIDENCE_DIR) --replace

oracle-tilt-v3-promote-revised-evidence:
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_BASELINE_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_BASELINE_DIR." >&2; exit 64; }
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR." >&2; exit 64; }
	$(R) application/scripts/51_promote_oracle_tilt_v3_revised_evidence.R --baseline-dir=$(ORACLE_TILT_DLM_SH_BASELINE_DIR) --adjudication-dir=$(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR) --policy=$(ORACLE_TILT_V3_ACCEPTANCE_POLICY) --output-dir=$(ORACLE_TILT_V3_EVIDENCE_DIR) --replace

oracle-tilt-v4-preflight:
	RQR_ORACLE_TILT_V4_CONFIG=$(ORACLE_TILT_V4_CONFIG) RQR_ORACLE_TILT_V4_OUTPUT_DIR=$(ORACLE_TILT_V4_DIR)/preflight application/scripts/53_run_oracle_tilt_publication_v4.sh preflight

oracle-tilt-v4-reference:
	RQR_ORACLE_TILT_V4_CONFIG=$(ORACLE_TILT_V4_CONFIG) RQR_ORACLE_TILT_V4_OUTPUT_DIR=$(ORACLE_TILT_V4_DIR)/reference-only application/scripts/53_run_oracle_tilt_publication_v4.sh reference-only

oracle-tilt-v4-benchmark:
	RQR_ORACLE_TILT_V4_CONFIG=$(ORACLE_TILT_V4_CONFIG) RQR_ORACLE_TILT_V4_OUTPUT_DIR=$(ORACLE_TILT_V4_DIR)/benchmark application/scripts/53_run_oracle_tilt_publication_v4.sh benchmark

oracle-tilt-v4-resource-rehearsal:
	RQR_ORACLE_TILT_V4_CONFIG=$(ORACLE_TILT_V4_CONFIG) RQR_ORACLE_TILT_V4_OUTPUT_DIR=$(ORACLE_TILT_V4_DIR)/resource-rehearsal application/scripts/53_run_oracle_tilt_publication_v4.sh resource-rehearsal

oracle-tilt-v4-launch:
	application/scripts/55_launch_oracle_tilt_v4_overnight.sh

oracle-tilt-v4-select:
	@test -n "$(strip $(ORACLE_TILT_V4_RUN_DIR))" || { echo "Set ORACLE_TILT_V4_RUN_DIR to one completed V4 execute run." >&2; exit 64; }
	$(R) application/scripts/56_select_oracle_tilt_v4_candidates.R --run-dir=$(ORACLE_TILT_V4_RUN_DIR) $(if $(strip $(ORACLE_TILT_V4_SELECTOR_DIR)),--output-dir=$(ORACLE_TILT_V4_SELECTOR_DIR),)

oracle-tilt-v4-package-evidence:
	@test -n "$(strip $(ORACLE_TILT_V4_RUN_DIR))" || { echo "Set ORACLE_TILT_V4_RUN_DIR to one completed V4 execute run." >&2; exit 64; }
	$(R) application/scripts/57_package_oracle_tilt_v4_evidence.R --run-dir=$(ORACLE_TILT_V4_RUN_DIR) --output-dir=$(ORACLE_TILT_V4_EVIDENCE_DIR) --replace

oracle-tilt-v5-preflight: package-install
	RQR_ORACLE_TILT_V5_OUTPUT_DIR=$(ORACLE_TILT_V5_DIR)/preflight application/scripts/58_run_oracle_tilt_v5.sh preflight

oracle-tilt-v5-reference: package-install
	RQR_ORACLE_TILT_V5_OUTPUT_DIR=$(ORACLE_TILT_V5_DIR)/reference-only application/scripts/58_run_oracle_tilt_v5.sh reference-only

oracle-tilt-v5-benchmark:
	RQR_ORACLE_TILT_V5_PREFLIGHT_DIR=$(ORACLE_TILT_V5_DIR)/preflight RQR_ORACLE_TILT_V5_REFERENCE_DIR=$(ORACLE_TILT_V5_DIR)/reference-only RQR_ORACLE_TILT_V5_OUTPUT_DIR=$(ORACLE_TILT_V5_DIR)/benchmark application/scripts/58_run_oracle_tilt_v5.sh benchmark

oracle-tilt-v5-execute:
	RQR_ORACLE_TILT_V5_PREFLIGHT_DIR=$(ORACLE_TILT_V5_DIR)/preflight RQR_ORACLE_TILT_V5_REFERENCE_DIR=$(ORACLE_TILT_V5_DIR)/reference-only RQR_ORACLE_TILT_V5_BENCHMARK_DIR=$(ORACLE_TILT_V5_DIR)/benchmark RQR_ORACLE_TILT_V5_OUTPUT_DIR=$(ORACLE_TILT_V5_DIR)/execute application/scripts/58_run_oracle_tilt_v5.sh execute

oracle-tilt-v5-package-evidence:
	@test -n "$(strip $(ORACLE_TILT_V5_RUN_DIR))" || { echo "Set ORACLE_TILT_V5_RUN_DIR to one completed V5 execute run." >&2; exit 64; }
	$(R) application/scripts/59_package_oracle_tilt_v5_evidence.R --run-dir=$(ORACLE_TILT_V5_RUN_DIR) --output-dir=$(ORACLE_TILT_V5_EVIDENCE_DIR)

test-oracle-mean-tilt-validation: package-install
	$(R) -e 'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-mean-tilt-validation.R", reporter = "summary")'

oracle-mean-tilt-validation-preflight: package-install
	RQR_OMTV_CONFIG=$(ORACLE_MEAN_TILT_VALIDATION_CONFIG) RQR_OMTV_OUTPUT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/preflight application/scripts/60_run_oracle_mean_tilt_validation.sh preflight

oracle-mean-tilt-validation-reference: package-install
	RQR_OMTV_CONFIG=$(ORACLE_MEAN_TILT_VALIDATION_CONFIG) RQR_OMTV_OUTPUT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/reference-only application/scripts/60_run_oracle_mean_tilt_validation.sh reference-only

oracle-mean-tilt-validation-benchmark:
	RQR_OMTV_CONFIG=$(ORACLE_MEAN_TILT_VALIDATION_CONFIG) RQR_OMTV_PREFLIGHT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/preflight RQR_OMTV_REFERENCE_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/reference-only RQR_OMTV_OUTPUT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/benchmark application/scripts/60_run_oracle_mean_tilt_validation.sh benchmark

oracle-mean-tilt-validation-sentinel:
	RQR_OMTV_CONFIG=$(ORACLE_MEAN_TILT_VALIDATION_CONFIG) RQR_OMTV_PREFLIGHT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/preflight RQR_OMTV_REFERENCE_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/reference-only RQR_OMTV_BENCHMARK_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/benchmark RQR_OMTV_OUTPUT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/sentinel application/scripts/60_run_oracle_mean_tilt_validation.sh sentinel

oracle-mean-tilt-validation-execute-wave:
	@test -n "$${RQR_OMTV_WAVE:-}" || { echo "Set RQR_OMTV_WAVE to a frozen wave number." >&2; exit 64; }
	RQR_OMTV_CONFIG=$(ORACLE_MEAN_TILT_VALIDATION_CONFIG) RQR_OMTV_PREFLIGHT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/preflight RQR_OMTV_REFERENCE_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/reference-only RQR_OMTV_BENCHMARK_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/benchmark RQR_OMTV_SENTINEL_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/sentinel RQR_OMTV_OUTPUT_DIR=$(ORACLE_MEAN_TILT_VALIDATION_DIR)/execute application/scripts/60_run_oracle_mean_tilt_validation.sh execute-wave

oracle-mean-tilt-validation-collect:
	@test -n "$(strip $(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR))" || { echo "Set ORACLE_MEAN_TILT_VALIDATION_RUN_DIR." >&2; exit 64; }
	$(R) application/scripts/61_collect_oracle_mean_tilt_validation.R --run-dir=$(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR)

oracle-mean-tilt-validation-precision:
	@test -n "$(strip $(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR))" || { echo "Set ORACLE_MEAN_TILT_VALIDATION_RUN_DIR." >&2; exit 64; }
	@test -n "$${RQR_OMTV_CHECKPOINT:-}" || { echo "Set RQR_OMTV_CHECKPOINT to a frozen checkpoint." >&2; exit 64; }
	$(R) application/scripts/60_run_oracle_mean_tilt_validation.R --mode=precision-check --config=$(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR)/config.json --run-dir=$(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR) --checkpoint=$${RQR_OMTV_CHECKPOINT}

oracle-mean-tilt-validation-package:
	@test -n "$(strip $(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR))" || { echo "Set ORACLE_MEAN_TILT_VALIDATION_RUN_DIR." >&2; exit 64; }
	$(R) application/scripts/62_package_oracle_mean_tilt_validation.R --run-dir=$(ORACLE_MEAN_TILT_VALIDATION_RUN_DIR) --output-dir=$(ORACLE_MEAN_TILT_VALIDATION_EVIDENCE_DIR)

oracle-tilt-dlm-sh-adjudication-preflight:
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_BASELINE_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_BASELINE_DIR." >&2; exit 64; }
	$(R) application/scripts/46_run_oracle_tilt_dlm_sh_adjudication.R --mode=preflight --config=$(ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIG) --baseline-dir=$(ORACLE_TILT_DLM_SH_BASELINE_DIR) --output-dir=$(ORACLE_TILT_DLM_SH_ADJUDICATION_DIR)/preflight

oracle-tilt-dlm-sh-adjudication-execute:
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_BASELINE_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_BASELINE_DIR." >&2; exit 64; }
	RQR_ORACLE_TILT_V3_BASELINE_DIR=$(ORACLE_TILT_DLM_SH_BASELINE_DIR) RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_OUTPUT_DIR=$(ORACLE_TILT_DLM_SH_ADJUDICATION_DIR)/execute application/scripts/43_run_oracle_tilt_publication_v3.sh adjudication

oracle-tilt-dlm-sh-reconcile-evidence:
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_BASELINE_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_BASELINE_DIR." >&2; exit 64; }
	@test -n "$(strip $(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR))" || { echo "Set ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR." >&2; exit 64; }
	$(R) application/scripts/48_reconcile_oracle_tilt_dlm_sh_evidence.R --baseline-dir=$(ORACLE_TILT_DLM_SH_BASELINE_DIR) --adjudication-dir=$(ORACLE_TILT_DLM_SH_ADJUDICATION_RUN_DIR) --output-dir=$(ORACLE_TILT_V3_EVIDENCE_DIR) --replace

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
	bash application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh preflight application/outputs/rqr_dlm_main_simulation_20260724/preflight

oracle-reference-dlm-confirmatory:
	bash application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh oracle-reference application/outputs/rqr_dlm_main_simulation_20260724/oracle-reference

validate-dlm-main-wave1-correction: package-install
	$(R) application/scripts/22_validate_rqr_dlm_wave1_corrections.R

validate-dlm-main-wave1-comparator: package-install
	$(R) application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R

validate-dlm-main-wave2-correction: package-install
	RQR_CORRECTION_WAVE_ID=local_level_gaussian_T200__target0200__sentinel \
	$(R) application/scripts/22_validate_rqr_dlm_wave1_corrections.R

validate-dlm-main-wave2-comparator: package-install
	RQR_CORRECTION_WAVE_ID=local_level_gaussian_T200__target0200__sentinel \
	$(R) application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R

validate-dlm-main-horizon-fixed-design: package-install
	$(R) application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R

preflight-dlm-main-wave2-m03-m08-stress: package-install
	$(R) application/scripts/39_validate_rqr_dlm_wave2_m03_m08_stress.R \
		--mode=preflight \
		--seed-ledger=$${RQR_CONFIRMATORY_SEED_LEDGER}

validate-dlm-main-wave2-m03-m08-stress: package-install
	$(R) application/scripts/39_validate_rqr_dlm_wave2_m03_m08_stress.R \
		--mode=execute \
		--seed-ledger=$${RQR_CONFIRMATORY_SEED_LEDGER}

validate-dlm-main-wave2-m03-m08-full: package-install
	$(R) application/scripts/39_validate_rqr_dlm_wave2_m03_m08_stress.R \
		--mode=execute --scope=full-wave \
		--seed-ledger=$${RQR_CONFIRMATORY_SEED_LEDGER}

validate-dlm-main-resource-envelope: package-install
	$(R) application/scripts/25_validate_rqr_dlm_resource_envelope.R

validate-dlm-m02-diagnostic-canary:
	@test -n "$(RQR_DLM_M02_CANARY_OUTPUT_ROOT)" || \
		(echo "RQR_DLM_M02_CANARY_OUTPUT_ROOT is required"; exit 1)
	$(R) application/scripts/59_validate_rqr_dlm_m02_diagnostic_thinning.R \
		"$(RQR_DLM_M02_CANARY_OUTPUT_ROOT)"

failclosed-dlm-confirmatory:
	@! $(R) application/scripts/15_run_rqr_dlm_confirmatory_simulation.R execute-confirmatory application/outputs/rqr_dlm_main_simulation_20260724/forbidden-execution

failclosed-dlm-confirmatory-wave:
	@! $(R) application/scripts/17_launch_rqr_dlm_confirmatory_wave.R execute-confirmatory application/outputs/rqr_dlm_main_simulation_20260724/preflight/execution_wave_plan_maximum.csv invalid-wave application/outputs/rqr_dlm_main_simulation_20260724/forbidden-wave

test-dlm-confirmatory-monitor:
	bash application/scripts/16_test_rqr_dlm_confirmatory_monitor.sh

launch-dlm-affected-wave-validation:
	@test -n "$(RQR_DLM_AFFECTED_PRIMARY_ATTESTATION)" || \
		(echo "RQR_DLM_AFFECTED_PRIMARY_ATTESTATION is required"; exit 1)
	@test -n "$(RQR_DLM_AFFECTED_EXDQLM_ATTESTATION)" || \
		(echo "RQR_DLM_AFFECTED_EXDQLM_ATTESTATION is required"; exit 1)
	@test -n "$(RQR_DLM_AFFECTED_QUANTREG_ATTESTATION)" || \
		(echo "RQR_DLM_AFFECTED_QUANTREG_ATTESTATION is required"; exit 1)
	@test -n "$(RQR_DLM_AFFECTED_OUTPUT_ROOT)" || \
		(echo "RQR_DLM_AFFECTED_OUTPUT_ROOT is required"; exit 1)
	application/scripts/52_launch_rqr_dlm_affected_wave_validation.sh \
		"$(RQR_DLM_AFFECTED_PRIMARY_ATTESTATION)" \
		"$(RQR_DLM_AFFECTED_EXDQLM_ATTESTATION)" \
		"$(RQR_DLM_AFFECTED_QUANTREG_ATTESTATION)" \
		"$(RQR_DLM_AFFECTED_OUTPUT_ROOT)"

health-dlm-affected-wave-validation:
	@test -n "$(RQR_DLM_AFFECTED_OUTPUT_ROOT)" || \
		(echo "RQR_DLM_AFFECTED_OUTPUT_ROOT is required"; exit 1)
	$(R) application/scripts/53_healthcheck_rqr_dlm_affected_wave_validation.R \
		"$(RQR_DLM_AFFECTED_OUTPUT_ROOT)"

launch-dlm-multicomponent-recovery:
	@test -n "$(RQR_DLM_RECOVERY_PRIMARY_ATTESTATION)" || \
		(echo "RQR_DLM_RECOVERY_PRIMARY_ATTESTATION is required"; exit 1)
	@test -n "$(RQR_DLM_RECOVERY_OUTPUT_ROOT)" || \
		(echo "RQR_DLM_RECOVERY_OUTPUT_ROOT is required"; exit 1)
	application/scripts/56_launch_rqr_dlm_multicomponent_recovery.sh \
		"$(RQR_DLM_RECOVERY_PRIMARY_ATTESTATION)" \
		"$(RQR_DLM_RECOVERY_OUTPUT_ROOT)" \
		"$(or $(RQR_DLM_RECOVERY_WORKERS),8)"

health-dlm-multicomponent-recovery:
	@test -n "$(RQR_DLM_RECOVERY_CONTROL_ROOT)" || \
		(echo "RQR_DLM_RECOVERY_CONTROL_ROOT is required"; exit 1)
	$(R) application/scripts/57_healthcheck_rqr_dlm_multicomponent_recovery.R \
		"$(RQR_DLM_RECOVERY_CONTROL_ROOT)"

launch-dlm-diagnostic-aware:
	@test -n "$(RQR_DLM_DIAGNOSTIC_LAUNCH_INPUTS)" || \
		(echo "RQR_DLM_DIAGNOSTIC_LAUNCH_INPUTS is required"; exit 1)
	@test -n "$(RQR_DLM_DIAGNOSTIC_RUN_ID)" || \
		(echo "RQR_DLM_DIAGNOSTIC_RUN_ID is required"; exit 1)
	@test -n "$(RQR_DLM_DIAGNOSTIC_RUN_ROOT)" || \
		(echo "RQR_DLM_DIAGNOSTIC_RUN_ROOT is required"; exit 1)
	@test -n "$(RQR_DLM_DIAGNOSTIC_LOG_ROOT)" || \
		(echo "RQR_DLM_DIAGNOSTIC_LOG_ROOT is required"; exit 1)
	bash application/scripts/20_launch_rqr_dlm_confirmatory_simulation.sh \
		"$(RQR_DLM_DIAGNOSTIC_LAUNCH_INPUTS)" \
		"$(RQR_DLM_DIAGNOSTIC_RUN_ID)" \
		"$(RQR_DLM_DIAGNOSTIC_RUN_ROOT)" \
		"$(RQR_DLM_DIAGNOSTIC_LOG_ROOT)"

health-dlm-diagnostic-aware:
	@test -n "$(RQR_DLM_DIAGNOSTIC_RUN_ROOT)" || \
		(echo "RQR_DLM_DIAGNOSTIC_RUN_ROOT is required"; exit 1)
	@if [ -n "$(RQR_DLM_DIAGNOSTIC_LOG_ROOT)" ]; then \
		$(R) application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R \
			"$(RQR_DLM_DIAGNOSTIC_RUN_ROOT)" \
			"$(RQR_DLM_DIAGNOSTIC_LOG_ROOT)"; \
	else \
		$(R) application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R \
			"$(RQR_DLM_DIAGNOSTIC_RUN_ROOT)"; \
	fi

literature-manifest:
	$(R) application/scripts/01_build_literature_manifest.R

clean-tex:
	rm -f *.aux *.bbl *.blg *.fdb_latexmk *.fls *.log *.out *.synctex.gz *.toc *.run.xml *.bcf
