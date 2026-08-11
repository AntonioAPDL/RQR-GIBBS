#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
stamp="${RQR_ARXIV_STAMP:-$(date +%Y%m%d_%H%M%S)}"
out_root="${repo_root}/application/cache/arxiv_preprint_${stamp}"
pkg_dir="${out_root}/rqr-gibbs-arxiv-source"
zip_path="${out_root}/rqr_gibbs_arxiv_source_${stamp}.zip"

if [[ ! -f "${repo_root}/main.bbl" ]]; then
  printf 'main.bbl is missing. Run `make pdf` before preparing the arXiv source.\n' >&2
  exit 1
fi

required_files=(
  "main.tex"
  "main.bbl"
  "refs.bib"
  "figures/generated/fig01_three_balance_principles.png"
  "figures/generated/figS01_cross_distribution_recovery.png"
  "figures/generated/fig03_mean_tilt_cf_anchors.png"
  "figures/generated/fig04_fixed_design_oracle_tilt_c095.pdf"
  "figures/generated/fig05_dlm_oracle_tilt_c095.pdf"
  "tables/mean_tilt_cf_mini_study.tex"
)

rm -rf "${out_root}"
mkdir -p "${pkg_dir}"

for rel in "${required_files[@]}"; do
  src="${repo_root}/${rel}"
  if [[ ! -f "${src}" ]]; then
    printf 'Required arXiv source file is missing: %s\n' "${rel}" >&2
    exit 1
  fi
  mkdir -p "${pkg_dir}/$(dirname "${rel}")"
  cp "${src}" "${pkg_dir}/${rel}"
done

{
  printf 'RQR-GIBBS arXiv source package\n'
  printf 'Generated: %s\n' "${stamp}"
  printf 'Top-level file: main.tex\n'
  printf 'Processor: PDFLaTeX\n'
  printf 'Notes: main.bbl is included because this project uses BibTeX/natbib.\n'
  printf 'The main-text Cornish--Fisher figure and generated population table are included.\n'
  printf 'The supplement is built separately from rqr-gibbs-supplement.tex and is not included in this main-source upload.\n'
} > "${pkg_dir}/README.txt"

(
  cd "${pkg_dir}"
  find . -type f | LC_ALL=C sort > SOURCE_MANIFEST.txt
  zip -qr "${zip_path}" .
)

sha256sum "${zip_path}" > "${zip_path}.sha256"
printf 'Created arXiv source package:\n%s\n' "${zip_path}"
printf 'SHA-256:\n'
cat "${zip_path}.sha256"
