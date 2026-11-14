#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec Rscript "${script_dir}/115_prepare_mti_manuscript_source.R" "$@"
