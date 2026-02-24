#!/usr/bin/env bash
set -euo pipefail

# -----------------------------
# PEPRMT PEcAn Wrapper Script
# -----------------------------
# This script delegates execution to an R script that loads the peprmt package.

# ---- CONFIG ----
R_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
R_RUN_SCRIPT="${R_SCRIPT_DIR}/peprmt_wrapper.R"

# Optional: custom R binary
R_BIN="${R_BIN:-Rscript}"

# ---- CHECKS ----
if ! command -v "$R_BIN" >/dev/null 2>&1; then
  echo "ERROR: Rscript not found in PATH"
  exit 1
fi

if [ ! -f "$R_RUN_SCRIPT" ]; then
  echo "ERROR: peprmt_wrapper.R not found at $R_RUN_SCRIPT"
  exit 1
fi

# ---- LOGGING ----
echo "====================================="
echo "PEPRMT Wrapper Starting"
echo "Date: $(date)"
echo "Host: $(hostname)"
echo "Working Dir: $(pwd)"
echo "Rscript: $(command -v $R_BIN)"
echo "Args: $@"
echo "====================================="

# ---- EXECUTE MODEL ----
# Pass all PEcAn arguments directly to R
"$R_BIN" "$R_RUN_SCRIPT" "$@"

STATUS=$?

echo "====================================="
echo "PEPRMT Wrapper Finished"
echo "Exit Code: $STATUS"
echo "====================================="

exit $STATUS
