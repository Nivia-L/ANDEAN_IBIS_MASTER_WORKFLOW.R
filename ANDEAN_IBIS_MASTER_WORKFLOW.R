# ============================================================
# ANDEAN IBIS HOME RANGE 2026
# MASTER REPRODUCIBLE WORKFLOW
# Species: Theristicus branickii (Andean Ibis)
# ============================================================
#
# Purpose:
#   Single entry point for the complete reproducible analysis.
#
# Requirements:
#   R >= 4.5
#   Project root must contain:
#     scripts/
#     data/
#     results/
#
# Important:
#   This workflow does NOT load .RData or depend on a hidden workspace.
#   All scripts are executed from the PROJECT ROOT so that relative paths
#   such as data/... and results/... remain valid.
# ============================================================

options(stringsAsFactors = FALSE)

# ---- 1. PROJECT ROOT ----------------------------------------

project_root <- normalizePath(
  "C:/Users/Nivia/Desktop/Andean_Ibis_HomeRange_2026",
  winslash = "/",
  mustWork = TRUE
)

setwd(project_root)

cat("\n============================================\n")
cat("ANDEAN IBIS HOME RANGE 2026\n")
cat("Project root:", project_root, "\n")
cat("R version:", R.version.string, "\n")
cat("============================================\n\n")

# ---- 2. DIRECTORIES -----------------------------------------

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# ---- 3. SCRIPT ORDER ----------------------------------------

scripts <- c(
  "00_setup.R",
  "01_import_cleaning.R",
  "02_state_space_models.R",
  "03_state_space_models.R",
  "04_extract_predicted_tracks.R",
  "05_home_range_MCP.R",
  "06_home_range_KDE.R",
  "07_home_range_landcover.R",
  "08_home_range_ndvi.R",
  "09_movement_persistence_GAM.R",
  "10_movement_metrics.R"
)

script_paths <- file.path("scripts", scripts)

missing_scripts <- script_paths[!file.exists(script_paths)]

if (length(missing_scripts) > 0) {
  stop(
    "Missing script(s):\n",
    paste(missing_scripts, collapse = "\n")
  )
}

cat("Scripts found:", length(script_paths), "\n\n")

# ---- 4. EXECUTION FUNCTION ---------------------------------

run_step <- function(script) {

  cat("\n--------------------------------------------\n")
  cat("RUNNING:", script, "\n")
  cat("--------------------------------------------\n")

  tryCatch(

    {
      # IMPORTANT:
      # chdir = FALSE keeps the project root as working directory.
      source(
        file.path("scripts", script),
        local = .GlobalEnv,
        chdir = FALSE,
        echo = FALSE
      )

      cat("STATUS: OK\n")
      TRUE
    },

    error = function(e) {

      cat("\nSTATUS: FAILED\n")
      cat("Script:", script, "\n")
      cat("Error:", conditionMessage(e), "\n")

      FALSE
    }
  )
}

# ---- 5. RUN COMPLETE PIPELINE ------------------------------

status <- vapply(
  scripts,
  run_step,
  logical(1)
)

names(status) <- scripts

cat("\n============================================\n")
cat("PIPELINE SUMMARY\n")
cat("============================================\n\n")

print(status)

# ---- 6. REQUIRED OUTPUT VALIDATION -------------------------

required_outputs <- c(
  "data/processed/track_overlap_clean.rds",
  "results/ssm_models.rds",
  "results/traj_ssm_wgs84.rds",
  "results/mcp_individual.rds"
)

cat("\nRequired outputs:\n")

for (f in required_outputs) {
  cat(
    if (file.exists(f)) "[OK]   " else "[MISS] ",
    f, "\n",
    sep = ""
  )
}

# ---- 7. SSM VALIDATION --------------------------------------

if (file.exists("results/ssm_models.rds")) {

  ssm <- readRDS("results/ssm_models.rds")

  cat("\nSSM validation:\n")
  cat("Number of fitted models:", length(ssm), "\n")

  if (length(ssm) > 0) {

    ssm_models <- vapply(
      ssm,
      function(x) {
        if (!is.null(x$model)) as.character(x$model) else NA_character_
      },
      character(1)
    )

    print(ssm_models)
  }
}

# ---- 8. TRAJECTORY VALIDATION -------------------------------

if (file.exists("results/traj_ssm_wgs84.rds")) {

  traj <- readRDS("results/traj_ssm_wgs84.rds")

  cat("\nPredicted trajectory validation:\n")
  cat("Rows:", nrow(traj), "\n")
  cat("Columns:", ncol(traj), "\n")

  if ("id" %in% names(traj)) {
    cat("Individuals:\n")
    print(table(traj$id))
  }
}

# ---- 9. MOVEMENT SUMMARY VALIDATION --------------------------

if (file.exists("results/movement_summary.csv")) {

  cat("\nMovement summary:\n")
  print(
    read.csv(
      "results/movement_summary.csv",
      stringsAsFactors = FALSE
    )
  )
}

# ---- 10. FINAL STATUS ---------------------------------------

if (all(status)) {

  cat("\n============================================\n")
  cat("PIPELINE COMPLETED SUCCESSFULLY\n")
  cat("============================================\n")

} else {

  failed <- names(status)[!status]

  cat("\n============================================\n")
  cat("PIPELINE COMPLETED WITH ERRORS\n")
  cat("Failed script(s):\n")
  cat(paste("-", failed, collapse = "\n"))
  cat("\n============================================\n")

}

cat("\nFinished:", format(Sys.time()), "\n")
cat("Project:", project_root, "\n")
