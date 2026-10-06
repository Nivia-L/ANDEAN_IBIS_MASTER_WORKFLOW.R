# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 01_import_cleaning.R
# Author  : Nivia Luzuriaga
# Purpose : Import and validate telemetry dataset
# ============================================================

#-------------------------------------------------------------
# Load project configuration
#-------------------------------------------------------------

source("scripts/00_setup.R")

#-------------------------------------------------------------
# Load official dataset
#-------------------------------------------------------------

track_analysis <- readRDS(
  "data/processed/track_analysis.rds"
)

cat("\nDataset loaded successfully.\n")

#-------------------------------------------------------------
# Verify structure
#-------------------------------------------------------------

str(track_analysis)

#-------------------------------------------------------------
# Summary
#-------------------------------------------------------------

summary_table <- track_analysis %>%
  group_by(id, source) %>%
  summarise(
    fixes = n(),
    start = min(date),
    end = max(date),
    .groups = "drop"
  )

print(summary_table)

#-------------------------------------------------------------
# Basic quality control
#-------------------------------------------------------------

cat("\nNumber of records:", nrow(track_analysis), "\n")

cat("Missing longitude:",
    sum(is.na(track_analysis$lon)), "\n")

cat("Missing latitude:",
    sum(is.na(track_analysis$lat)), "\n")

cat("Missing dates:",
    sum(is.na(track_analysis$date)), "\n")

#-------------------------------------------------------------
# End
#-------------------------------------------------------------

cat("\nImport completed successfully.\n")

