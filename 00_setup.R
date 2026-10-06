# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 00_setup.R
# Author  : Nivia Luzuriaga
# Purpose : Configure project environment
# ============================================================

# Global options
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Required packages
packages <- c(
  "tidyverse",
  "sf",
  "terra",
  "lubridate",
  "aniMotum",
  "adehabitatHR",
  "readxl",
  "writexl"
)

missing <- packages[!packages %in% installed.packages()[, "Package"]]

if(length(missing) > 0){
  install.packages(missing)
}

invisible(lapply(packages, library, character.only = TRUE))

# Project directory
proj_dir <- getwd()

cat("Project directory:\n")
cat(proj_dir, "\n\n")

cat("Processed datasets:\n")
print(list.files("data/processed"))

cat("\nSetup completed successfully.\n")
