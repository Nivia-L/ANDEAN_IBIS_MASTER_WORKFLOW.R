# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 02_state_space_models.R
# Author  : Nivia Luzuriaga
# Purpose : Fit State-Space Models (aniMotum)
# ============================================================

#------------------------------------------------------------
# 1. Load project
#------------------------------------------------------------

source("scripts/00_setup.R")

track_analysis <- readRDS("data/processed/track_analysis.rds")
#------------------------------------------------------------
# 2. Load telemetry dataset
#------------------------------------------------------------

track_overlap <- track_analysis |>
  filter(
    date >= as.POSIXct("2020-02-28", tz="UTC"),
    date <= as.POSIXct("2021-06-24", tz="UTC")
  )
#---BLOQUE 3----Bloque 3----------------------------
# 3. Create common analysis period

#------------------------------------------------------------
stopifnot(
  
  sum(is.na(track_overlap$lon))==0,
  
  sum(is.na(track_overlap$lat))==0,
  
  sum(is.na(track_overlap$date))==0
  
)
#------------------------------------------------------------
# 4. Data quality control
#------------------------------------------------------------
track_overlap_clean <- track_overlap |>
  
  distinct(id,date,lon,lat,lc,.keep_all=TRUE) |>
  
  mutate(
    
    lc_rank = recode(
  lc,
  "3" = 6,
  "2" = 5,
  "1" = 4,
  "0" = 3,
  "A" = 2,
  "B" = 1,
  .default = NA_real_
    )
    
  ) |>
  
  arrange(id,date,desc(lc_rank)) |>
  
  group_by(id,date) |>
  
  slice(1) |>
  
  ungroup() |>
  
  select(-lc_rank)
#------------------------------------------------------------
# 5. comprobacion
#------------------------------------------------------------

stopifnot(
  
  nrow(
    
    track_overlap_clean |>
      
      count(id,date) |>
      
      filter(n>1)
    
  )==0
  
)

#------------------------------------------------------------
#--------------Bloque 6_Guardar
#------------------------------------------------------------
saveRDS(
  track_overlap_clean,
  "data/processed/track_overlap_clean.rds"
)
write.csv(
  track_overlap_clean,
  "data/processed/track_overlap_clean.csv",
  row.names = FALSE
)

cat("\nClean dataset saved successfully.\n")
#------------------------------------------------------------

