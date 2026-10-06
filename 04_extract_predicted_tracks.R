# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 04_extract_predicted_tracks.R
# Author  : Nivia Luzuriaga
# Purpose : Extract predicted trajectories from SSM models
# ============================================================

#------------------------------------------------------------
# 1. Load project
#------------------------------------------------------------

source("scripts/00_setup.R")

#------------------------------------------------------------
# 2. Load fitted SSM models
#------------------------------------------------------------

results_ssm <- readRDS(
  "results/ssm_models.rds"
)

#------------------------------------------------------------
# 3. Extract predicted trajectories
#------------------------------------------------------------

traj_ssm <- purrr::map_dfr(
  results_ssm,
  function(x){
    
    pred <- x$fit$ssm[[1]]$predicted
    
    pred$year <- x$year
    pred$model <- x$model
    
    pred
    
  }
)

#------------------------------------------------------------
# 4. Summary
#------------------------------------------------------------

cat("\nPredicted locations:\n")

print(
  traj_ssm |>
    dplyr::count(id, year)
)

#------------------------------------------------------------
# 5. Save results
#------------------------------------------------------------

saveRDS(
  traj_ssm,
  "results/traj_ssm_wgs84.rds"
)

write.csv(
  sf::st_drop_geometry(traj_ssm),
  "results/traj_ssm_wgs84.csv",
  row.names = FALSE
)

cat("\nPredicted trajectories exported successfully.\n")
list.files("results")
#------------------------------------------------------------
traj_ssm <- readRDS("results/traj_ssm_wgs84.rds")

st_crs(traj_ssm)
#------------------------------------------------------------
#------------------------Transformar---------------------------
traj_utm <- st_transform(
  traj_ssm,
  32717
)

st_crs(traj_utm)
