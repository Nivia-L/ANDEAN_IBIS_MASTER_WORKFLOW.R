# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 06_home_range_KDE.R
# Author  : Nivia Luzuriaga
# Purpose : Kernel Density Estimation (50, 70, 95%)
# ============================================================

#------------------------------------------------------------
# 1. Load project
#------------------------------------------------------------

source("scripts/00_setup.R")
#------------------------------------------------------------
# 2. Load predicted trajectories
#------------------------------------------------------------

traj_ssm <- readRDS(
  "results/traj_ssm_wgs84.rds"
)

cat("\nPredicted trajectories loaded.\n")

print(
  traj_ssm |>
    sf::st_drop_geometry() |>
    count(id)
)
#------------------------------------------------------------
# 3. Transform to UTM
#------------------------------------------------------------

traj_utm <- st_transform(
  traj_ssm,
  32717
)
#------------------------------------------------------
#Paso 4 (sin cambios importantes)
#------------------------------------------------------
traj_sp <- as(traj_utm, "Spatial")

#------------------------------------------------------
#Paso Paso 5 y 6
#------------------------------------------------------
#------------------------------------------------------------
# 5. KDE parameters
#------------------------------------------------------------

library(adehabitatHR)

kde_h <- "href"
kde_grid <- 500
kde_extent <- 3

ids <- sort(unique(traj_sp$id))

cat("\nIndividuals:\n")
print(ids)
#------------------------------------------------------------
# 6. Calculate KDE
#------------------------------------------------------------

results_kde <- list()

for(id in ids){
  
  cat("\n-----------------------------\n")
  cat("Individual:", id, "\n")
  
  datos_id <- traj_sp[traj_sp$id == id, ]
  
  cat("Locations:", nrow(datos_id), "\n")
  
  kud <- kernelUD(
    datos_id,
    h = kde_h,
    grid = kde_grid,
    extent = kde_extent
  )
  
  results_kde[[id]] <- list(
    kud = kud,
    kde50 = getverticeshr(kud, percent = 50),
    kde70 = getverticeshr(kud, percent = 70),
    kde95 = getverticeshr(kud, percent = 95)
  )
  
  cat("Completed.\n")
  
}
#------------------------------------------------------------
#------------------------------------------------------------
kde50_list <- Map(function(x,id){ sf <- st_as_sf(x$kde50); sf$id <- id; st_set_crs(sf,32717) }, results_kde, names(results_kde))
kde70_list <- Map(function(x,id){ sf <- st_as_sf(x$kde70); sf$id <- id; st_set_crs(sf,32717) }, results_kde, names(results_kde))
kde95_list <- Map(function(x,id){ sf <- st_as_sf(x$kde95); sf$id <- id; st_set_crs(sf,32717) }, results_kde, names(results_kde))
kde50 <- do.call(rbind, kde50_list)

kde70 <- do.call(rbind, kde70_list)

kde95 <- do.call(rbind, kde95_list)
#------------------------------------------------------------
# 9. Validation
#------------------------------------------------------------

cat("\nKDE polygons:\n")

cat("50%:", nrow(kde50), "\n")
cat("70%:", nrow(kde70), "\n")
cat("95%:", nrow(kde95), "\n")

print(st_crs(kde95))
#------------------------------------------------------------
# 10. Calculate areas
#------------------------------------------------------------

calc_area <- function(x){
  
  x$area_m2 <- as.numeric(st_area(x))
  
  x$area_km2 <- x$area_m2 / 1e6
  
  x
  
}

kde50 <- calc_area(kde50)

kde70 <- calc_area(kde70)

kde95 <- calc_area(kde95)
#------------------------------------------------------------
# 11. Export results
#------------------------------------------------------------

saveRDS(kde50,"results/kde50.rds")
saveRDS(kde70,"results/kde70.rds")
saveRDS(kde95,"results/kde95.rds")

st_write(
  kde50,
  "results/kde50.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  kde70,
  "results/kde70.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  kde95,
  "results/kde95.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
#------------------------------------------------------------
# 12. Summary table
#------------------------------------------------------------

kde_summary <- bind_rows(
  
  st_drop_geometry(kde50) |> mutate(level = 50),
  
  st_drop_geometry(kde70) |> mutate(level = 70),
  
  st_drop_geometry(kde95) |> mutate(level = 95)
  
)

write.csv(
  kde_summary,
  "results/kde_summary.csv",
  row.names = FALSE
)
exists("kde50")
exists("kde70")
exists("kde95")
st_crs(kde50)
st_crs(kde70)
st_crs(kde95)
sf::st_drop_geometry(kde95)
#--------------------------------------------------
sf::st_drop_geometry(kde70)

sf::st_drop_geometry(kde50)
#---------------------------------------------------
mcp_summary <- read.csv(
  "results/mcp_individual_summary.csv"
)

mcp_summary
#---------------------------------------
library(ggplot2)

ggplot() +
  geom_sf(data = kde95, aes(fill = id), alpha = 0.25, color = "black") +
  geom_sf(data = kde70, aes(color = id), fill = NA, linewidth = 0.8) +
  geom_sf(data = kde50, aes(color = id), fill = NA, linewidth = 1.1) +
  geom_sf(data = traj_utm, aes(color = id), size = 0.3) +
  theme_bw()
#-----------------------------------------------------
#Validacion automática
#------------------------------------------------------------
# Final validation
#------------------------------------------------------------

stopifnot(
  inherits(kde50, "sf"),
  inherits(kde70, "sf"),
  inherits(kde95, "sf")
)

stopifnot(
  nrow(kde50) == length(ids),
  nrow(kde70) == length(ids),
  nrow(kde95) == length(ids)
)

cat("\nKDE analysis completed successfully.\n")



