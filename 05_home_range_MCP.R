# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 05_home_range_MCP.R
# Author  : Nivia Luzuriaga
# Purpose : Calculate Minimum Convex Polygon (MCP)
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
    count(id, year)
)
#------------------------------------------------------------
# 3. Transform to UTM 17S
#------------------------------------------------------------

traj_utm <- st_transform(
  traj_ssm,
  32717
)

cat("\nCRS:\n")

print(st_crs(traj_utm))
###############Cargar trayectorias
traj_ssm <- readRDS("results/traj_ssm_wgs84.rds")
#2. Transformar a UTM 17S
traj_utm <- st_transform(
  traj_ssm,
  32717
)
#4Eliminar la separación por año
traj_utm$id <- as.factor(traj_utm$id)
#4. Convertir a Spatial---------------------
traj_sp <- as(
  traj_utm,
  "Spatial"
)
#5. Calcular MCP-----------------------------------------
library(adehabitatHR)

mcp100 <- mcp(
  traj_sp[, "id"],
  percent = 100
)
mcp_sf <- st_as_sf(mcp100)
#-7. Calcular áreas-------------------------------------------
mcp_sf$area_m2 <- as.numeric(
  st_area(mcp_sf)
)

mcp_sf$area_km2 <-
  mcp_sf$area_m2 / 1e6
#8. Añadir número de localizaciones---------------------------
nfix <- traj_utm |>
  st_drop_geometry() |>
  dplyr::count(id)

mcp_sf <- dplyr::left_join(
  mcp_sf,
  nfix,
  by = "id"
)
#9. Guardar resultados----------------------------------------
saveRDS(
  mcp_sf,
  "results/mcp_individual.rds"
)

st_write(
  mcp_sf,
  "results/mcp_individual.gpkg",
  delete_dsn = TRUE
)

st_write(
  mcp_sf,
  "results/mcp_individual.shp",
  delete_layer = TRUE
)

write.csv(
  st_drop_geometry(mcp_sf),
  "results/mcp_individual_summary.csv",
  row.names = FALSE
)
list.files("results")
write.csv(
  sf::st_drop_geometry(mcp_sf),
  "results/mcp_individual_summary.csv",
  row.names = FALSE
)
list.files("results")
cat("\nFiles created:\n")

print(
  list.files("results")
)
cat("\n====================================\n")
cat("MCP analysis completed successfully\n")
cat("====================================\n")

cat("\nOutput files:\n")

print(list.files("results"))

cat("\nMCP summary:\n")

print(
  sf::st_drop_geometry(mcp_sf)
)
#----------------------------------------------------
#figuras--------------------------------------------------
ggplot() +
  
  geom_sf(
    data = mcp_sf,
    aes(fill = id),
    alpha = 0.25,
    color = "black"
  ) +
  
  geom_sf(
    data = traj_utm,
    aes(color = id),
    size = 0.7
  ) +
  
  facet_wrap(~id) +
  
  theme_bw()
###############
ggplot(
  st_drop_geometry(mcp_sf),
  aes(
    x = id,
    y = area_km2,
    fill = id
  )
) +
  
  geom_col(width = .6) +
  
  theme_classic()
