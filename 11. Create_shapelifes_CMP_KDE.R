#=========================================================
# Export Home Range polygons to Shapefiles
#=========================================================

library(sf)

#---------------------------------------------------------
# Output directory
#---------------------------------------------------------

dir.create(
  "results/Shapefiles",
  recursive = TRUE,
  showWarnings = FALSE
)

#---------------------------------------------------------
# Load saved home-range objects
#---------------------------------------------------------

mcp100 <- readRDS(
  "results/mcp_individual.rds"
)

kde95 <- readRDS(
  "results/kde95.rds"
)

kde70 <- readRDS(
  "results/kde70.rds"
)

kde50 <- readRDS(
  "results/kde50.rds"
)

#---------------------------------------------------------
# Convert MCP to sf
#---------------------------------------------------------

mcp_sf <- st_as_sf(mcp100)

#---------------------------------------------------------
# Export MCP100 by individual
#---------------------------------------------------------

ids <- unique(mcp_sf$id)

for (i in ids) {

  st_write(
    subset(mcp_sf, id == i),
    paste0(
      "results/Shapefiles/MCP100_",
      i,
      ".shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

}

#---------------------------------------------------------
# Export KDE95 by individual
#---------------------------------------------------------

ids <- unique(kde95$id)

for (i in ids) {

  st_write(
    subset(kde95, id == i),
    paste0(
      "results/Shapefiles/KDE95_",
      i,
      ".shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

}

#---------------------------------------------------------
# Export KDE70 by individual
#---------------------------------------------------------

ids <- unique(kde70$id)

for (i in ids) {

  st_write(
    subset(kde70, id == i),
    paste0(
      "results/Shapefiles/KDE70_",
      i,
      ".shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

}

#---------------------------------------------------------
# Export KDE50 by individual
#---------------------------------------------------------

ids <- unique(kde50$id)

for (i in ids) {

  st_write(
    subset(kde50, id == i),
    paste0(
      "results/Shapefiles/KDE50_",
      i,
      ".shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

}

#---------------------------------------------------------
# Finished
#---------------------------------------------------------

cat("\nAll home-range shapefiles exported successfully.\n")
