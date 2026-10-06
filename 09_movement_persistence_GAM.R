#============================================================
# 09. Movement persistence and habitat predictors
#============================================================
#------------------------------------------------------------
# 1. Packages
#------------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(mgcv)
library(ggplot2)
library(readr)

#------------------------------------------------------------
# 2. Project directory
#------------------------------------------------------------

proj_dir <- "C:/Users/Nivia/Desktop/Andean_Ibis_HomeRange_2026"
#------------------------------------------------------------
# 3. Load trajectories
#------------------------------------------------------------

traj_ssm <- readRDS(
  file.path(
    proj_dir,
    "results",
    "traj_ssm_wgs84.rds"
  )
)
#------------------------------------------------------------
# Transform trajectories to UTM 17S for metric calculations
#------------------------------------------------------------

traj_utm <- st_transform(
  traj_ssm,
  32717
)
#------------------------------------------------------------
# 4. Load environmental rasters
#------------------------------------------------------------

ndvi <- rast(
  file.path(
    proj_dir,
    "data",
    "environmental",
    "MODIS_NDVI_20190901.tif"
  )
)

landcover <- rast(
  file.path(
    proj_dir,
    "data",
    "environmental",
    "GlobCover2009__StudyArea.tif"
  )
)
#------------------------------------------------------------
# 5. Extract NDVI
#------------------------------------------------------------

traj_ssm$NDVI <- terra::extract(
  ndvi,
  terra::vect(traj_ssm)
)[,2]

summary(traj_ssm$NDVI)
#------------------------------------------------------------
# 6. Extract land-cover
#------------------------------------------------------------

traj_ssm$CLASS <- terra::extract(
  landcover,
  terra::vect(traj_ssm)
)[,2]

table(traj_ssm$CLASS)
#----------------------------------------------------------
globcover_classes <- tibble::tribble(
  ~CLASS, ~CLASSNAME,
  11, "Post-flooding or irrigated croplands",
  14, "Rainfed croplands",
  20, "Mosaic cropland/vegetation",
  30, "Mosaic vegetation/cropland",
  40, "Broadleaved evergreen forest",
  50, "Broadleaved deciduous forest",
  60, "Open broadleaved deciduous forest",
  70, "Needleleaved evergreen forest",
  90, "Mixed needleleaved forest",
  100, "Mixed forest",
  110, "Mosaic forest/shrubland",
  120, "Mosaic grassland/forest",
  130, "Shrubland",
  140, "Grassland",
  150, "Sparse vegetation",
  160, "Flooded freshwater forest",
  170, "Flooded saline forest",
  180, "Flooded vegetation",
  190, "Artificial surfaces",
  200, "Bare areas",
  210, "Water bodies",
  220, "Permanent snow and ice"
)
#------------------------------------------------------------
# 7. Convert codes to land-cover names
#------------------------------------------------------------
movement_env <-
  
  traj_ssm |>
  
  sf::st_drop_geometry() |>
  
  left_join(
    globcover_classes,
    by = "CLASS"
  )
#------------------------------------------------------------
# 8. Revisar la base
#------------------------------------------------------------
glimpse(movement_env)

summary(movement_env)

table(movement_env$CLASSNAME)
summary(movement_env$NDVI)

table(movement_env$CLASSNAME)

summary(movement_env$g)

sum(is.na(movement_env))
nrow(movement_env)
#------------------------------------------------------------
# 9. Group land-cover classes
#------------------------------------------------------------

movement_env <- movement_env |>

  mutate(

    LandCover = case_when(

      CLASSNAME == "Shrubland" ~ "Shrubland",

      CLASSNAME == "Mosaic cropland/vegetation" ~
        "Mosaic cropland",

      CLASSNAME == "Grassland" ~
        "Grassland",

      CLASSNAME == "Broadleaved evergreen forest" ~
        "Evergreen forest",

      TRUE ~ "Other"

    )

  )

movement_env$LandCover <- factor(
  movement_env$LandCover,
  levels = c(
    "Shrubland",
    "Mosaic cropland",
    "Grassland",
    "Evergreen forest",
    "Other"
  )
)

#------------------------------------------------------------
# Check frequencies
#------------------------------------------------------------

table(movement_env$LandCover)
#------------------------------------------------------------
# 10 Exploratory analysis
#------------------------------------------------------------

summary(movement_env)

summary(movement_env$g)

summary(movement_env$NDVI)

table(movement_env$LandCover)

colSums(is.na(movement_env))
#------------------------------------------------------------
# 11. Initial GAM-nullmodel
#------------------------------------------------------------

gam1 <- mgcv::gam(
  
  g ~
    s(NDVI, k = 20) +
    LandCover,
  
  data = movement_env,
  
  method = "REML"
  
)

summary(gam1)
#------------------------------------------------------------
# Diagnostics
#------------------------------------------------------------

gam.check(gam1)

AIC(gam1)

logLik(gam1)
#-------------------------------------------
gam2 <- gam(
  g ~ s(NDVI, k = 20),
  data = movement_env,
  method = "REML"
)
AIC(gam1, gam2)
anova(gam1, gam2, test = "Chisq")
#-----------------------------------------
gam_final <- gam(
  g ~ s(NDVI, k = 20),
  data = movement_env,
  method = "REML"
)
summary(gam_final)
#------------------------------------------------------------

#------------------------------------------------------------
# 12. Figure 5. GAM effect of NDVI on movement persistence
#------------------------------------------------------------

ndvi_seq <- seq(
  min(movement_env$NDVI, na.rm = TRUE),
  max(movement_env$NDVI, na.rm = TRUE),
  length.out = 300
)

pred_gam <- predict(
  gam2,
  newdata = data.frame(NDVI = ndvi_seq),
  type = "response",
  se.fit = TRUE
)

ndvi_pred <- data.frame(
  NDVI = ndvi_seq,
  fit = as.numeric(pred_gam$fit),
  se = as.numeric(pred_gam$se.fit)
) |>
  dplyr::mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

Figure5 <- ggplot2::ggplot(
  ndvi_pred,
  ggplot2::aes(x = NDVI, y = fit)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill = "grey75",
    alpha = 0.35
  ) +
  ggplot2::geom_line(
    linewidth = 0.9,
    colour = "#2166AC"
  ) +
  ggplot2::geom_rug(
    data = movement_env,
    ggplot2::aes(x = NDVI),
    sides = "b",
    alpha = 0.18,
    length = grid::unit(0.015, "npc"),
    inherit.aes = FALSE
  ) +
  ggplot2::scale_x_continuous(
    name = "Normalized Difference Vegetation Index (NDVI)",
    breaks = seq(0.4, 0.8, 0.1),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    name = "Predicted movement persistence (gamma)",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 13),
    axis.text = ggplot2::element_text(size = 11, colour = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.5),
    axis.ticks = ggplot2::element_line(linewidth = 0.4),
    plot.margin = ggplot2::margin(8, 10, 8, 8)
  )

print(Figure5)

ggplot2::ggsave(
  filename = file.path(
    proj_dir,
    "results",
    "Figure5_GAM_NDVI.png"
  ),
  plot = Figure5,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white"
)

#------------------------------------------------------------
# 13. Figure 7. Monthly Net Squared Displacement
#------------------------------------------------------------

coords <- sf::st_coordinates(traj_utm)

nsd_plot <- traj_utm |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    X = coords[, 1],
    Y = coords[, 2]
  ) |>
  dplyr::arrange(date)

x0 <- nsd_plot$X[1]
y0 <- nsd_plot$Y[1]

nsd_plot <- nsd_plot |>
  dplyr::mutate(
    NSD = ((X - x0)^2 + (Y - y0)^2) / 1e6,
    date = as.Date(date),
    Month = lubridate::floor_date(date, "month")
  )

nsd_month <- nsd_plot |>
  dplyr::group_by(Month) |>
  dplyr::summarise(
    Median = median(NSD, na.rm = TRUE),
    P95 = quantile(NSD, 0.95, na.rm = TRUE),
    Q25 = quantile(NSD, 0.25, na.rm = TRUE),
    Q75 = quantile(NSD, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

Figure7 <- ggplot2::ggplot(
  nsd_month,
  ggplot2::aes(x = Month)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = Q25, ymax = Q75),
    fill = "grey85",
    alpha = 0.8
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = Median),
    colour = "grey40",
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = Median),
    colour = "grey40",
    size = 2.5
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = P95),
    colour = "black",
    linewidth = 1.1
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = P95),
    colour = "black",
    size = 2.8
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  ggplot2::labs(
    x = NULL,
    y = expression("Net Squared Displacement (km"^2*")")
  ) +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    )
  )

print(Figure7)

ggplot2::ggsave(
  filename = file.path(
    proj_dir,
    "results",
    "Figure7_NSD_monthly.png"
  ),
  plot = Figure7,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white"
)
