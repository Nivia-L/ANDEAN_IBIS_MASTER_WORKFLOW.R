# Load setup
#------------------------------------------------------------
source("scripts/00_setup.R")
#------------------------------------------------------------
# Load predicted trajectories
#------------------------------------------------------------

traj_ssm <- readRDS(
  "results/traj_ssm_wgs84.rds"
)

traj_utm <- st_transform(
  traj_ssm,
  32717
)

#------------------------------------------------------------
# Packages
#------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(ggplot2)

#------------------------------------------------------------
# Load home-range polygons
#------------------------------------------------------------

mcp   <- readRDS("results/mcp_individual.rds")
kde50 <- readRDS("results/kde50.rds")
kde70 <- readRDS("results/kde70.rds")
kde95 <- readRDS("results/kde95.rds")

#------------------------------------------------------------
# Load GlobCover raster
#------------------------------------------------------------

landcover <- rast(
  "data/environmental/GlobCover2009__StudyArea.tif"
)[[1]]

#------------------------------------------------------------
# Transform home ranges to raster CRS
#------------------------------------------------------------

target_crs <- crs(landcover)

mcp   <- st_transform(mcp, target_crs)
kde50 <- st_transform(kde50, target_crs)
kde70 <- st_transform(kde70, target_crs)
kde95 <- st_transform(kde95, target_crs)

cat("\nRaster loaded successfully\n")
print(landcover)
#------------------------------------------------------------
# Project raster to UTM 17S
#------------------------------------------------------------

landcover <- project(
  landcover,
  "EPSG:32717",
  method = "near"
)

print(landcover)
#------------------------------------------------------------
# Extract land-cover composition from raster
#------------------------------------------------------------

extract_landcover_raster <- function(home_range,
                                     landcover,
                                     method){
  
  results <- list()
  
  ids <- unique(home_range$id)
  
  for(i in ids){
    
    poly <- home_range[home_range$id == i, ]
    
    r <- crop(
      landcover,
      vect(poly)
    )
    
    r <- mask(
      r,
      vect(poly)
    )
    
    f <- as.data.frame(freq(r))
    
    names(f) <- c("CLASS","COUNT")
    
    cell_area <- prod(res(r))
    
    f$AREA_HA <- f$COUNT * cell_area / 10000
    
    f$PERCENT <- 100 * f$AREA_HA / sum(f$AREA_HA)
    
    f$id <- i
    
    f$METHOD <- method
    
    results[[i]] <- f
    
  }
  
  dplyr::bind_rows(results)
  
}

mcp   <- st_transform(mcp, 32717)
kde50 <- st_transform(kde50, 32717)
kde70 <- st_transform(kde70, 32717)
kde95 <- st_transform(kde95, 32717)
st_crs(mcp)

st_bbox(mcp)
mcp_vect <- terra::vect(mcp)
terra::ext(mcp_vect)
terra::ext(landcover)
landcover_crop <- terra::crop(
  landcover,
  mcp_vect
)
landcover_mask <- terra::mask(
  landcover_crop,
  mcp_vect
)

plot(landcover_mask)
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
freq_tab <- terra::freq(landcover_mask) |>
  dplyr::rename(
    CLASS = value,
    COUNT = count
  ) |>
  dplyr::select(CLASS, COUNT)

names(freq_tab) <- c("CLASS", "COUNT")

cell_area <- prod(terra::res(landcover_mask))

freq_tab$AREA_HA <- freq_tab$COUNT * cell_area / 10000
freq_tab$AREA_KM2 <- freq_tab$AREA_HA / 100

freq_tab <- dplyr::left_join(
  freq_tab,
  globcover_classes,
  by = "CLASS"
)
freq_raw <- terra::freq(landcover_mask)

freq_raw

names(freq_raw)

str(freq_raw)

rm(freq_tab)

freq_tab <- freq_raw |>
  dplyr::rename(
    CLASS = value,
    COUNT = count
  ) |>
  dplyr::select(CLASS, COUNT)

names(freq_tab)
cell_area <- prod(terra::res(landcover_mask))

freq_tab <- freq_tab |>
  dplyr::mutate(
    AREA_HA  = COUNT * cell_area / 10000,
    AREA_KM2 = AREA_HA / 100
  )
names(freq_tab)
freq_tab <- dplyr::left_join(
  freq_tab,
  globcover_classes,
  by = "CLASS"
)
names(freq_tab)
freq_tab <- dplyr::arrange(
  freq_tab,
  dplyr::desc(AREA_HA)
)
which(names(freq_tab) == "")
which(is.na(names(freq_tab)))

names(freq_tab)
freq_tab <- freq_tab |>
  dplyr::arrange(desc(AREA_HA))

freq_tab
nrow(mcp)

mcp$id

mcp |>
  dplyr::select(id)
extract_landcover_raster <- function(home_range,
                                     landcover,
                                     method){

  out <- list()

  for(i in seq_len(nrow(home_range))){

    poly <- home_range[i, ]

    r <- crop(
      landcover,
      terra::vect(poly)
    )

    r <- mask(
      r,
      terra::vect(poly)
    )

    freq_tab <- terra::freq(r) |>
      dplyr::rename(
        CLASS = value,
        COUNT = count
      ) |>
      dplyr::select(CLASS, COUNT)

    cell_area <- prod(terra::res(r))

    freq_tab <- freq_tab |>
      dplyr::mutate(
        AREA_HA  = COUNT * cell_area / 10000,
        AREA_KM2 = AREA_HA / 100,
        PERCENT  = 100 * AREA_HA / sum(AREA_HA),
        id       = poly$id,
        METHOD   = method
      ) |>
      dplyr::left_join(
        globcover_classes,
        by = "CLASS"
      )

    out[[i]] <- freq_tab

  }

  dplyr::bind_rows(out)

}
#---------------------------------------------------------------------------
mcp_land <- extract_landcover_raster(
  mcp,
  landcover,
  "MCP"
)

kde95_land <- extract_landcover_raster(
  kde95,
  landcover,
  "KDE95"
)

kde70_land <- extract_landcover_raster(
  kde70,
  landcover,
  "KDE70"
)

kde50_land <- extract_landcover_raster(
  kde50,
  landcover,
  "KDE50"
)
mcp_land
View(mcp_land)
mcp_land <- mcp_land |>
  dplyr::arrange(id, dplyr::desc(PERCENT))

mcp_land
#-----------------------------------------------
#----------------RESUMEN POR INDIVIDUO
#----------------------------------------------
mcp_land |>
  dplyr::select(
    id,
    CLASSNAME,
    AREA_HA,
    AREA_KM2,
    PERCENT
  )
mcp |>
  dplyr::mutate(
    MCP_KM2 = as.numeric(sf::st_area(geometry)) / 1e6
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(id, MCP_KM2)
landcover_all <- bind_rows(
  mcp_land,
  kde95_land,
  kde70_land,
  kde50_land
)
summary(landcover_all)
table(landcover_all$id)
validation <- landcover_all |>
  dplyr::group_by(id, METHOD) |>
  dplyr::summarise(
    TOTAL_HA = sum(AREA_HA),
    TOTAL_KM2 = sum(AREA_KM2),
    TOTAL_PERCENT = sum(PERCENT),
    .groups = "drop"
  )

validation
validation <- landcover_all |>
  dplyr::group_by(id, METHOD) |>
  dplyr::summarise(
    TOTAL_HA = sum(AREA_HA),
    TOTAL_KM2 = sum(AREA_KM2),
    TOTAL_PERCENT = sum(PERCENT),
    .groups = "drop"
  )

write.csv(
  validation,
  "results/landcover_validation.csv",
  row.names = FALSE
)
#############EXPLORAR LOS DATOS EXTREMOS--------------------------------------
library(sf)
library(terra)

# Coordenadas de las trayectorias
coords <- sf::st_coordinates(traj_utm)

# MCP del individuo Argos
mcp_argos <- mcp[mcp$id == "183516", ]

# Vértices del MCP
vertices <- sf::st_cast(sf::st_boundary(mcp_argos), "POINT")

plot(sf::st_geometry(mcp_argos), border="red")
plot(sf::st_geometry(traj_utm[traj_utm$id=="183516",]),
     add=TRUE, pch=16, cex=.3)
plot(vertices, add=TRUE, pch=19, col="blue", cex=1.5)
##-------------------------
#----------------OBSERVACIONES DE DATOS EXTREMOS
argos <- traj_utm[traj_utm$id=="183516", ]

v <- sf::st_coordinates(vertices)

idx <- sf::st_nearest_feature(vertices, argos)

argos[idx, c("date","x.se","y.se","g")]
summary(argos$x.se)
summary(argos$y.se)

boxplot(argos$x.se)
boxplot(argos$y.se)
###############################
argos <- argos |>
  dplyr::arrange(date)

xy <- sf::st_coordinates(argos)

argos$step_km <- c(
  NA,
  sqrt(
    diff(xy[,1])^2 +
      diff(xy[,2])^2
  ) / 1000
)

summary(argos$step_km)
boxplot(argos$step_km)
hist(argos$step_km, breaks = 40)
argos |>
  dplyr::filter(step_km > 20) |>
  dplyr::select(
    date,
    step_km,
    x.se,
    y.se,
    g
  )
threshold <- quantile(argos$step_km, 0.99, na.rm = TRUE)

argos |>
  dplyr::filter(step_km > threshold)
#-----------------------TRAYECTORIA DE FECHAS EXTREMAS
###############---------------junio
argos |>
  dplyr::filter(
    date >= as.POSIXct("2020-06-10") &
      date <= as.POSIXct("2020-06-30")
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(date, step_km, x.se, y.se)
#-------------------------------------------maRZO
argos |>
  dplyr::filter(
    date >= as.POSIXct("2021-03-01") &
      date <= as.POSIXct("2021-03-20")
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(date, step_km, x.se, y.se)
#------Distancia entre posiciones consecutivas (step length)-------------------------
#FIGURA1--------------------------------------------------
library(ggplot2)
library(dplyr)

ggplot(argos, aes(date, step_km)) +
  
  geom_line(
    linewidth = 0.4,
    colour = "grey40"
  ) +
  
  geom_point(
    aes(colour = step_km > 20),
    size = 2
  ) +
  
  scale_colour_manual(
    values = c("black", "red"),
    labels = c("Normal movement", ">20 km"),
    name = NULL
  ) +
  
  geom_vline(
    xintercept = as.POSIXct(c(
      "2020-06-18",
      "2021-03-09"
    )),
    linetype = 2,
    colour = "blue"
  ) +
  
  annotate(
    "text",
    x = as.POSIXct("2020-06-20"),
    y = max(argos$step_km) * 0.97,
    label = "Long-distance\nmovement\nJune 2020",
    colour = "blue",
    size = 4
  ) +
  
  annotate(
    "text",
    x = as.POSIXct("2021-03-11"),
    y = max(argos$step_km) * 0.97,
    label = "Long-distance\nmovement\nMarch 2021",
    colour = "blue",
    size = 4
  ) +
  
  labs(
    x = "Date",
    y = "Step length (km)"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )
#------Distancia entre posiciones consecutivas (step length)-------------------------
#FIGURA2--------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

# Ordenar cronológicamente
argos <- argos |>
  arrange(date)

# Crear una línea con la trayectoria
track_line <- argos |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

# Puntos extremos
outliers <- argos |>
  filter(step_km > 20)

ggplot() +
  geom_sf(
    data = track_line,
    colour = "grey60",
    linewidth = 0.7
  ) +
  geom_sf(
    data = argos,
    colour = "black",
    size = 1.3
  ) +
  geom_sf(
    data = outliers,
    colour = "red",
    size = 3
  ) +
  theme_bw() +
  labs(
    x = "UTM Easting (m)",
    y = "UTM Northing (m)"
  )
#-----------------------FIGURAS EN utm
#----------------------------------------
ls()
st_crs(traj_utm)
st_crs(mcp)
library(ggplot2)
library(sf)

ggplot() +
  
  geom_sf(
    data = mcp,
    fill = NA,
    colour = "red",
    linewidth = 1
  ) +
  
  geom_sf(
    data = traj_utm,
    aes(colour = id),
    size = 0.8,
    alpha = 0.7
  ) +
  
  coord_sf(crs = st_crs(32717)) +
  
  labs(
    x = "UTM Easting (m)",
    y = "UTM Northing (m)",
    colour = "Individual"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "top"
  )
st_crs(argos)
st_crs(track_line)
st_crs(traj_utm)
#-----------------------------------------------
#FIGURA DE EDATOS EXTREMOS--------------------------------
#-----------------------------------------------
outliers <- argos |>
  dplyr::filter(step_km > 20)

ggplot() +
  
  geom_sf(
    data = track_line,
    colour = "grey60",
    linewidth = 0.6
  ) +
  
  geom_sf(
    data = argos,
    colour = "black",
    size = 1.2
  ) +
  
  geom_sf(
    data = outliers,
    colour = "red",
    size = 3
  ) +
  
  coord_sf(crs = st_crs(32717)) +
  
  labs(
    x = "UTM Easting (m)",
    y = "UTM Northing (m)"
  ) +
  
  theme_bw(base_size = 14)
#----------------------------------------
st_bbox(argos)
st_bbox(track_line)
st_bbox(mcp)
#--------------------------------------
packageVersion("sf")
packageVersion("ggplot2")
st_crs(argos)
#---------------------------------------------
library(ggplot2)

p <- ggplot() +
  geom_sf(data = track_line, colour = "grey60") +
  geom_sf(data = argos, colour = "black", size = 1) +
  geom_sf(data = outliers, colour = "red", size = 3) +
  theme_bw()

p
#-------------------------------
library(sf)
library(dplyr)

line_df <- track_line |>
  st_coordinates() |>
  as.data.frame()
#---------------------------------------------------------
library(sf)
library(dplyr)

point_df <- cbind(
  sf::st_drop_geometry(argos),
  sf::st_coordinates(argos)
)

head(point_df)
#----------------------------------------
out_df <- point_df |>
  dplyr::filter(step_km > 20)
#-----------------------
line_df <- sf::st_coordinates(track_line) |>
  as.data.frame()

head(line_df)
#--------------------------------------------------------
library(ggplot2)


p <- ggplot() +
  
  geom_path(
    data = line_df,
    aes(X, Y),
    colour = "grey60",
    linewidth = 0.7
  ) +
  
  geom_point(
    data = point_df,
    aes(X, Y),
    colour = "black",
    size = 1.5
  ) +
  
  geom_point(
    data = out_df,
    aes(X, Y),
    colour = "red",
    size = 3
  ) +
  
  coord_equal() +
  
  labs(
    x = "UTM Easting (m)",
    y = "UTM Northing (m)"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      linewidth = 0.8
    ),
    
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    
    legend.position = "none"
  )
ggsave(
  filename = "results/Figure_Argos_long_distance_movements.png",
  plot = p,
  width = 180,
  height = 160,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  filename = "results/Figure_Argos_long_distance_movements.tiff",
  plot = p,
  width = 180,
  height = 160,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)
#################################
extract_landcover_raster <- function(home_range,
                                     landcover,
                                     method){
  
  out <- list()
  
  for(i in seq_len(nrow(home_range))){
    
    poly <- home_range[i, ]
    
    r <- terra::crop(
      landcover,
      terra::vect(poly)
    )
    
    r <- terra::mask(
      r,
      terra::vect(poly)
    )
    
    freq_tab <-
      terra::freq(r) |>
      dplyr::rename(
        CLASS = value,
        COUNT = count
      ) |>
      dplyr::select(CLASS, COUNT)
    
    cell_area <- prod(terra::res(r))
    
    freq_tab <-
      freq_tab |>
      dplyr::mutate(
        
        AREA_HA = COUNT * cell_area / 10000,
        
        AREA_KM2 = AREA_HA / 100,
        
        PERCENT = 100 * AREA_HA / sum(AREA_HA),
        
        id = poly$id,
        
        METHOD = method
        
      )
    
    out[[i]] <- freq_tab
    
  }
  
  dplyr::bind_rows(out)
  
}
mcp_land <- extract_landcover_raster(
  mcp,
  landcover,
  "MCP"
)

kde95_land <- extract_landcover_raster(
  kde95,
  landcover,
  "KDE95"
)

kde70_land <- extract_landcover_raster(
  kde70,
  landcover,
  "KDE70"
)

kde50_land <- extract_landcover_raster(
  kde50,
  landcover,
  "KDE50"
)
landcover_all <-
  dplyr::bind_rows(
    mcp_land,
    kde95_land,
    kde70_land,
    kde50_land
  )
#--------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)

#------------------------------------------------------------
# Prepare data (MCP only)
#------------------------------------------------------------
names(mcp_land)
names(globcover_classes)
names(landcover_all)
landcover_all <- landcover_all |>
  dplyr::left_join(
    globcover_classes,
    by = "CLASS"
  )
names(landcover_all)
class(landcover_all$CLASS)

class(globcover_classes$CLASS)

unique(landcover_all$CLASS)[1:10]

unique(globcover_classes$CLASS)
tmp <- dplyr::left_join(
  landcover_all,
  globcover_classes,
  by = "CLASS"
)
landcover_all <- tmp |>
  dplyr::mutate(
    CLASSNAME = dplyr::coalesce(CLASSNAME.x, CLASSNAME.y)
  ) |>
  dplyr::select(
    -CLASSNAME.x,
    -CLASSNAME.y
  )
names(landcover_all)
#--------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)

#------------------------------------------------------------
# MCP only
#------------------------------------------------------------

plot_land <- landcover_all |>
  dplyr::filter(METHOD == "MCP") |>
  dplyr::select(id, CLASSNAME, PERCENT)

# Cambiar nombres de individuos
plot_land$id <- factor(
  plot_land$id,
  levels = c("183516", "6700"),
  labels = c("Argos (ID 183516)",
             "GPS-GSM (ID 6700)")
)

# Ordenar las clases por abundancia total
plot_land$CLASSNAME <-
  forcats::fct_reorder(
    plot_land$CLASSNAME,
    plot_land$PERCENT,
    .fun = sum,
    .desc = TRUE
  )

#------------------------------------------------------------
# Figure
#------------------------------------------------------------

landcover_fig <-
  
  ggplot(plot_land,
         aes(x = id,
             y = PERCENT,
             fill = CLASSNAME)) +
  
  geom_col(
    width = 0.65,
    colour = "white",
    linewidth = 0.3
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    limits = c(0,100),
    expand = c(0,0)
  ) +
  
  labs(
    x = NULL,
    y = "Land-cover composition (%)",
    fill = "Land-cover class"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    
    panel.grid = element_blank(),
    
    axis.text = element_text(colour = "black"),
    
    axis.title = element_text(face = "bold"),
    
    legend.title = element_text(face = "bold"),
    
    legend.position = "right"
    
  )

landcover_fig
#########################################
#---------------------------------------------
#============================================================
# Figure 3. Land-cover composition within MCP home ranges
#============================================================

library(dplyr)
library(ggplot2)
library(forcats)

#------------------------------------------------------------
# Prepare data
#------------------------------------------------------------

plot_land <- landcover_all |>
  filter(METHOD == "MCP") |>
  select(id, CLASSNAME, PERCENT)

# Etiquetas de individuos
plot_land$id <- factor(
  plot_land$id,
  levels = c("183516", "6700"),
  labels = c(
    "Argos (ID 183516)",
    "GPS-GSM (ID 6700)"
  )
)

# Ordenar coberturas por abundancia
order_levels <- plot_land |>
  group_by(CLASSNAME) |>
  summarise(
    MAX = max(PERCENT),
    .groups = "drop"
  ) |>
  arrange(desc(MAX)) |>
  pull(CLASSNAME)

plot_land$CLASSNAME <- factor(
  plot_land$CLASSNAME,
  levels = order_levels
)

#------------------------------------------------------------
# Figure
#------------------------------------------------------------

landcover_fig <-
  
  ggplot(
    plot_land,
    aes(
      x = CLASSNAME,
      y = PERCENT,
      fill = id
    )
  ) +
  
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    colour = "black",
    linewidth = 0.20
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    expand = expansion(mult = c(0,0.05))
  ) +
  
  labs(
    
    x = NULL,
    
    y = "Percentage of home range (%)",
    
    fill = NULL
    
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    # Sin cuadrícula
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Sin marco
    panel.border = element_blank(),
    
    # Solo ejes
    axis.line.x = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    axis.line.y = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    axis.text = element_text(
      colour = "black",
      size = 12
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 13
    ),
    
    legend.position = "top",
    
    legend.text = element_text(size = 11)
    
  )

# Mostrar figura
print(landcover_fig)

#------------------------------------------------------------
# Save figure
#------------------------------------------------------------

ggsave(
  filename = "results/Figure3_LandCover_MCP.png",
  plot = landcover_fig,
  width = 9,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)
