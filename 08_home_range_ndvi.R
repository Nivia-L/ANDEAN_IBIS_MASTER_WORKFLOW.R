#------------------------------------------------------------
# Script 08
# Vegetation productivity (NDVI) within home ranges
#------------------------------------------------------------

#------------------------------------------------------------
# Load setup
#------------------------------------------------------------

source("scripts/00_setup.R")

#------------------------------------------------------------
# Load home-range polygons
#------------------------------------------------------------

mcp <- readRDS(
  "results/mcp_individual.rds"
)

kde50 <- readRDS(
  "results/kde50.rds"
)

kde70 <- readRDS(
  "results/kde70.rds"
)

kde95 <- readRDS(
  "results/kde95.rds"
)

#------------------------------------------------------------
# Load NDVI raster
#------------------------------------------------------------

library(terra)



#------------------------------------------------------------
# Load NDVI raster
#------------------------------------------------------------

library(terra)

ndvi_source <- "C:/Users/Nivia/Documents/ANALYSE_R_BANDURRIA_2/21Juli/MODIS Land Vegetation Indices .05deg 16d Aqua NDVI/MODIS Land Vegetation Indices .05deg 16d Aqua NDVI-20190901000000000-0-0.tif"

ndvi_target <- file.path(
  "data",
  "environmental",
  "MODIS_NDVI_20190901.tif"
)

# Copy raster to project folder if it does not exist
if (!file.exists(ndvi_target)) {
  
  dir.create(dirname(ndvi_target),
             recursive = TRUE,
             showWarnings = FALSE)
  
  file.copy(
    from = ndvi_source,
    to   = ndvi_target,
    overwrite = FALSE
  )
  
  cat("\nNDVI raster copied to project folder.\n")
  
}

#------------------------------------------------------------
# Reproject NDVI to match home-range CRS
#------------------------------------------------------------
ndvi <- rast(ndvi_target)
target_crs <- st_crs(mcp)$wkt

ndvi <- project(
  ndvi,
  target_crs,
  method = "bilinear"
)

cat("\nNDVI reprojected to EPSG:32717\n")

#------------------------------------------------------------
# Validate CRS
#------------------------------------------------------------

stopifnot(
  crs(ndvi) == st_crs(mcp)$wkt
)

cat("\nCRS validation successful\n")

#------------------------------------------------------------
# Function to summarize NDVI
#------------------------------------------------------------

extract_ndvi <- function(home_range,
                         raster,
                         method){
  
  results <- list()
  
  ids <- unique(home_range$id)
  
  for(i in seq_along(ids)){
    
    hr <- home_range |>
      dplyr::filter(id == ids[i])
    
    hr_vect <- vect(hr)
    
    ndvi_crop <- crop(raster, hr_vect)
    
    ndvi_mask <- mask(ndvi_crop, hr_vect)
    
    values <- values(
      ndvi_mask,
      na.rm = TRUE
    )
    
    values <- values[
      is.finite(values)
    ]
    
    results[[i]] <-
      
      data.frame(
        
        id = ids[i],
        
        METHOD = method,
        
        N_PIXELS = length(values),
        
        NDVI_MEAN = mean(values),
        
        NDVI_MEDIAN = median(values),
        
        NDVI_SD = sd(values),
        
        NDVI_CV = sd(values) / mean(values) * 100,
        NDVI_IQR = IQR(values),
        NDVI_MIN = min(values),
        
        NDVI_MAX = max(values)
        
      )
    
  }
  
  dplyr::bind_rows(results)
  
}

#------------------------------------------------------------
# Run extraction
#------------------------------------------------------------

mcp_ndvi <- extract_ndvi(
  mcp,
  ndvi,
  "MCP"
)

kde95_ndvi <- extract_ndvi(
  kde95,
  ndvi,
  "KDE95"
)

kde70_ndvi <- extract_ndvi(
  kde70,
  ndvi,
  "KDE70"
)

kde50_ndvi <- extract_ndvi(
  kde50,
  ndvi,
  "KDE50"
)

#------------------------------------------------------------
# Combine all results
#------------------------------------------------------------

ndvi_all <-
  
  dplyr::bind_rows(
    
    mcp_ndvi,
    
    kde95_ndvi,
    
    kde70_ndvi,
    
    kde50_ndvi
    
  )

#------------------------------------------------------------
# Validation
#------------------------------------------------------------

print(ndvi_all)

summary(ndvi_all)

#------------------------------------------------------------
# Export
#------------------------------------------------------------

write.csv(
  
  ndvi_all,
  
  "results/ndvi_summary_all_methods.csv",
  
  row.names = FALSE
  
)

cat("\nNDVI summary exported successfully.\n")


#---------------------------------------------
mcp_ndvi <- extract_ndvi(
  mcp,
  ndvi,
  "MCP"
)
kde95_ndvi <- extract_ndvi(kde95, ndvi, "KDE95")

kde70_ndvi <- extract_ndvi(kde70, ndvi, "KDE70")

kde50_ndvi <- extract_ndvi(kde50, ndvi, "KDE50")
#----------------------------------------------------------------------------------------
mcp_ndvi <- extract_ndvi(
  mcp,
  ndvi,
  "MCP"
)

kde95_ndvi <- extract_ndvi(
  kde95,
  ndvi,
  "KDE95"
)

kde70_ndvi <- extract_ndvi(
  kde70,
  ndvi,
  "KDE70"
)

kde50_ndvi <- extract_ndvi(
  kde50,
  ndvi,
  "KDE50"
)
#-------------------------------------------------------
ndvi_all <-
  
  dplyr::bind_rows(
    
    mcp_ndvi,
    
    kde95_ndvi,
    
    kde70_ndvi,
    
    kde50_ndvi
    
  )
##----------------------------------------------------
print(ndvi_all)

View(ndvi_all)

str(ndvi_all)
#------------------------------------------------
ndvi_all <-
  
  ndvi_all |>
  
  dplyr::arrange(
    
    id,
    
    factor(
      METHOD,
      levels = c(
        "MCP",
        "KDE95",
        "KDE70",
        "KDE50"
      )
    )
    
  )
#------------------------------------------------
write.csv(
  
  ndvi_all,
  
  "results/ndvi_summary_all_methods.csv",
  
  row.names = FALSE
  
)
#----------------------------------------------------
ndvi_all |>
  
  dplyr::select(
    
    id,
    
    METHOD,
    
    NDVI_MEAN,
    
    NDVI_SD,
    
    NDVI_CV,
    
    NDVI_IQR
    
  )
#----------------------------------------------------
library(ggplot2)

ggplot(
  
  ndvi_all,
  
  aes(
    
    METHOD,
    
    NDVI_MEAN,
    
    fill = id
    
  )
  
) +
  
  geom_col(
    
    position = "dodge"
    
  ) +
  
  theme_bw() +
  
  theme(
    
    panel.grid = element_blank(),
    
    legend.title = element_blank()
    
  )
#-----------------------------------------------------------
ggplot(
  
  ndvi_all,
  
  aes(
    
    METHOD,
    
    NDVI_SD,
    
    colour = id,
    
    group = id
    
  )
  
) +
  
  geom_line(
    
    linewidth = 1
    
  ) +
  
  geom_point(
    
    size = 3
    
  ) +
  
  theme_bw() +
  
  theme(
    
    panel.grid = element_blank()
    
  )
#------------------------------------------------------
#============================================================
# Figure 4. Mean NDVI ± SD
#============================================================

library(dplyr)
library(ggplot2)

plot_ndvi <- ndvi_all |>
  
  mutate(
    
    METHOD = factor(
      METHOD,
      levels = c("MCP","KDE95","KDE70","KDE50")
    ),
    
    id = factor(
      id,
      levels = c("183516","6700"),
      labels = c(
        "Argos (ID 183516)",
        "GPS-GSM (ID 6700)"
      )
    )
    
  )

#------------------------------------------------------------
# Figure
#------------------------------------------------------------

ndvi_fig <-
  
  ggplot(
    plot_ndvi,
    aes(
      x = METHOD,
      y = NDVI_MEAN,
      colour = id,
      group = id
    )
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbar(
    
    aes(
      
      ymin = NDVI_MEAN - NDVI_SD,
      
      ymax = NDVI_MEAN + NDVI_SD
      
    ),
    
    width = 0.12,
    
    linewidth = 0.6
    
  ) +
  
  labs(
    
    x = "Home-range estimator",
    
    y = "Mean NDVI",
    
    colour = NULL
    
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    panel.grid = element_blank(),
    
    panel.border = element_blank(),
    
    axis.line = element_line(
      colour = "black"
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

print(ndvi_fig)

#------------------------------------------------------------
# Save
#------------------------------------------------------------

ggsave(
  
  "results/Figure4_NDVI_mean_SD.png",
  
  plot = ndvi_fig,
  
  width = 7,
  
  height = 5,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
  
)
#-------------------------------------------------------------------------
#-otra figura
#---------------------------------------------------------------------
#============================================================
# Figure 4. Spatial variability of NDVI
#============================================================

library(dplyr)
library(ggplot2)

#------------------------------------------------------------
# Prepare data
#------------------------------------------------------------

plot_ndvi <- ndvi_all |>
  
  mutate(
    
    METHOD = factor(
      METHOD,
      levels = c("MCP", "KDE95", "KDE70", "KDE50")
    ),
    
    id = factor(
      id,
      levels = c("183516", "6700"),
      labels = c(
        "Argos (ID 183516)",
        "GPS-GSM (ID 6700)"
      )
    )
    
  )

#------------------------------------------------------------
# Figure
#------------------------------------------------------------

ndvi_sd_fig <-
  
  ggplot(
    plot_ndvi,
    aes(
      x = METHOD,
      y = NDVI_SD,
      colour = id,
      group = id
    )
  ) +
  
  geom_line(
    linewidth = 0.9
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_text(
    aes(label = sprintf("%.3f", NDVI_SD)),
    vjust = -0.9,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.02,0.10))
  ) +
  
  labs(
    
    x = "Home-range estimator",
    
    y = "NDVI standard deviation",
    
    colour = NULL
    
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    panel.grid = element_blank(),
    
    panel.border = element_blank(),
    
    axis.line = element_line(
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
    
    legend.position = "right",
    
    legend.text = element_text(size = 11))

print(ndvi_sd_fig)

#------------------------------------------------------------
# Save
#------------------------------------------------------------

ggsave(
  
  filename = "results/Figure4_NDVI_SD.png",
  
  plot = ndvi_sd_fig,
  
  width = 7,
  
  height = 5,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
  
)

