#============================================================
# 10. Movement metrics
#============================================================
# 1. Packages
#============================================================

library(sf)
library(dplyr)
library(geosphere)
library(lubridate)
library(readr)
library(ggplot2)

# 1. Load trajectories
#============================================================
proj_dir <- "C:/Users/Nivia/Desktop/Andean_Ibis_HomeRange_2026"
#------------------------------------------------------------
#2 Prepare trajectories
#============================================================
traj_ssm <- readRDS(
  file.path(
    proj_dir,
    "results",
    "traj_ssm_wgs84.rds"
  )
)
traj_utm <- st_transform(
  traj_ssm,
  32717
)

coords <- st_coordinates(traj_utm)

movement <- traj_utm %>%
  st_drop_geometry() %>%
  mutate(
    X = coords[,1],
    Y = coords[,2]
  ) %>%
  arrange(
    id,
    date
  )

#============================================================
# 3 Calculate movement metrics
#============================================================

#2--------------------------------------------------------------
library(dplyr)
library(tidyr)

movement_metrics <- movement %>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(
    
    X_prev = lag(X),
    Y_prev = lag(Y),
    date_prev = lag(date),
    
    #---------------------------------------------------------
    # Step length (km)
    #---------------------------------------------------------
   step_length = sqrt(
  	(X - X_prev)^2 +
   	(Y - Y_prev)^2
	) / 1000,
    
    #---------------------------------------------------------
    # Time interval (hours)
    #---------------------------------------------------------
    time_interval = as.numeric(
      difftime(date, date_prev, units = "hours")
    ),
    
    #---------------------------------------------------------
    # Speed (km/h)
    #---------------------------------------------------------
    speed = ifelse(
      time_interval > 0,
      step_length / time_interval,
      NA_real_
    ),
    
    #---------------------------------------------------------
    # Cumulative distance (km)
    #---------------------------------------------------------
    cum_distance = cumsum(
      replace_na(step_length, 0)
    )
    
  ) %>%
  ungroup()

#---3. Verificación------------------------------------
summary(movement_metrics$step_length)

summary(movement_metrics$time_interval)

summary(movement_metrics$speed)

summary(movement_metrics$cum_distance)


#------------------------------------------------------------
#5   Summarize by individual
#============================================================
movement_summary <- movement_metrics %>%
  group_by(id) %>%
  summarise(
    
    n_locations = n(),
    
    tracking_days =
      as.numeric(max(date)-min(date)),
    
    total_distance = max(cum_distance),
    
    mean_step = mean(step_length, na.rm=TRUE),
    
    median_step = median(step_length, na.rm=TRUE),
    
    max_step = max(step_length, na.rm=TRUE)
    
  )
print(movement_summary)




#6 Select only GPS (6700)
#============================================================
# Seleccionar solo el individuo GPS
gps <- traj_ssm %>%
  filter(id == "6700") %>%
  arrange(date)

# Extraer coordenadas del objeto sf
coords <- st_coordinates(gps)

# Convertir a data.frame y añadir coordenadas
gps <- gps %>%
  st_drop_geometry() %>%
  mutate(
    x = coords[,1],
    y = coords[,2]
  )

# Verificar
names(gps)

x0 <- gps$x[1]
y0 <- gps$y[1]

gps <- gps %>%
  mutate(
    NSD = (x - x0)^2 +
      (y - y0)^2
  )

# Centroide de las primeras 10 localizaciones
x0 <- mean(gps$x[1:10])
y0 <- mean(gps$y[1:10])

gps <- gps %>%
  mutate(
    NSD = (x - x0)^2 +
      (y - y0)^2
  )

summary(gps$NSD)

hist(gps$NSD, breaks = 40)

plot(gps$date, gps$NSD, type = "l")
plot(gps$x,
     gps$y,
     asp = 1,
     pch = 16)

points(x0,
       y0,
       pch = 19,
       col = "red",
       cex = 2)

summary(gps$x)
summary(gps$y)
#Extract coordinates
#----------------------------------------------
ggplot(gps, aes(date, NSD)) +
  geom_line(colour = "grey60", linewidth = 0.4) +
  geom_point(size = 1.2) +
  theme_classic()
#-------------------------------------------------
#============================================================
# Assign biological phases
#============================================================

library(lubridate)

gps <- gps %>%
  mutate(
    month = month(date),
    
    phase = case_when(
      month %in% c(10, 11, 12) ~ "Pre-reproductive",
      month %in% c(1, 2, 3, 4) ~ "Reproductive",
      month %in% c(5, 6, 7, 8, 9) ~ "Post-reproductive",
      TRUE ~ NA_character_
    )
  )

# Orden de las fases
gps$phase <- factor(
  gps$phase,
  levels = c(
    "Pre-reproductive",
    "Reproductive",
    "Post-reproductive"
  )
)
#-----------------------------------------------
table(gps$phase)
table(month(gps$date), gps$phase)
#-----------------------------------------------------------
nsd_summary <- gps %>%
  group_by(phase) %>%
  summarise(
    N = n(),
    Mean_NSD = mean(NSD),
    SD_NSD = sd(NSD),
    Median_NSD = median(NSD),
    Q25 = quantile(NSD, 0.25),
    Q75 = quantile(NSD, 0.75),
    Min_NSD = min(NSD),
    Max_NSD = max(NSD),
    IQR = IQR(NSD)
  )

nsd_summary
kruskal.test(NSD ~ phase, data = gps)
#___________________________________________________________________
tracking_summary <- gps %>%
  summarise(
    First_date = min(date),
    Last_date = max(date),
    Tracking_days = as.numeric(max(date) - min(date)),
    Locations = n()
  )

tracking_summary
#-------------------------------------------------------------------------------
movement_metrics %>%
  summarise(
    Total_distance = max(cum_distance, na.rm = TRUE),
    Mean_step = mean(step_length, na.rm = TRUE),
    SD_step = sd(step_length, na.rm = TRUE),
    Median_step = median(step_length, na.rm = TRUE),
    Q25 = quantile(step_length, 0.25, na.rm = TRUE),
    Q75 = quantile(step_length, 0.75, na.rm = TRUE),
    Max_step = max(step_length, na.rm = TRUE)
  )
movement_metrics %>%
  summarise(
    P95_step = quantile(step_length,0.95,na.rm=TRUE),
    P99_step = quantile(step_length,0.99,na.rm=TRUE)
  )
#----------------------------------------------------------------------
#------------------------------------------
##==========================================================
# 3. Verification
#==========================================================

summary(movement_metrics$step_length)
summary(movement_metrics$time_interval)
summary(movement_metrics$speed)
summary(movement_metrics$cum_distance)

#==========================================================
# 4. Summary of movement metrics (Table 4)
#==========================================================

tracking_summary <- summary(movement_metrics)
tracking_summary

#==========================================================
# 5. Figure 6. Distribution of step length
#==========================================================

library(ggplot2)

median_step <- median(movement_metrics$step_length, na.rm = TRUE)

p95 <- quantile(movement_metrics$step_length,
                0.95,
                na.rm = TRUE)

p99 <- quantile(movement_metrics$step_length,
                0.99,
                na.rm = TRUE)

Figure6 <- ggplot(movement_metrics,
                  aes(x = step_length)) +
  
  geom_histogram(
    bins = 40,
    fill = "grey80",
    colour = "black"
  ) +
  
  scale_x_log10() +
  
  geom_vline(
    xintercept = median_step,
    colour = "red",
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = p95,
    colour = "blue",
    linetype = 2,
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = p99,
    colour = "darkgreen",
    linetype = 3,
    linewidth = 0.8
  ) +
  
  labs(
    x = "Step length (km)",
    y = "Frequency"
  ) +
  
  theme_classic(base_size = 13)

Figure6

#----------------------------------------------------------
# Export Figure 6
#----------------------------------------------------------

ggsave(
  filename = "results/Figures/Figure6_StepLength_Histogram.png",
  plot = Figure6,
  width = 18,
  height = 14,
  units = "cm",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "results/Figures/Figure6_StepLength_Histogram.pdf",
  plot = Figure6,
  width = 18,
  height = 14,
  units = "cm",
  device = cairo_pdf
)

#----------------------------------------------------------
