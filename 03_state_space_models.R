# ============================================================
# Project : Andean_Ibis_HomeRange_2026
# Script  : 03_state_space_models.R
# Author  : Nivia Luzuriaga
# Purpose : Fit State-Space Models using aniMotum
# ============================================================

#------------------------------------------------------------
# 1. Load project
#------------------------------------------------------------

source("scripts/00_setup.R")

#------------------------------------------------------------
# 2. Load cleaned telemetry dataset
#------------------------------------------------------------

track_overlap_clean <- readRDS(
  "data/processed/track_overlap_clean.rds"
)

cat("\nClean dataset loaded successfully.\n")

cat("Records:", nrow(track_overlap_clean), "\n")

print(
  track_overlap_clean |>
    count(id)
)
#------------------------------------------------------------
# 3. Create combinations: individual × year
#------------------------------------------------------------

combos <- track_overlap_clean |>
  group_by(id, year) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 30)

print(combos)
#------------------------------------------------------------
# 5. Function to fit a State-Space Model
#------------------------------------------------------------

fit_year_ssm <- function(data_sf, individuo, anio){
  
  cat("\n-----------------------------------------\n")
  cat("Individual:", individuo,
      "| Year:", anio, "\n")
  
  datos <- data_sf |>
    filter(
      id == individuo,
      year == anio
    )
  
  cat("Locations:", nrow(datos), "\n")
  
  if(nrow(datos) < 30){
    
    warning("Not enough locations.")
    
    return(NULL)
    
  }
  
  fit <- NULL
  
  model_used <- NA
  
  #----------------------------------------------------------
  # 1. Move Persistence Model
  #----------------------------------------------------------
  
  fit <- tryCatch(
    
    fit_ssm(
      datos,
      model = "mp",
      vmax = 4,
      time.step = 24,
      control = ssm_control(verbose = 1)
    ),
    
    error = function(e) NULL
    
  )
  
  if(!is.null(fit) && all(fit$converged)){
    
    model_used <- "mp"
    
  }
  
  #----------------------------------------------------------
  # 2. Continuous Random Walk
  #----------------------------------------------------------
  
  if(is.null(fit) || !all(fit$converged)){
    
    fit <- tryCatch(
      
      fit_ssm(
        datos,
        model = "crw",
        vmax = 4,
        time.step = 24,
        control = ssm_control(verbose = 1)
      ),
      
      error=function(e) NULL
      
    )
    
    if(!is.null(fit) && all(fit$converged)){
      
      model_used <- "crw"
      
    }
    
  }
  
  #----------------------------------------------------------
  # 3. Random Walk
  #----------------------------------------------------------
  
  if(is.null(fit) || !all(fit$converged)){
    
    fit <- tryCatch(
      
      fit_ssm(
        datos,
        model = "rw",
        vmax = 4,
        time.step = 24,
        control = ssm_control(verbose = 1)
      ),
      
      error=function(e) NULL
      
    )
    
    if(!is.null(fit) && all(fit$converged)){
      
      model_used <- "rw"
      
    }
    
  }
  
  cat("Model:", model_used, "\n")
  
  list(
    
    id = individuo,
    
    year = anio,
    
    model = model_used,
    
    fit = fit
    
  )
  
}
exists("fit_year_ssm")
################################################
exists("track_sf")
track_overlap_clean <- readRDS(
  "data/processed/track_overlap_clean.rds"
)

track_sf <- track_overlap_clean |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )
class(track_sf)
#############################
track_overlap_clean <- readRDS(
  "data/processed/track_overlap_clean.rds"
)

track_sf <- track_overlap_clean |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

exists("track_sf")
##########MODELOS######################################
#------------------------------------------------------------
# Function to fit State-Space Models
#------------------------------------------------------------

fit_year_ssm <- function(data_sf, individuo, anio){
  
  cat("\n--------------------------------------\n")
  cat("Individual:", individuo,
      "| Year:", anio, "\n")
  
  datos <- data_sf |>
    dplyr::filter(
      id == individuo,
      year == anio
    )
  
  cat("Locations:", nrow(datos), "\n")
  
  if(nrow(datos) < 30){
    
    warning("Insufficient observations.")
    
    return(NULL)
    
  }
  
  modelos <- c("mp","crw","rw")
  
  for(m in modelos){
    
    cat("Trying model:", m, "\n")
    
    ajuste <- tryCatch(
      
      fit_ssm(
        datos,
        model = m,
        time.step = 24,
        vmax = 4,
        control = ssm_control(verbose = 0)
      ),
      
      error = function(e) NULL
      
    )
    
    if(!is.null(ajuste)){
      
      if(all(ajuste$converged)){
        
        cat("Converged with:", m, "\n")
        
        return(
          
          list(
            
            id = individuo,
            
            year = anio,
            
            model = m,
            
            fit = ajuste
            
          )
          
        )
        
      }
      
    }
    
  }
  
  warning("No model converged.")
  
  return(NULL)
  
}
exists("fit_year_ssm")
############################
test_ssm <- fit_year_ssm(
  data_sf = track_sf,
  individuo = "183516",
  anio = 2020
)
str(test_ssm, max.level = 1)
names(test_ssm$fit)
str(test_ssm$fit, max.level = 2)
#######################
#------------------------------------------------------------
# 6. Fit SSM for all individual-year combinations
#------------------------------------------------------------

results_ssm <- purrr::pmap(
  
  list(
    combos$id,
    combos$year
  ),
  
  function(id, year){
    
    fit_year_ssm(
      data_sf = track_sf,
      individuo = id,
      anio = year
    )
    
  }
  
)
#------------------------------------------------------------
# 7. Convergence summary
#------------------------------------------------------------

summary_ssm <- purrr::map_dfr(
  
  results_ssm,
  
  function(x){
    
    if(is.null(x)){
      
      return(NULL)
      
    }
    
    tibble(
      
      id = x$id,
      
      year = x$year,
      
      model = x$model,
      
      converged = x$fit$converged,
      
      pdHess = x$fit$pdHess
      
    )
    
  }
  
)

exists("results_ssm")
#------------------------------------------------------------
# Summary of fitted SSM models
#------------------------------------------------------------

summary_ssm <- purrr::map_dfr(
  
  results_ssm,
  
  function(x){
    
    if(is.null(x)) return(NULL)
    
    tibble(
      id = x$id,
      year = x$year,
      model = x$model,
      converged = unname(x$fit$converged),
      pdHess = unname(x$fit$pdHess)
    )
    
  }
  
)

summary_ssm

saveRDS(
  summary_ssm,
  "results/summary_ssm.rds"
)

write.csv(
  summary_ssm,
  "results/convergence_summary.csv",
  row.names = FALSE
)
########################
list.files("results", full.names = TRUE)
saveRDS(
  results_ssm,
  "results/ssm_models.rds"
)
list.files("results")
