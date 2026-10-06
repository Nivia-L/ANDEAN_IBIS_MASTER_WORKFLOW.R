# Andean Ibis Home Range 2026

Reproducible R workflow for home-range, movement, habitat-use, NDVI, and movement-persistence analyses of the Andean Ibis (*Theristicus branickii*) using Argos and GPS telemetry data.

## Project overview

This repository contains the reproducible analysis workflow for a telemetry study of Andean Ibis (*Theristicus branickii*) in the Ecuadorian Andes.

The workflow integrates telemetry data, state-space modeling, home-range estimation, environmental variables, habitat composition, NDVI, and movement metrics.

Two individuals were analyzed:

- **ID 183516** — Argos telemetry
- **ID 6700** — GPS-GSM telemetry

All spatial analyses use **WGS84 / UTM zone 17S (EPSG:32717)** for metric calculations.

## Reproducible workflow

The complete analysis is organized into sequential R scripts:

| Script | Analysis |
|---|---|
| `00_setup.R` | Packages, directories, and project setup |
| `01_import_cleaning.R` | Import and cleaning of telemetry data |
| `02_state_space_models.R` | Preparation for state-space modeling |
| `03_state_space_models.R` | State-space model fitting |
| `04_extract_predicted_tracks.R` | Extraction of predicted trajectories |
| `05_home_range_MCP.R` | Minimum Convex Polygon home ranges |
| `06_home_range_KDE.R` | Kernel density home ranges |
| `07_home_range_landcover.R` | Land-cover composition within home ranges |
| `08_home_range_ndvi.R` | NDVI extraction and variability |
| `09_movement_persistence_GAM.R` | Movement persistence and GAM analysis |
| `10_movement_metrics.R` | Movement and displacement metrics |
| `11. Create_shapelifes_CMP_KDE.R` | MCP/KDE shapefile generation |

## Master workflow

The complete pipeline can be launched from:

`ANDEAN_IBIS_MASTER_WORKFLOW.R`

The master workflow executes the analysis scripts in the correct order and performs basic validation of the principal outputs.

The workflow is designed to run from the project root and does not depend on a hidden `.RData` workspace.

## Main analyses

The workflow includes:

- Telemetry data cleaning and harmonization
- State-space modeling
- Predicted movement trajectories
- Minimum Convex Polygon (MCP) home ranges
- Kernel Density Estimate (KDE) home ranges
- Land-cover composition
- NDVI statistics
- Net Squared Displacement (NSD)
- Movement distance and speed metrics
- Movement persistence modeling using generalized additive models (GAMs)

## Software

Analyses were developed using:

- R 4.5.2
- RStudio 2025.5.1.513
- EPSG:32717 — WGS84 / UTM zone 17S

Main R packages include:

- `tidyverse`
- `sf`
- `terra`
- `lubridate`
- `aniMotum`
- `adehabitatHR`
- `mgcv`
- `readxl`
- `writexl`

## Reproducibility

The workflow is intended to be reproducible from a clean R session.

The analysis does **not** require loading:

- `.RData`
- `.Rhistory`
- a previously saved interactive workspace

Intermediate and final outputs are written to the `results/` directory.

## Repository structure

```text
Andean_Ibis_HomeRange_2026/
│
├── README.md
├── ANDEAN_IBIS_MASTER_WORKFLOW.R
│
├── scripts/
│   ├── 00_setup.R
│   ├── 01_import_cleaning.R
│   ├── 02_state_space_models.R
│   ├── 03_state_space_models.R
│   ├── 04_extract_predicted_tracks.R
│   ├── 05_home_range_MCP.R
│   ├── 06_home_range_KDE.R
│   ├── 07_home_range_landcover.R
│   ├── 08_home_range_ndvi.R
│   ├── 09_movement_persistence_GAM.R
│   ├── 10_movement_metrics.R
│   └── 11. Create_shapelifes_CMP_KDE.R
│
├── data/
└── results/
