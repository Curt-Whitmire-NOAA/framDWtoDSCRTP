### Load required packages
library(rstudioapi) # Safely Access the RStudio API
library(nwfscSurvey) # Access NWFSC survey data via FRAM Data Warehouse API
library(sf) # Simple Features for R
library(dplyr) # A Grammar of Data Manipulation
library(stringr)

### Set working directories
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
dir <- getwd()
print(getwd())

outPath <- "C:/Users/curt.whitmire/Documents/Github/_data/GIS" # Data output location

### Pull haul data from Data Warehouse API and update attributes
SrvyName <- "NWFSC.Combo"
haul_data <- nwfscSurvey::pull_haul(survey = SrvyName)

if (SrvyName == "NWFSC.Combo") {
  drop_cols <- c("fluorescence_at_surface_mg_per_m3_der",
               "operation_dim$legacy_performance_code")
  haul_data <- haul_data %>% 
    arrange(trawl_id) %>% 
    select(-any_of(drop_cols)) %>% 
    select(trawl_id,
           performance,
           # station_invalid, # all zeroes based on default pull_haul function
           project,
           year,
           pass,
           leg,
           # date_formatted, # reads as numeric in gdb format
           datetime_utc_iso,
           sampling_start_hhmmss,
           sampling_end_hhmmss,
           vessel,
           longitude_dd,
           latitude_dd,
           vessel_start_longitude_dd,
           vessel_start_latitude_dd,
           vessel_end_longitude_dd,
           vessel_end_latitude_dd,
           gear_start_longitude_dd,
           gear_start_latitude_dd,
           gear_end_longitude_dd,
           gear_end_latitude_dd,
           depth_hi_prec_m,
           area_swept_ha_der,
           net_height_m_der,
           net_width_m_der,
           door_width_m_der,
           temperature_at_surface_c_der,
           temperature_at_gear_c_der,
           salinity_at_gear_psu_der,
           o2_at_gear_ml_per_l_der,
           turbidity_ntu_der,
           vertebrate_weight_kg,
           invertebrate_weight_kg,
           nonspecific_organics_weight_kg
    )
} else {
  drop_cols <- NULL
  haul_data <- haul_data %>% 
    arrange(trawl_id) %>% 
    select(-any_of(drop_cols))
}

### Get haul data attributes for future use
YrSrt <- min(haul_data$year)
YrEnd <- max(haul_data$year)

### Convert to sf object
hauls = sf::st_as_sf(haul_data, coords = c("longitude_dd", "latitude_dd"), remove = FALSE, crs = 4326)

### Write to ESRI file geodatabase
sf::st_write(hauls, dsn = paste0(outPath, "/FRAMDW.gdb"), layer = paste0(str_replace_all(SrvyName, "\\.", "_"), "_hauls_bestpos_pt"), driver = "OpenFileGDB", delete_layer = TRUE)

