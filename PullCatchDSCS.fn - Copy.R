# Edit PullCatch.fn.R to accommodate a custom list of SciNames for corals, sponges, and sea pens
# WARNING!! PullCatch.fn has been deprecated; new function is 'pull_catch'
# Ask Jim Fellows about creating functional groups matching those in national DSCS database
# Draws data from the FRAM Data Warehouse tables: 'catch_fact' and 'operation_haul_fact'

# For testing purposes
library(nwfscSurvey)
outDir = file.path(paste0(getwd(), "/shp"))
dat = nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Combo", SciName = "Gorgonacea", SaveFile = TRUE, Dir = outDir)
dat = PullCatchDSCS.fn(SurveyName = "NWFSC.Combo", SciName = "Gorgonacea", SaveFile = TRUE, Dir = outDir)
dat = PullCatchDSCS.fn(SurveyName = "NWFSC.Combo", Name = "coral", SaveFile = TRUE, Dir = outDir)
dat = PullCatchDSCS.fn(SurveyName = "NWFSC.Combo", Name = "sponge", SaveFile = TRUE, Dir = outDir)
dat = PullCatchDSCS.fn(SurveyName = "NWFSC.Combo", Name = "sea pen", SaveFile = TRUE, Dir = outDir)
dat = PullCatchDSCS.fn(SurveyName = "NWFSC.Combo", Name = "DSCS", SaveFile = TRUE, Dir = outDir)

#' Pull catch data from the NWFSC data warehouse
#' The website is: https://www.webapp.nwfsc.noaa.gov/data
#' This function can be used to pull a single species or all observed species
#' In order to pull all species leave Name = NULL and SciName = NULL
#'
#' @param Name  common name of species data to pull from the data warehouse; options are (coral, sponge, sea pen, DSCS)
#' @param SciName scientific name of species data to pull from the data warehouse
#' @param YearRange range of years to pull data
#' @param SurveyName survey to pull the data for the options are: 
#' Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia, 
#' NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both are not working), NWFSC.Video
#' @param SaveFile option to save the file to the directory
#' @param Dir directory where the file should be saved
#' @param verbose opt to print out message statements
#'
#' @author Curt Whitmire, based on code from Chantel Wetzel and John Wallace
#' @export
#'
#' @import jsonlite
#' @import chron
#' @importFrom dplyr left_join rename
#'
#' @examples
#'\dontrun{
#' # SurveyName is only arg that has to be specified
#' dat = PullCatch.fn(SurveyName = "NWFSC.Combo")
#'}

PullCatchDSCS.fn <- function (Name = NULL, SciName = NULL, YearRange = c(2003, 5000), SurveyName = NULL, SaveFile = FALSE, Dir = NULL, verbose = TRUE)
{
  
  if (SurveyName %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")){
    stop ("The catch pull currently does not work for hook & line data. Pull directly from the warehouse https://www.webapp.nwfsc.noaa.gov/data")}
  
  if(SaveFile){
    if(is.null(Dir)){
      stop("The Dir input needs to be specified in order to save output file.")
    }
    if (!file.exists(Dir)) stop("The Dir argument leads to a location",
                                ",\ni.e., ", Dir, ", that doesn't exist.")
  }
  
  if (Name %in% c("DSCS", "coral", "sponge", "sea pen")) { var.name = "scientific_name"; Species = Name; new.name = "Common_name"; outName = Name} #CEW: need to add database sub_categories for more informative common_name
  if (is.null(Name)) { var.name = "scientific_name"; Species = SciName; new.name = "ScientificName"; outName = Name}
  # if (is.null(SciName)) { var.name = "common_name"; Species = Name; new.name = "Common_name"; outName = SciName} #CEW: need to fix this logic now that added DSCS logic
  if (is.null(SciName) & is.null(Name)) { var.name = "common_name"; Species = "pull all"; new.name = "Common_name" }#stop("Need to specify Name or SciName to pull data!")}
  
  # Survey options available in the data warehouse
  surveys = createMatrix()
  
  # Check the input survey name against available options
  if (!SurveyName %in% surveys[,1]) {
    stop("The SurveyName argument does not match one of the available options:\n",
         paste(surveys[,1], collapse = "\n")) }
  
  # Find the long project name to extract data from the warehouse
  for(i in 1:dim(surveys)[1]){
    if(SurveyName == surveys[i,1]){
      project = surveys[i,2]
      projectShort = surveys[i,1]
    }
  }
  
  if (length(YearRange) == 1) {
    YearRange <- c(YearRange, YearRange)    }
  
  
  # Pull data for the specific species for the following variables
  Vars <- c(var.name, "taxon_rank", "year", "subsample_count", "subsample_wt_kg", "project", "cpue_kg_per_ha_der",
            "total_catch_numbers", "total_catch_wt_kg", "vessel", "tow", "operation_dim$legacy_performance_code", 
            "statistical_partition_dim$statistical_partition_type"
            # , "best_available_taxonomy_whid", "best_available_taxonomy_observation_detail_whid",
            # "field_identified_taxonomy_dim$worms_aphiaid",
            # "best_available_taxonomy_dim$worms_aphiaid"
            )
  
  Vars.short <- c(var.name, "taxon_rank", "year", "subsample_count", "subsample_wt_kg", "project", "cpue_kg_per_ha_der",
                  "total_catch_numbers", "total_catch_wt_kg", "vessel", "tow")
  
  
  UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                    "station_invalid=0,",
                    "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,",
                    "field_identified_taxonomy_dim$", var.name,"=", paste(strsplit(Species, " ")[[1]], collapse = "%20"),
                    ",date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                    "&variables=", paste0(Vars, collapse = ","))

  
  # if (Species == "pull all"){
  #   UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
  #                     "station_invalid=0,",
  #                     "performance=Satisfactory,", "depth_ftm>=30,depth_ftm<=700,",
  #                     "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
  #                     "&variables=", paste0(Vars, collapse = ","))
  # }
  
  #CEW: start edits: added to return only DSCS taxa
  if (Species == "coral"){
    UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                      # "station_invalid=0,", #CEW: not necessary for DSCS catch
                      "performance=Satisfactory,", 
                      # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                      "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                      ",species_subcategory=coral", #CEW added to return only coral taxa
                      "&variables=", paste0(Vars, collapse = ","))
    
    DataPull <- try(jsonlite::fromJSON(UrlText))
    
    if (verbose){
      message(UrlText)}
    
    if(!is.data.frame(DataPull)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
  }
  
  if (Species == "sponge"){
    UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                      # "station_invalid=0,", #CEW: not necessary for DSCS catch
                      "performance=Satisfactory,", 
                      # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                      "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                      ",best_available_taxonomy_dim$phylum_20=Porifera", #CEW added to return only sponge taxa
                      "&variables=", paste0(Vars, collapse = ","))
    
    DataPull <- try(jsonlite::fromJSON(UrlText))
    
    if (verbose){
      message(UrlText)}
    
    if(!is.data.frame(DataPull)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
  }
  
  if (Species == "sea pen"){
    UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                      # "station_invalid=0,", #CEW: not necessary for DSCS catch
                      "performance=Satisfactory,", 
                      # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                      "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                      ",best_available_taxonomy_dim$order_40=Pennatulacea", #CEW added to return only sea pen taxa
                      "&variables=", paste0(Vars, collapse = ","))
    
    DataPull <- try(jsonlite::fromJSON(UrlText))
    
    if (verbose){
      message(UrlText)}
    
    if(!is.data.frame(DataPull)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
  }
  
  if (Species == "DSCS"){
    UrlText1 <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                       # "station_invalid=0,", #CEW: not necessary for DSCS catch
                      "performance=Satisfactory,", 
                      # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                      "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                      ",species_subcategory=coral", #CEW added to return only coral taxa
                      "&variables=", paste0(Vars, collapse = ","))
    UrlText2 <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                       # "station_invalid=0,", #CEW: not necessary for DSCS catch
                       "performance=Satisfactory,", 
                       # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                       "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                       ",best_available_taxonomy_dim$phylum_20=Porifera", #CEW added to return only sponge taxa
                       "&variables=", paste0(Vars, collapse = ","))
    UrlText3 <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                       # "station_invalid=0,", #CEW: not necessary for DSCS catch
                       "performance=Satisfactory,", 
                       # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                       "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                       ",best_available_taxonomy_dim$order_40=Pennatulacea", #CEW added to return only sea pen taxa
                       "&variables=", paste0(Vars, collapse = ","))
    
    DataPull1 <- try(jsonlite::fromJSON(UrlText1))
    if(!is.data.frame(DataPull1)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
    
    DataPull2 <- try(jsonlite::fromJSON(UrlText2))
    if(!is.data.frame(DataPull2)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
    
    DataPull3 <- try(jsonlite::fromJSON(UrlText3))
    if(!is.data.frame(DataPull3)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
    
    # CEW: add logic to append results of multiple pull requests
    DataPull <- rbind(DataPull1, DataPull2, DataPull3)
    
    if (verbose){
      message(UrlText1)
      message(UrlText2)
      message(UrlText2)
      }
    
    if(!is.data.frame(DataPull)) {
      stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
    }
  }

  #CEW: end edits
  
  if (verbose){
    message("Pulling catch data. This can take up to ~ 30 seconds (or more).")}
  # Pull data from the warehouse
  # DataPull <- try(jsonlite::fromJSON(UrlText))
  # if(!is.data.frame(DataPull)) {
  #   stop(cat("\nNo data returned by the warehouse for the filters given.\n Make sure the year range is correct for the project selected and the input name is correct,\n otherwise there may be no data for this species from this project.\n"))
  # }
  
  # Remove water hauls
  fix =  is.na(DataPull[,"operation_dim$legacy_performance_code"])
  if(sum(fix) > 0) { DataPull[fix,"operation_dim$legacy_performance_code"] = -999 }
  # Whether values are NA or "NA" varies based on the presence of "Life Stage" samples
  if(sum(is.na(DataPull[,"statistical_partition_dim$statistical_partition_type"])) != dim(DataPull)[1]){
    keep = DataPull[,"statistical_partition_dim$statistical_partition_type"] == "NA"
    DataPull = DataPull[keep, ]
  }
  
  keep = DataPull[,"operation_dim$legacy_performance_code"] != 8
  DataPull = DataPull[keep,]
  DataPull = DataPull[,Vars.short]
  
  Data = dplyr::rename(DataPull, TaxonRank = taxon_rank,
                       ObservationYear = year, Subsample_count = subsample_count,
                       Subsample_wt_kg = subsample_wt_kg, SurveyID = project,
                       CPUE_kg_per_ha = cpue_kg_per_ha_der, Subsample_count = subsample_count,
                       Subsample_wt_kg = subsample_wt_kg, Vessel = vessel, Tow = tow)
  
  names(Data)[which(names(Data)=="scientific_name")] = "ScientificName"
  names(Data)[which(names(Data)=="common_name")] = "Common_name"
  
  # Pull all tow data (includes tows where the species was not observed)
  Vars <- c("project", "year", "vessel", "tow", "datetime_utc_iso", "gear_depth_m_der", "longitude_dd", "latitude_dd", "area_swept_ha_der", "trawl_id", "operation_dim$legacy_performance_code", 
            "actual_station_design_dim$station_code", "tow_end_timestamp", "temperature_at_gear_c_der", "salinity_at_gear_psu_der", "o2_at_gear_ml_per_l_der") #CEW: added for DSCRTP
  Vars.short <- c("project", "year", "vessel", "tow", "datetime_utc_iso", "tow_end_timestamp",  "gear_depth_m_der", "longitude_dd", "latitude_dd", "area_swept_ha_der", "trawl_id", 
                  "actual_station_design_dim$station_code", "temperature_at_gear_c_der", "salinity_at_gear_psu_der", "o2_at_gear_ml_per_l_der") #CEW: added for DSCRTP
  
  UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=", paste(strsplit(project, " ")[[1]], collapse = "%20"),",",
                    # "station_invalid=0,", #CEW: not necessary for DSCS catch
                    "performance=Satisfactory,",
                    # "depth_ftm>=30,depth_ftm<=700,", #CEW: not necessary for DSCS catch
                    "date_dim$year>=", YearRange[1], ",date_dim$year<=", YearRange[2],
                    "&variables=", paste0(Vars, collapse = ","))
  
  if (verbose){
    message(UrlText)}
  
  All.Tows <- jsonlite::fromJSON(UrlText)
  
  # Remove water hauls
  fix =  is.na(All.Tows[,"operation_dim$legacy_performance_code"])
  if(sum(fix) > 0) { All.Tows[fix,"operation_dim$legacy_performance_code"] = -999 }
  keep = All.Tows[,"operation_dim$legacy_performance_code"] != 8
  All.Tows = All.Tows[keep,]
  All.Tows = All.Tows[,Vars.short]
  
  All.Tows = dplyr::rename(All.Tows, SurveyID = project, EventID = trawl_id, ObservationYear = year, ObservationDT = tow_end_timestamp,
                           Vessel = vessel, Tow = tow, ObservationDate = datetime_utc_iso, Station = "actual_station_design_dim$station_code",
                           DepthInMeters = gear_depth_m_der, LongitudeInDD = longitude_dd, LatitudeInDD = latitude_dd, Area_Swept_ha = area_swept_ha_der, 
                           Temperature = temperature_at_gear_c_der, Salinity = salinity_at_gear_psu_der, Oxygen = o2_at_gear_ml_per_l_der) #CEW: added for DSCRTP
  
  All.Tows <- All.Tows[!duplicated(paste(All.Tows$ObservationYear, All.Tows$Pass, All.Tows$Vessel, All.Tows$Tow)),
                       c("SurveyID", "EventID", "Station", "ObservationYear", "ObservationDT", "Vessel", "Tow", "DepthInMeters", "LongitudeInDD", "LatitudeInDD", "Area_Swept_ha", 
                         "Temperature", "Salinity", "Oxygen")] #CEW: added for DSCRTP
  
  # Link each data set together based on trawl_id
  if("Common_name" %in% names(Data)) {
    grid = expand.grid("EventID" = unique(All.Tows$EventID), "Common_name"=unique(Data$Common_name),
                       stringsAsFactors = FALSE)
  } else {
    grid = expand.grid("EventID" = unique(All.Tows$EventID), "ScientificName"=unique(Data$ScientificName),
                       stringsAsFactors = FALSE)
  }
  Out = dplyr::left_join(grid, All.Tows)
  # Out = dplyr::left_join(Out, Data)
  Out = dplyr::left_join(Data, Out) #CEW: changed to only include tows with positive catch
  #Out = dplyr::left_join(All.Tows, Data)
  
  # Fill in zeros where needed
  Out$total_catch_wt_kg[is.na(Out$total_catch_wt_kg)] <- 0
  
  Out$CPUE_kg_per_ha[is.na(Out$CPUE_kg_per_ha)] <- 0
  
  Out$Subsample_count[is.na(Out$Subsample_count)] <- 0
  
  Out$Subsample_wt_kg[is.na(Out$Subsample_wt_kg)] <- 0
  
  Out$total_catch_numbers[is.na(Out$total_catch_numbers)] <- 0
  
  # Need to check what this is doing
  noArea = which(is.na(Out$Area_Swept_ha))
  if (length(noArea) > 0) {
    if (verbose){
      print(cat("\nThere are", length(noArea), "records with no area swept calculation. These record will be filled with the mean swept area across all tows.\n"))
      print(Out[noArea,c("EventID", "ObservationYear", "Area_Swept_ha", "CPUE_kg_per_ha", "total_catch_numbers")]) }
    Out[noArea, "Area_Swept_ha"] <- mean(Out$Area_Swept_ha, trim = 0.05, na.rm = TRUE)
  }
  
  # Scientific Name is missing after the matching when Total_sp_wt_kg is zero
  if (!is.null(Name)) { Out$Common_name <- Species }
  if (!is.null(SciName)) { Out$Scientific_name <- Species }
  
  # Out$ObservationDate <- chron::chron(format(as.POSIXlt(Out$ObservationDate, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
  Out$ObservationDate <- format(as.POSIXlt(Out$ObservationDT, format = "%Y-%m-%dT%H:%M:%SZ"), "%Y-%m-%d") #CEW: cleaner way to export date format
  Out$ObservationTime <- format(as.POSIXlt(Out$ObservationDT, format = "%Y-%m-%dT%H:%M:%SZ"), "%H:%M:%S") #CEW: cleaner way to export time format
  
  Out$Project <- projectShort #CEW: changes from long name of project to short name (e.g., "NWFSC.Combo")
  
  Out$EventID = as.character(Out$EventID)
  
  #CEW: add metadata fields
  Out$DataProvider <- "NOAA, Northwest Fisheries Science Center"
  Out$DataContact <- "Whitmire, Curt: curt.whitmire@noaa.gov"
  Out$Citation <- "West Coast Groundfish Bottom Trawl Survey, NOAA Fisheries, NWFSC/FRAM, 2725 Montlake Blvd. East, Seattle, WA 98112"
  Out$Repository <- "FRAM Data Warehouse"
  Out$VehicleName <- "NA"
  Out$PI <- "NA"
  Out$PIAffiliation <- "NA"
  Out$SamplingEquipment <- "trawl"
  Out$DepthMethod <- "reported"
  Out$NavType <- "ship GPS; other"
  Out$LocationAccuracy <- ">1000m"
  Out$Ocean <- "North Pacific"
  Out$LargeMarineEcosystem <- "California Current"
  Out$Country <- "USA"
  Out$FishCouncilRegion <- "Pacific"
  Out$RecordType <- "catch record"
  Out$Reporter <- "Whitmire, Curt"
  Out$ReporterEmail <- "curt.whitmire@noaa.gov"
  # Out$Locality <- "NA"
  loc <- function(x) {
    case_when(
      x <= 34.5 ~ "Southern CA Bight",
      TRUE    ~ "NA"
    )
  }
  Out$Locality <- loc(Out$LatitudeInDD)
  Out$ImageFilePath <- "NA"
  Out$Density <- "NA"
  Out$Condition <- "NA"
  Out$OccurrenceRemarks <- "wet weight"
  Out$Habitat <- "NA"
  OutSubstrate <- "NA"
  
  # Convert the CPUE into km2
  Out$cpue_kg_km2 = Out$CPUE_kg_per_ha * 100
  remove = "CPUE_kg_per_ha"
  Out = Out[,!(names(Out) %in% remove)]
  
  if(SaveFile){
    time = Sys.time()
    time = substring(time, 1, 10)
    #save(Out, file = paste0(Dir, "/Catch_", outName, "_", SurveyName, "_",  time, ".rda"))
    save(Out, file = file.path(Dir, paste("Catch_", outName, "_", SurveyName, "_",  time, ".rda", sep = "")))
    #convert to sf object and save shapefile
    require(sf)
    Out_sf <- sf::st_as_sf(Out, coords = c("LongitudeInDD","LatitudeInDD"), remove = FALSE, crs = 4326)
    sf::st_write(Out_sf, file.path(Dir, paste("Catch_", outName, "_", SurveyName, "_",  time, ".shp", sep = "")))
    
    if (verbose){
      message(paste("Catch data file saved to following location:", Dir))}
  }
  
  return(Out)
}
