# This script will read in a set of good fire data from your google drive and turn it into a single CSV

########
# NOTE: The google drive access functions will not work on R4.5.x yet! Use R4.4.x
########

rm(list=ls()) #Ensure empty workspace if running from beginning

if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("utils", "functions.R"))

install_and_load_packages(c("googledrive",
                                  "dplyr",
                                  "readr",
                                  "purrr",
                                  "sf",
                                  "here"))

#################################################

# INPUTS ----

summarizeName <- 'states_welty_wildfire_fri_splitfrg5'  # CHANGE THIS TO MATCH summarizeName IN GEE SCRIPT 2_full-streamlined-good-fire
eventsName <- 'welty_wildfire_fri_splitfrg5' # This should be the same as summarizeName, but without the summary appendix (i.e. the version name)
driveFolder <- 'GEE_Exports'     # MAKE SURE THAT THIS IS THE GDRIVE FOLDER YOU ARE USING. 'GEE_Exports' is the default for the export script
earliestYear <- 2010
latestYear <- 2020
rawGFEventsDatabase <- 'welty_wildfire_west_1984_2020.gpkg'
event_id_column <- "OBJECTID"

##### Run to authenticate gdrive access
drive_auth()
####

##################################################


# FUNCTIONS ----


# A function to read a good fire data file
read_gf_csv_from_gdrive <- function(year, summarizeName) {
  nm <- paste0("gf_data_", as.character(year), "_", summarizeName, ".csv")
  csv <- read_csv_from_gdrive_v2(drive_folder = driveFolder, file_name = nm)
  return(csv)
}



# OPERATE ----

# Manage GF summaries ----
years <- seq(earliestYear,latestYear)

# Map file over all years in the set and write as local csv

fullGFDataSet <- purrr::map(.x = years, .f = read_gf_csv_from_gdrive, summarizeName) |>
  dplyr::bind_rows() |>
  dplyr::select(-system.index, -.geo)


readr::write_csv(fullGFDataSet, here::here('data', 'derived', paste0("gf_data_combined_", summarizeName, "_", earliestYear, "_", latestYear, ".csv")))

# FOR MOORE OPERATIONALIZED VERSION; SEE OPERATIONALIZED REPO
# ## Clean dataset and re-write as clean dataset
# 
# 
# if(grepl('spark', summarizeName)) {
#   if(!summarizeName == 'sparkEcoregions') {
#     cleanGFDataSet <- fullGFDataSet |>
#       dplyr::filter(SPARK != 'AK - Bristol Bay') |>
#       dplyr::filter(SPARK != 'BC - Bulkley Morice')
#   }
# }
# 
# cleanGFDataSet <- cleanGFDataSet |>
#   dplyr::mutate(cbiAnyBurned = cbiAnyBurned * 0.000247105,
#                 cbiHigh = cbiHigh * 0.000247105,
#                 cbiLower = cbiLower * 0.000247105,
#                 cbiUnburned = cbiUnburned * 0.000247105,
#                 highGoodFire = highGoodFire * 0.000247105,
#                 lowerGoodFire = lowerGoodFire * 0.000247105,
#                 lowerRegimeCbiHigh = lowerRegimeCbiHigh * 0.000247105,
#                 lowerRegimeCbiUnburned = lowerRegimeCbiUnburned * 0.000247105,
#                 replaceRegimeCbiLow = replaceRegimeCbiLow * 0.000247105,
#                 replaceRegimeCbiUnburned = replaceRegimeCbiUnburned * 0.000247105,
#                 totalArea = totalArea * 0.000247105,
#                 yearPriorForest = yearPriorForest * 0.000247105) |>
#   dplyr::rename(TotalBurnedForestAcres = cbiAnyBurned,
#                 TotalHighSeverityBurnedForestAcres = cbiHigh,
#                 TotalLowModSeverityBurnedForestAcres = cbiLower,
#                 TotalUnburnedForestWithinFirePerimetersAcres = cbiUnburned,
#                 HighSeverityGoodFireAcres = highGoodFire,
#                 LowerSeverityGoodFireAcres = lowerGoodFire,
#                 LowerRegimeHighSeverityBurnAcres = lowerRegimeCbiHigh,
#                 LowerRegimeUnburnedAcres = lowerRegimeCbiUnburned,
#                 ReplaceRegimeLowSeverityBurnAcres = replaceRegimeCbiLow,
#                 ReplaceRegimeUnburnedAcres = replaceRegimeCbiUnburned,
#                 TotalPolygonAreaAcres = totalArea,
#                 TotalForestedAreaInYearPriorAcres = yearPriorForest) |>
#   dplyr::select(-units) |>
#   dplyr::relocate(where(is.numeric), .after = where(is.character)) |>
#   dplyr::relocate(TotalForestedAreaInYearPriorAcres, .before = TotalBurnedForestAcres) |>
#   dplyr::relocate(TotalPolygonAreaAcres, .before = TotalForestedAreaInYearPriorAcres) |>
#   dplyr::relocate(year, .before = TotalPolygonAreaAcres) |>
#   dplyr::mutate(TotalGoodFireAcres = HighSeverityGoodFireAcres + LowerSeverityGoodFireAcres)
# 
# readr::write_csv(cleanGFDataSet, here::here('data', 'derived', paste0("clean_gf_data_combined_", summarizeName, ".csv")))



# Manage GF events ----



#GF event data from GDrive
gfEventDataFlnm <- paste0('gf_data_fire_events_', earliestYear, '_', latestYear, '_', eventsName, '.csv')
gfEventDataFl <- here::here('data', 'derived', gfEventDataFlnm)
if(file.exists(gfEventDataFl)) {
  gfEventData <- readr::read_csv(gfEventDataFl)
} else {
  gfEventData <- read_csv_from_gdrive_v2(drive_folder = driveFolder, file_name = gfEventDataFlnm) #from GEE
  readr::write_csv(gfEventData, gfEventDataFl)
}

# Raw GF events originally created
goodfireEventDatabase <- sf::st_read(here::here("data", "derived", rawGFEventsDatabase)) |>
  sf::st_transform(sf::st_crs("EPSG:5070")) |>
  dplyr::filter(Fire_Year >= earliestYear & Fire_Year <= latestYear)
  

# Join the data and export
allGFDats <- goodfireEventDatabase |>
  dplyr::left_join(
    gfEventData |>
      dplyr::select(
        dplyr::any_of(setdiff(names(gfEventData), setdiff(names(goodfireEventDatabase), event_id_column)))
      ),
    by = event_id_column
  ) |>
  filter(!is.na(`system.index`))

sf::st_write(allGFDats, here::here("data", "derived", paste0("merged_goodfire_final_", summarizeName,".gpkg")), append = FALSE)

