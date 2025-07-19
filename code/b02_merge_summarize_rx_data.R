
if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("utils", "functions.R"))

install_and_load_packages(c("googledrive",
                                  "purrr",
                                  "here",
                            "tigris",
                                  "tidyverse",
                                  "sf"))


##################################################

# INPUTS ----

driveFolder <- 'GEE_Exports'
derivedDatDir <- here::here("data", "derived")
epsg <- 5070
all_yrs_in_dataset <- seq(1986, 2023)
years_of_interest <- seq(2010, 2020)

##################################################

# FUNCTIONS ----

# A function to create the prescribed burning summary
# PARAMETERS
# polys :: the set of polygons to summarize by
# grpAttribute :: the name of the polygon attribute to use for summarizing, etc (should be a name of a column in the polygon set, as a character) e.g. "NAME"
create.rx.summary <- function(polys, grpAttribute) {
  allYrs <- all_yrs_in_dataset
  uniquePolys <- unique(polys[[grpAttribute]])
  combos <- expand.grid(year = allYrs, grpAttribute = uniquePolys) |>
    setNames(c("year", as.character(substitute(grpAttribute))))
  
  rxSummary <- rxInterest |>
    sf::st_join(polys, join = sf::st_within) |>
    dplyr::group_by(!!rlang::sym(grpAttribute), treatment_year, FRGDescription) |>
    dplyr::summarise(rxBurnHa = sum(TOTALACCOMPLISHMENT_HA),
                     nEvents = n()) |>
    dplyr::rename(year = treatment_year) |>
    dplyr::filter(!is.na({{grpAttribute}})) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(rxBurnArea = rxBurnHa * 10000) |>
    mutate(units = "m^2") #|>
  #select(-rxBurnHa)
  
  rxSummary <- combos %>%
    dplyr::left_join(rxSummary, by = c('year', grpAttribute)) %>%
    replace(is.na(.), 0) |>
    dplyr::mutate(FRGDescription = ifelse(FRGDescription == 0, NA, FRGDescription))
  
  return(rxSummary)
}

# OPERATE ----

## Read in and merge rx datasets ----

# Read in RX data from GDrive & write local if not already acquired
rxNm <- 'gee_twig_rx_lcms_lcmap.csv'
localRxPath <- here::here(derivedDatDir, rxNm)
if(!file.exists(localRxPath)) {
  rx_dats <- read_csv_from_gdrive_v2(drive_folder = driveFolder, file_name = rxNm)
  write_csv(csv, localRxPath)
} else {
  rx_dats <- readr::read_csv(localRxPath)
}


# Clean new data
rx_dats <- rx_dats |>
  dplyr::select(-`.geo`, -`system.index`)

# Load processed twig gpkg
twig_intentional_filtered <- sf::st_read(here::here(derivedDatDir, "twig_planned_ignition_no_dup_west.gpkg"))
twig_intentional_filtered_centroids <- twig_intentional_filtered |>
  sf::st_centroid(of_largest_polygon = TRUE)
twig_rx <- twig_intentional_filtered_centroids |>
  sf::st_transform(epsg)



# Create contextual data for understanding GEE RX outputs

#LCMS classes; for context
lcmsClasses <- cbind(
  seq(1,15),
  c("Trees",
    "Tall Shrubs & Trees Mix (SEAK Only)",
    "Shrubs & Trees Mix",
    "Grass/Forb/Herb & Trees Mix",
    "Barren & Trees Mix",
    "Tall Shrubs (SEAK Only)",
    "Shrubs",
    "Grass/Forb/Herb & Shrubs Mix",
    "Barren & Shrubs Mix",
    "Grass/Forb/Herb",
    "Barren & Grass/Forb/Herb Mix",
    "Barren or Impervious",
    "Snow or Ice",
    "Water",
    "Non-Processing Area Mask")
) |>
  as.data.frame() |> 
  `names<-`(c("LCMS_LandCover_Code", "LCMS_LandCoverDescription")) |>
  dplyr::mutate(LCMS_LandCover_Code = as.double(LCMS_LandCover_Code))

#LCMS classes; for context
lcmapClasses <- cbind(
  seq(1,8),
  c("Developed",
    "Cropland",
    "Grass/shrubs",
    "Tree cover",
    "Water",
    "Wetland",
    "Ice and snow",
    "Barren")
) |>
  as.data.frame() |> 
  `names<-`(c("LCMAP_LandCover_Code", "LCMAP_LandCoverDescription")) |>
  dplyr::mutate(LCMAP_LandCover_Code = as.double(LCMAP_LandCover_Code))

#FRG classes; for context
frgClasses <- cbind(
  seq(1,3),
  c("frcLowMix",
    "frcReplace",
    "frcOther")) |>
  as.data.frame() |>
  `names<-`(c("FRG", "FRGDescription"))  |>
  dplyr::mutate(FRG = as.double(FRG))


# Generate column names for each row
twig_rx <- twig_rx |>
  dplyr::mutate(
    LCMS_col = paste0("LandCover_LCMS_", treatment_year - 1),
    LCMAP_col = paste0("LandCover_LCMAP_", treatment_year - 1)
  )

# Join in the raster data (rx_dats)
rx_joined <- twig_rx |>
  dplyr::left_join(rx_dats, by = "unique_id")

# Safely extract the values using mapply
rx_joined$LCMS_PreBurnYear <- mapply(function(row, col) {
  if (col %in% names(rx_joined)) rx_joined[[col]][row] else NA_real_
}, seq_len(nrow(rx_joined)), rx_joined$LCMS_col)

rx_joined$LCMAP_PreBurnYear <- mapply(function(row, col) {
  if (col %in% names(rx_joined)) rx_joined[[col]][row] else NA_real_
}, seq_len(nrow(rx_joined)), rx_joined$LCMAP_col)

# Final join with class descriptions
rxWithGEE <- rx_joined |>
  dplyr::left_join(lcmsClasses, by = c("LCMS_PreBurnYear" = "LCMS_LandCover_Code")) |>
  dplyr::left_join(lcmapClasses, by = c("LCMAP_PreBurnYear" = "LCMAP_LandCover_Code")) |>
  dplyr::left_join(frgClasses, by = c("frcRcls" = "FRG")) |>
  dplyr::mutate(TOTALACCOMPLISHMENT_HA = acres * 0.404686)


#write out dataset
readr::write_csv(rxWithGEE, here::here(derivedDatDir, "twig_rx_with_gee_lcms_lcmap.csv"))




# Summarize RX numbers ----

#Filter
rxInterest <- rxWithGEE |>
  dplyr::filter(
    (LCMS_PreBurnYear == 1 & LCMAP_PreBurnYear == 4) |
      (LCMS_PreBurnYear == 1 & is.na(LCMAP_PreBurnYear)) |
      (is.na(LCMS_PreBurnYear) & LCMAP_PreBurnYear == 4)
  )


sf::st_write(rxInterest, here::here(derivedDatDir, "twig_gee_filtered_ready_for_analysis.gpkg"), append = FALSE)


# Prep summarizing polygons

# States
#Pull in state data as context
usa <- tigris::states() |>
  sf::st_transform(epsg)
west <- usa[usa$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),]  

# STATES
rxStateSummary <- create.rx.summary(polys = west, "NAME")
write_csv(rxStateSummary, here::here('data', 'derived', "twig_rx_state_summary_all.csv"))

rxStateSummaryInterest <- rxStateSummary |>
  filter(year %in% years_of_interest)
write_csv(rxStateSummaryInterest, here::here('data', 'derived', paste0("twig_rx_state_summary_", min(years_of_interest), "_", max(years_of_interest), ".csv")))


