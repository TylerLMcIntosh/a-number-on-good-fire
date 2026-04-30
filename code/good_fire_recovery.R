library(terra)
library(data.table)

# =========================================================
# USER SETTINGS
# =========================================================


# GEE Burned GEDI data download: https://code.earthengine.google.com/8846e386239ff59a3635d0c95143fb15
# GEE Unburned GEDI data download : https://code.earthengine.google.com/5eb6cb472f9e623df3f6da4db968e35c


# Folder with point CSV files
csv_dir <- "/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/burned_cor"

# Folder with yearly raster stacks
raster_dir <- "/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/rasters"

# Output folder
out_dir <- "/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Point column names
x_col         <- "GEDI_lon"   # or "x"
y_col         <- "GEDI_lat"    # or "y"
fire_id_col   <- "OBJECTID"
fire_year_col <- "Fire_Year"
value_col     <- "agbd"
severity <- "severity"

# Input point CRS
points_crs <- "EPSG:4326"

# Raster band names
band1_name <- "gwf_class"
band2_name <- "gwf_class_granular"

# Which raster band defines severity?
severity_band_index <- 1

# If TRUE, recode numeric severity values
use_severity_labels <- FALSE

severity_map <- c(
  "1" = "lowerGoodFire",
  "2" = "highGoodFire",
  "3" = "unmatchedRegime",
  "4" = "tooFrequent"
)

# =========================================================
# RASTER LOOKUP TABLE
# =========================================================
# Create a table that links each fire year to its raster file.
# Edit this to match your actual files.

raster_lookup <- data.table(
  fire_year = c(2010, 2011, 2012,2013, 2014, 2015,2016, 2017, 2018,2019, 2020),
  raster_file = c(
    file.path(raster_dir, "gwf_classified_welty_2010.tif"),
    file.path(raster_dir, "gwf_classified_welty_2011.tif"),
    file.path(raster_dir, "gwf_classified_welty_2012.tif"),
    file.path(raster_dir, "gwf_classified_welty_2013.tif"),
    file.path(raster_dir, "gwf_classified_welty_2014.tif"),
    file.path(raster_dir, "gwf_classified_welty_2015.tif"),
    file.path(raster_dir, "gwf_classified_welty_2016.tif"),
    file.path(raster_dir, "gwf_classified_welty_2017.tif"),
    file.path(raster_dir, "gwf_classified_welty_2018.tif"),
    file.path(raster_dir, "gwf_classified_welty_2019.tif"),
    file.path(raster_dir, "gwf_classified_welty_2020.tif")
  )
)

# Check raster files exist
missing_rasters <- raster_lookup[!file.exists(raster_file)]
if (nrow(missing_rasters) > 0) {
  stop("These raster files are missing:\n", paste(missing_rasters$raster_file, collapse = "\n"))
}

# =========================================================
# READ AND COMBINE ALL CSV FILES
# =========================================================

csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in: ", csv_dir)
}

points_dt <- rbindlist(lapply(csv_files, function(f) {
  dt <- fread(f)
  dt[, source_file := basename(f)]
  dt
}), fill = TRUE)

# Check required columns
req_cols <- c(x_col, y_col, fire_id_col, fire_year_col, value_col)
missing_cols <- setdiff(req_cols, names(points_dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns in combined point data: ",
       paste(missing_cols, collapse = ", "))
}

# Clean
points_dt <- points_dt[
  !is.na(get(x_col)) &
    !is.na(get(y_col)) &
    !is.na(get(fire_year_col))
]

if (nrow(points_dt) == 0) {
  stop("No valid points left after removing missing coordinates/fire_year.")
}

# Make sure fire_year type matches lookup
points_dt[, (fire_year_col) := as.integer(get(fire_year_col))]
raster_lookup[, fire_year := as.integer(fire_year)]

# =========================================================
# ATTACH RASTER FILE TO EACH POINT BASED ON FIRE YEAR
# =========================================================

points_dt <- merge(
  points_dt,
  raster_lookup,
  by.x = fire_year_col,
  by.y = "fire_year",
  all.x = TRUE
)

# Points with no matching raster
no_raster_pts <- points_dt[is.na(raster_file)]
if (nrow(no_raster_pts) > 0) {
  warning(nrow(no_raster_pts), " points have no matching raster for their fire_year and will be skipped.")
}

points_dt <- points_dt[!is.na(raster_file)]

if (nrow(points_dt) == 0) {
  stop("No points have matching raster files.")
}

# =========================================================
# EXTRACT YEAR-MATCHED RASTER VALUES
# =========================================================

extract_one_year <- function(dt_year) {
  
  this_year <- unique(dt_year[[fire_year_col]])
  this_raster_file <- unique(dt_year$raster_file)
  
  if (length(this_raster_file) != 1) {
    stop("More than one raster file found for fire_year = ", this_year)
  }
  
  cat("Processing fire_year:", this_year, " | n points:", nrow(dt_year), "\n")
  
  # Load raster for this year
  r <- rast(this_raster_file)
  
  if (nlyr(r) < 2) {
    stop("Raster for year ", this_year, " has fewer than 2 bands: ", this_raster_file)
  }
  
  r <- r[[1:2]]
  names(r) <- c(band1_name, band2_name)
  
  # Build point vector in WGS84
  pts <- vect(
    dt_year,
    geom = c(x_col, y_col),
    crs  = points_crs
  )
  
  # Reproject points to raster CRS if needed
  if (!same.crs(pts, r)) {
    pts <- project(pts, crs(r))
  }
  
  # Extract raster values
  ext <- terra::extract(r, pts)
  ext <- as.data.table(ext)
  ext[, ID := NULL]
  
  # Combine back
  out <- cbind(dt_year, ext)
  
  # Create severity variable from selected band
  severity_band_name <- names(r)[severity_band_index]
  out[, severity := get(severity_band_name)]
  
  # Optional relabeling
  if (use_severity_labels) {
    out[, severity := severity_map[as.character(severity)]]
  }
  
  return(out)
}

# Split by fire year and process
points_split <- split(points_dt, by = fire_year_col, keep.by = TRUE)

extracted_list <- lapply(points_split, extract_one_year)

all_points <- rbindlist(extracted_list, use.names = TRUE, fill = TRUE)

# =========================================================
# SAVE EXTRACTED POINT DATA
# =========================================================

point_output_file <- file.path(out_dir, "all_points_with_year_matched_raster_values.rds")
saveRDS(all_points, point_output_file)

cat("Saved extracted point data:\n", point_output_file, "\n")

# =========================================================
# SUMMARIZE BY fire_id + fire_year + severity
# =========================================================

summary_dt <- all_points[
  !is.na(get(fire_id_col)) &
    !is.na(get(fire_year_col)) &
    !is.na(severity) &
    !is.na(get(value_col)),
  .(
    n_points     = .N,
    mean_value   = mean(get(value_col), na.rm = TRUE),
    median_value = median(get(value_col), na.rm = TRUE),
    sd_value     = sd(get(value_col), na.rm = TRUE)
  ),
  by = c(fire_id_col, fire_year_col, "severity")
]

setorderv(summary_dt, cols = c(fire_year_col, fire_id_col, "severity"))

summary_output_file <- file.path(out_dir, "summary_by_fireid_fireyear_severity.rds")
saveRDS(summary_dt, summary_output_file)

cat("Saved summary:\n", summary_output_file, "\n")

# =========================================================
# OPTIONAL: SUMMARY WITH BAND STATS TOO
# =========================================================

summary_with_bands <- all_points[
  !is.na(get(fire_id_col)) &
    !is.na(get(fire_year_col)) &
    !is.na(severity) &
    !is.na(get(value_col)),
  .(
    n_points      = .N,
    mean_value    = mean(get(value_col), na.rm = TRUE),
    median_value  = median(get(value_col), na.rm = TRUE),
    sd_value      = sd(get(value_col), na.rm = TRUE),
    
    mean_band1    = mean(get(band1_name), na.rm = TRUE),
    median_band1  = median(get(band1_name), na.rm = TRUE),
    sd_band1      = sd(get(band1_name), na.rm = TRUE),
    
    mean_band2    = mean(get(band2_name), na.rm = TRUE),
    median_band2  = median(get(band2_name), na.rm = TRUE),
    sd_band2      = sd(get(band2_name), na.rm = TRUE)
  ),
  by = c(fire_id_col, fire_year_col, "severity")
]

setorderv(summary_with_bands, cols = c(fire_year_col, fire_id_col, "severity"))

summary_bands_output_file <- file.path(out_dir, "summary_by_fireid_fireyear_severity_with_band_stats.csv")
fwrite(summary_with_bands, summary_bands_output_file)

cat("Saved summary with band stats:\n", summary_bands_output_file, "\n")
cat("Done.\n")


########################################################
#Unburned
#######################################################

unburned <- read.csv("/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/GEDI_all_unburned_41_42_43.csv")
unburned$severity <- "unburned"

library(data.table)

setDT(unburned)

summary_dt_un <- unburned[
  !is.na(get(fire_id_col)) &
    !is.na(get(fire_year_col)) &
    !is.na(get(value_col)),
  .(
    n_points     = .N,
    mean_value   = mean(get(value_col), na.rm = TRUE),
    median_value = median(get(value_col), na.rm = TRUE),
    sd_value     = sd(get(value_col), na.rm = TRUE)
  ),
  by = c(fire_id_col, fire_year_col)
]

setorderv(summary_dt_un, cols = c(fire_year_col, fire_id_col))

summary_dt_un$severity <- "unburned"

summary_output_file <- file.path(out_dir, "summary_by_fireid_fireyear_unburned.rds")
saveRDS(summary_dt, summary_output_file)



#####################

library(data.table)

setDT(summary_dt_un)
setDT(summary_dt)

merged <- rbindlist(list(summary_dt_un, summary_dt), use.names = TRUE, fill = TRUE)

summary_output_file <- file.path(out_dir, "summary_by_fireid_fireyear_unburned_buned.rds")
saveRDS(merged, summary_output_file)

merged$time_since <- 2020 - merged$Fire_Year

#############Plot

merged <- readRDS("/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/results/summary_by_fireid_fireyear_unburned_buned.rds")
merged$time_since <- 2020 - merged$Fire_Year



summary_output_file <- file.path(out_dir, "summary_by_fireid_fireyear_unburned_buned_with_ts.rds")
saveRDS(merged, summary_output_file)

library(ggplot2)


# dev.new(width=1, height=1)
# 
# lab_text = element_text(face = "bold", color = "black", size = 18)
# lab_text_x = element_text(face = "bold", color = "black", size = 18,angle=45, margin = margin(t = 10))
# axis_text = element_text(face = "bold", color = "black", size = 18)
# ggplot() + 
#   geom_smooth(data = merged[merged$severity=="unburned",], aes(x=time_since, y=median_value),method = "lm",
#               formula = y ~ poly(x, 2), se = TRUE ,  linetype="solid",
#               color="black", fill = "gray",alpha = 0.3) +
#   geom_smooth(data = merged[merged$severity=="1",], aes(x=time_since, y=median_value),method = "lm",
#               formula = y ~ poly(x, 2), se = TRUE ,  linetype="solid",
#               color="Magenta",fill = "Plum",alpha = 0.3) +
#   geom_smooth(data = merged[merged$severity==2,], aes(x=time_since, y=median_value),method = "lm",
#               formula = y ~ poly(x, 2), se = TRUE ,  linetype="solid",
#               color="Blue Violet",fill = "Medium Orchid",alpha = 0.3) +
#   geom_smooth(data = merged[merged$severity==3,], aes(x=time_since, y=median_value),method = "lm",
#               formula = y ~ poly(x, 2), se = TRUE ,  linetype="solid",
#               color="Dark Orange 1",fill = "Tan 2",alpha = 0.3) +
#   geom_smooth(data = merged[merged$severity==4,], aes(x=time_since, y=median_value),method = "lm",
#               formula = y ~ poly(x, 2), se = TRUE ,  linetype="solid",
#               color="Red",fill = "Salmon",alpha = 0.3) +
#   
#   #geom_boxplot(data=data4,aes(x=time_since,y=percent_rec_CH,fill = factor(time_since)),outlier.size=-1, alpha = 0.1)+
#   # stat_boxplot(geom ='errorbar') +
#   # coord_cartesian(ylim = y_limits) +
#   theme(legend.position='right',legend.title = element_blank(), plot.title = element_text(size = 20)) +
#   #labs(y = "Percent recovery - Canopy cover ", x = "Time since fire (years)" ,title = "Pacific Northwest" )+
#   #labs(y = "Percent recovery - PAI ", x = "Time since fire (years)" ,title = "Pacific Northwest" )+
#   #labs(y = "Percent recovery - FHD ", x = "Time since fire (years)" ,title = "Pacific Northwest" )+
#   labs(y = "Biomass Recovery (Mg/ha) ", x = "Time since fire (years)" ,title = "" )+
#   theme(axis.line.x = element_line(size = 0.7, colour = "black"),
#         axis.line.y = element_line(size = 0.7, colour = "black"),
#         axis.line = element_line(size=1, colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = lab_text_x, axis.text.y = lab_text, axis.title = axis_text,
#         plot.title = element_text(hjust = 0.1))


#########################

lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

merged2 <- merged[merged$severity %in% c("unburned", "1", "2", "3", "4"), ]
merged2$severity <- factor(merged2$severity, levels = c("unburned", "1", "2", "3", "4"))

ggplot(merged2, aes(x = time_since, y = median_value,
                    color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "unburned" = "black",
      "1" = "magenta",
      "2" = "blueviolet",
      "3" = "darkorange1",
      "4" = "red"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "highGoodFire",
               "unmatchedRegime",
               "tooFrequent")
  ) +
  scale_fill_manual(
    values = c(
      "unburned" = "gray",
      "1" = "plum",
      "2" = "mediumorchid",
      "3" = "tan2",
      "4" = "salmon"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "highGoodFire",
               "unmatchedRegime",
               "tooFrequent")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Recovery (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )



###############################


lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

merged2 <- merged[merged$severity %in% c("unburned", "1", "3"), ]
merged2$severity <- factor(merged2$severity, levels = c("unburned", "1", "3"))

ggplot(merged2, aes(x = time_since, y = median_value,
                    color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "unburned" = "black",
      "1" = "magenta",
      "3" = "darkorange1"
          ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime"
               )
  ) +
  scale_fill_manual(
    values = c(
      "unburned" = "gray",
      "1" = "plum",
      "3" = "tan2"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Recovery (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )





lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

merged2 <- merged[merged$severity %in% c("unburned"), ]
merged2$severity <- factor(merged2$severity, levels = c("unburned"))

ggplot(merged2, aes(x = time_since, y = median_value,
                    color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "unburned" = "black"
    ),
    labels = c("Unburned"
    )
  ) +
  scale_fill_manual(
    values = c(
      "unburned" = "gray"
    ),
    labels = c("Unburned")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Recovery (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )












#############################

library(data.table)

setDT(merged)

# get unburned mean_value for each ObjectID-fire_year
unburned_ref <- merged[severity == "unburned",
                    .(OBJECTID, Fire_Year, unburned_mean_value = mean_value)
]

# join back to all rows
dat2 <- merge(
  merged,
  unburned_ref,
  by = c("OBJECTID", "Fire_Year"),
  all.x = TRUE
)


dat2_nonunburned <- dat2[severity != "unburned"]

dat2_nonunburned$diff_biomass <- dat2_nonunburned$mean_value - dat2_nonunburned$unburned_mean_value
dat2_nonunburned$diff_biomass_percent <- (dat2_nonunburned$mean_value - dat2_nonunburned$unburned_mean_value)*100/dat2_nonunburned$unburned_mean_value


lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

dat2_nonunburned2 <- dat2_nonunburned[dat2_nonunburned$severity %in% c("1", "3"), ]
dat2_nonunburned2$severity <- factor(dat2_nonunburned2$severity, levels = c("1", "3"))

ggplot(dat2_nonunburned2, aes(x = time_since, y = diff_biomass,
                    color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
            "1" = "magenta",
      "3" = "darkorange1"
    ),
    labels = c(               "lowerGoodFire",
               "unmatchedRegime"
    )
  ) +
  scale_fill_manual(
    values = c(
            "1" = "plum",
      "3" = "tan2"
    ),
    labels = c(               "lowerGoodFire",
               "unmatchedRegime")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Change Post-fire (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )



##percentage

lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

dat2_nonunburned2 <- dat2_nonunburned[dat2_nonunburned$severity %in% c("1", "3"), ]
dat2_nonunburned2$severity <- factor(dat2_nonunburned2$severity, levels = c("1", "3"))

ggplot(dat2_nonunburned2, aes(x = time_since, y = diff_biomass_percent,
                              color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "1" = "magenta",
      "3" = "darkorange1"
    ),
    labels = c(               "Low severity good wildfire",
                              "Unmatched regime wildfire"
    )
  ) +
  scale_fill_manual(
    values = c(
      "1" = "plum",
      "3" = "tan2"
    ),
    labels = c(               "Low severity good wildfire",
                              "Unmatched regime wildfire")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Change Post-fire (%)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )


###################data with all points (no fire based summary used)

burned <-readRDS("/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/results/all_points_with_year_matched_raster_values.rds")
unburned <- read.csv("/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/unburned/GEDI_all_unburned.csv")
unburned$severity <- "unburned"


setDT(burned)
setDT(unburned)

erged_points <- rbindlist(list(burned, unburned), use.names = TRUE, fill = TRUE)




lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

merged2_pts <- erged_points[erged_points$severity %in% c("unburned", "1", "3"), ]
merged2_pts$severity <- factor(merged2_pts$severity, levels = c("unburned", "1", "3"))

merged2_pts$time_since <- 2020 - merged2_pts$Fire_Year


saveRDS(merged2_pts, "/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/results/bunred_unburned_merge_points.rds")


ggplot(merged2_pts, aes(x = time_since, y = agbd,
                    color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "unburned" = "black",
      "1" = "magenta",
      "3" = "darkorange1"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime"
    )
  ) +
  scale_fill_manual(
    values = c(
      "unburned" = "gray",
      "1" = "plum",
      "3" = "tan2"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Recovery (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )



###########################unburned_within_regime


all_points_2 <- readRDS(file.path(out_dir, "unburned_points_with_year_matched_raster_values_regime_in.rds"))

all_points_2$severity <- as.character(all_points_2$severity)
all_points_2$severity <- "unburned"
 
  
setDT(burned)
setDT(all_points_2)

erged_points2 <- rbindlist(list(burned, all_points_2), use.names = TRUE, fill = TRUE)




lab_text = element_text(face = "bold", color = "black", size = 18)
lab_text_x = element_text(face = "bold", color = "black", size = 18, angle = 45, margin = margin(t = 10))
axis_text = element_text(face = "bold", color = "black", size = 18)

merged2_pts <- erged_points[erged_points2$severity %in% c("unburned", "1", "3"), ]
merged2_pts$severity <- factor(merged2_pts$severity, levels = c("unburned", "1", "3"))

merged2_pts$time_since <- 2020 - merged2_pts$Fire_Year


# saveRDS(merged2_pts, "/data-store/iplant/home/nilangakoon/Fire_recovery/good_fire/new_data/results/bunred_unburned_merge_points.rds")


ggplot(merged2_pts, aes(x = time_since, y = agbd,
                        color = severity, fill = severity)) + 
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE,
              linetype = "solid",
              alpha = 0.3) +
  scale_color_manual(
    values = c(
      "unburned" = "black",
      "1" = "magenta",
      "3" = "darkorange1"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime"
    )
  ) +
  scale_fill_manual(
    values = c(
      "unburned" = "gray",
      "1" = "plum",
      "3" = "tan2"
    ),
    labels = c("Unburned",
               "lowerGoodFire",
               "unmatchedRegime")
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    plot.title = element_text(size = 20)
  ) +
  labs(
    y = "Biomass Recovery (Mg/ha)",
    x = "Time since fire (years)",
    title = ""
  ) +
  theme(
    axis.line.x = element_line(size = 0.7, colour = "black"),
    axis.line.y = element_line(size = 0.7, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = lab_text_x,
    axis.text.y = lab_text,
    axis.title = axis_text,
    plot.title = element_text(hjust = 0.1)
  )

