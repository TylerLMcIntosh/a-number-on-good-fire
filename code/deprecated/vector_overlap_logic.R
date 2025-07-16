
get_non_overlapping_parts_fast <- function(polygons) {
  intersections <- st_intersects(polygons, polygons, sparse = TRUE)
  
  result <- map_dfr(seq_len(nrow(polygons)), function(i) {
    this_poly <- polygons[i, ]
    
    neighbors <- setdiff(intersections[[i]], i)
    
    if (length(neighbors) == 0) {
      return(this_poly)
    }
    
    other_union <- st_union(st_geometry(polygons[neighbors, ]))
    diff_geom <- st_difference(st_geometry(this_poly), other_union)
    
    if (length(diff_geom) == 0 || all(st_is_empty(diff_geom))) {
      return(NULL)
    } else {
      this_poly$geometry <- diff_geom
      return(this_poly)
    }
  })
  
  return(result)
}

get_non_overlapping_parts_fast_2 <- function(polygons1, polygons2) {
  # Precompute spatial index for efficiency
  intersection_index <- st_intersects(polygons1, polygons2, sparse = TRUE)
  
  result <- map_dfr(seq_len(nrow(polygons1)), function(i) {
    this_poly <- polygons1[i, ]
    neighbors_idx <- intersection_index[[i]]
    
    if (length(neighbors_idx) == 0) {
      return(this_poly)  # No overlap — return original
    }
    
    overlapping_union <- st_union(st_geometry(polygons2[neighbors_idx, ]))
    diff_geom <- st_difference(st_geometry(this_poly), overlapping_union)
    
    if (length(diff_geom) == 0 || all(st_is_empty(diff_geom))) {
      return(NULL)
    } else {
      this_poly$geometry <- diff_geom
      return(this_poly)
    }
  })
  
  return(result)
}


create_no_reburn_dataset <- function(dats, subset_start_year, subset_end_year, pre_start_year, pre_end_year, year_col) {
  subset <- dats |> 
    dplyr::filter({{year_col}} >= subset_start_year & {{year_col}} <= subset_start_year)
  
  pre_subset <- welty |>
    dplyr::filter({{year_col}}  <= pre_end_year & {{year_col}} >= pre_start_year)
  
  
  tic("Subset resolve overlap")
  subset_no_self_reburn <- subset |>
    get_non_overlapping_parts_fast()
  toc()
  tic("Non-subset resolve overlap")
  subset_no_reburn_all <- subset_no_self_reburn |>
    get_non_overlapping_parts_fast_2(polygons2 = pre_subset)
  toc()
  
  return(subset_no_reburn_all)
}



# Use functions
welty_no_reburn_2010_2020 <- create_no_reburn_dataset(
  dats = welty_wf,
  subset_start_year = 2010,
  subset_end_year = 2020,
  pre_start_year = 1984,
  pre_end_year = 2009,
  year_col = Fire_Year
)

welty_no_reburn_1984_2020 <- create_no_reburn_dataset(
  dats = welty_wf,
  start_year = 1984,
  end_year = 2020,
  year_col = Fire_Year
)

# Get overall area
welty_wf_og <- welty_wf |>
  filter(Fire_Year >= 2010 & Fire_Year <= 2020) |>
  st_union()
welty_wf_og_ha <- welty_wf_og |>
  st_area() |>
  sum() %>%
  {. * 0.0001}
welty_wf_no_reburn_ha <- welty_no_reburn_2010_2020 |>
  st_area() |>
  sum() %>%
  {. * 0.0001}

reburned_ha <- welty_wf_og_ha - welty_wf_no_reburn_ha
perc_reburn <- (reburned_ha / welty_wf_og_ha) * 100
perc_reburn
