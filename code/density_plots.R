

rm(list=ls()) #Ensure empty workspace if running from beginning

if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("utils", "functions.R"))

##################################
###### USER FILLED OUT PARAMETERS
##################################

# Color scheme
rxCol <- col2hex("darkblue")
gFCol <- "#dc0ab4ff"
totCol <- col2hex("goldenrod2")
gfHsCol <- '#FA6EE2' #"#FA97E7"
boundaryCol <- "gray50"
bgNeutralCol <- "gray95"


install_and_load_packages(c(
  "tidyverse", #Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  "sf",
  "tigris",
  "patchwork",
  "purrr",
  "grid",
  "scales")) #add commas to axis


#Ensure necessary directories are created
derivedDatDir <- here::here("data", "derived")
dir_figs <- here('figs_welty_wildfire_fri_splitfrg5')
dir_ensure(derivedDatDir)
dir_ensure(dir_figs)


dats <- st_read(here(derivedDatDir, "merged_goodfire_final_states_welty_wildfire_fri_splitfrg5.gpkg"))


dats <- dats |>
  mutate(perc_gf = (goodFireAll / yearPriorForest) * 100,
         perc_forest = (yearPriorForest / totalArea) * 100)

states <- tigris::states()
west <- states[states$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),]  |>
  st_transform(st_crs(dats))

dats <- dats |>
  sf::st_join(west)


dats_forest <- dats |>
  filter(perc_forest >= 10)





make_density_plots <- function(dats) {
  
  # Compute medians and re-order levels
  med <- median(dats$perc_gf, na.rm = TRUE)
  
  meds <- dats |>
    group_by(NAME) |>
    summarise(med = median(perc_gf, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(med))
  
  facet_levels <- meds$NAME
  
  dats <- dats |>
    mutate(NAME = factor(NAME, levels = facet_levels))
  
  meds <- meds |>
    mutate(NAME = factor(NAME, levels = facet_levels))
  
  # Plots
  p_all <- ggplot(dats, aes(x = perc_gf)) +
    geom_density() +
    geom_vline(xintercept = med, linetype = "dashed", color = "black") +
    labs(y = "Event Density") +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      axis.title.y = element_text(angle = 90),
    )
  p_all
  
  p_facet <- ggplot(dats, aes(x = perc_gf)) +
    geom_density() +
    geom_vline(
      data = meds,
      aes(xintercept = med),
      linetype = "dashed"
    ) +
    facet_wrap(~ NAME, ncol = 1, strip.position = "left") +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      #axis.title.x = element_text()
    )
  
  x_lab <- wrap_elements(
    grid::textGrob(
      "Percent of burned forest impacted by good wildfire",
      gp = grid::gpar(fontsize = 11)
    )
  )
  
  # Combine
  p_combined <- ((p_all + p_facet) +
                   plot_layout(widths = c(3, 1))) /
    x_lab +
    plot_layout(heights = c(20, 1))
  
  p_combined
  
  
  return(p_combined)
  
}


all_p <- make_density_plots(dats)
forest_p <- make_density_plots(dats_forest)

all_p
forest_p

ggsave(filename = here(dir_figs, "density_all_events.png"), plot = all_p)
ggsave(filename = here(dir_figs, "density_min10percentforested_events.png"), plot = forest_p)






make_colored_density_plots <- function(dats, totCol, gFCol) {

  x_rng <- range(dats$perc_gf, na.rm = TRUE, finite = TRUE)
  
  density_tbl <- function(x, group_name = NULL, from = x_rng[1], to = x_rng[2], n = 512) {
    d <- density(x, na.rm = TRUE, from = from, to = to, n = n)
    
    tibble(
      x = d$x,
      y = d$y,
      NAME = group_name
    )
  }
  
  med <- median(dats$perc_gf, na.rm = TRUE)
  
  meds <- dats |>
    group_by(NAME) |>
    summarise(
      med = median(perc_gf, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(med))
  
  facet_levels <- meds$NAME
  
  dats <- dats |>
    mutate(NAME = factor(NAME, levels = facet_levels))
  
  meds <- meds |>
    mutate(NAME = factor(NAME, levels = facet_levels))
  
  dens_all <- density_tbl(dats$perc_gf)
  
  dens_facet <- dats %>%
    split(.$NAME) |>
    imap_dfr(function(df, nm) {
      density_tbl(df$perc_gf, group_name = nm)
    }) |>
    mutate(NAME = factor(NAME, levels = facet_levels))
  
  p_all <- ggplot(dens_all, aes(x = x, y = y, color = x)) +
    geom_line(linewidth = 1.1) +
    geom_vline(
      aes(xintercept = med, color = med),
      data = tibble(med = med),
      linetype = "dashed",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_color_gradient(
      low = totCol,
      high = gFCol,
      limits = x_rng,
      guide = "none"
    ) +
    labs(y = "Event Density") +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      axis.title.y = element_text(angle = 90)
    ) +
    scale_y_continuous(limits = c(0, NA))
  
  p_facet <- ggplot(dens_facet, aes(x = x, y = y, color = x, group = NAME)) +
    geom_line(linewidth = 0.9) +
    geom_vline(
      data = meds,
      aes(xintercept = med, color = med),
      linetype = "dashed",
      linewidth = 0.6,
      show.legend = FALSE
    ) +
    facet_wrap(~NAME, ncol = 1, strip.position = "left") +
    scale_color_gradient(
      low = totCol,
      high = gFCol,
      limits = x_rng,
      guide = "none"
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      axis.ticks.x = element_line()
    ) +
    scale_y_continuous(limits = c(0, NA))
  
  x_lab <- wrap_elements(
    grid::textGrob(
      "Percent of burned forest impacted by good wildfire",
      gp = grid::gpar(fontsize = 11)
    )
  )
  
  p_combined <- ((p_all + p_facet) +
                   plot_layout(widths = c(3, 1))) /
    x_lab +
    plot_layout(heights = c(20, 1))
  
  p_combined
}



all_p_color <- make_colored_density_plots(dats, totCol = totCol, gFCol = gFCol)
forest_p_color <- make_colored_density_plots(dats_forest, totCol = totCol, gFCol = gFCol)

all_p_color
forest_p_color

ggsave(filename = here(dir_figs, "density_all_events_color.png"), plot = all_p_color)
ggsave(filename = here(dir_figs, "density_min10percentforested_events_color.png"), plot = forest_p_color)

