

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


# assess 10% cutoff
forest_perc_all_goodwf <- sum(dats_forest$goodFireAll) / sum(dats$goodFireAll) * 100
forest_perc_all_forest <- sum(dats_forest$yearPriorForest) / sum(dats$yearPriorForest) * 100
nrow(dats_forest) / nrow(dats) * 100


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
  
  
  return(list(plot = p_combined, med = med, meds = meds))
  
}


all_p <- make_density_plots(dats)
forest_p <- make_density_plots(dats_forest)

all_p
forest_p

ggsave(filename = here(dir_figs, "density_all_events.png"), plot = all_p$plot)
ggsave(filename = here(dir_figs, "density_min10percentforested_events.png"), plot = forest_p$plot)






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
  
  return(list(combined = p_combined, all = p_all, facet = p_facet))
}



#all_p_color <- make_colored_density_plots(dats, totCol = totCol, gFCol = gFCol)
forest_p_color <- make_colored_density_plots(dats_forest, totCol = totCol, gFCol = gFCol)

#all_p_color
forest_p_color

#ggsave(filename = here(dir_figs, "density_all_events_color.png"), plot = all_p_color)
ggsave(filename = here(dir_figs, "density_min10percentforested_events_color.png"), plot = forest_p_color$combined)




ggplot(dats |> filter(perc_forest >= 10)) +
  geom_point(aes(x = perc_gf, y = perc_forest))


ggplot(dats |> filter(perc_forest >= 10)) +
  geom_point(aes(x = GIS_Hectar, y = perc_gf, color = perc_gf), alpha = 0.4) +
  scale_x_continuous(trans = "log10") +
  theme_minimal() +
  labs(x = "Fire size (ha)", y = "Percent of burned forest impacted by good wildfire") +
  scale_color_gradient(
    low = totCol,
    high = gFCol,
    guide = "none"
  ) 






scatter <- ggplot(dats |> filter(perc_forest >= 10 & GIS_Hectar > 0)) +
  geom_point(aes(x = perc_gf, y = GIS_Hectar, color = perc_gf), alpha = 0.4) +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  theme_void() +
  labs(y = "Fire size\n(log-transformed hectares)"#,
       #x = "Percent of burned forest impacted by good wildfire"
       ) +
  geom_smooth(aes(x = perc_gf, y = GIS_Hectar), method = "lm", formula = y ~ poly(x,2), color = "black") +
  scale_color_gradient(
    low = totCol,
    high = gFCol,
    guide = "none"
  ) +
  theme(
    axis.text.x = element_text(),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(angle = 90)
  )
scatter



x_lab <- wrap_elements(
  grid::textGrob(
    "Percent of burned forest impacted by good wildfire",
    gp = grid::gpar(fontsize = 11)
  )
)

p_combined <- (((scatter / forest_p_color$all) | forest_p_color$facet) +
                 plot_layout(widths = c(3, 1))) /
  x_lab +
  plot_layout(heights = c(20, 1))
p_combined
ggsave(filename = here(dir_figs, "density_min10percentforested_events_color_with_scatter.png"),
       plot = p_combined,
       units = "px",
       width = 2000, height = 1500)






# Generate tables

top_gwf_events <- data.frame()

for(state in unique(dats_forest$STUSPS)) {
  st_events <- dats_forest |>
    filter(STUSPS == state & perc_forest > 80 & perc_gf > 95) #|>
    #arrange(desc(perc_gf)) |>
    #slice_head(n = 5)
  top_gwf_events <- rbind(top_gwf_events, st_events)
}

top_gwf_events <- top_gwf_events |>
  select(perc_gf, perc_forest, STUSPS, GIS_Hectar, Listed_F_1, Fire_Year, goodFireAll, highGoodFire, lowerGoodFire) |>
  arrange(STUSPS)

ggplot(top_gwf_events) +
  geom_density(aes(x = Fire_Year))

ggplot(top_gwf_events) +
  geom_histogram(aes(x = GIS_Hectar))

ggplot(top_gwf_events) +
  geom_bar(aes(x = STUSPS))


top_gwf_events |> 
  group_by(STUSPS) |>
  summarize(n = n())



# Cumulative stats

dats_cum <- dats_forest |>
  arrange(desc(perc_gf)) |>
  mutate(
    cum_goodFireAll = cumsum(goodFireAll),
    cum_prop = cum_goodFireAll / sum(goodFireAll, na.rm = TRUE)
  )




cumulative_p <- ggplot(dats_cum, aes(x = perc_gf, y = cum_prop, color = perc_gf)) +
  geom_line(linewidth = 1.2) +
  scale_x_reverse() +
  scale_color_gradient(
    low = totCol,
    high = gFCol
  ) +
  labs(
    x = "Event percent of burned forest impacted by good wildfire (high → low)",
    y = "Cumulative proportion of forested good wildfire area"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray80"),
    axis.ticks = element_line(color = "gray80"),
    panel.grid.major.x = element_line(color = "gray90", linetype = 2),
    panel.grid.major.y = element_line(color = "gray90", linetype = 2)
  ) +
  theme(legend.position = "none")
cumulative_p

ggsave(filename = here(dir_figs, "cumulative_plot.png"),
       plot = cumulative_p,
       units = "px",
       width = 2000,
       height = 1500)

gwf75 <- dats_forest |> filter(perc_gf >= 75)
gwf50_75 <- dats_forest |> filter(perc_gf < 75 & perc_gf >= 50)
gwf25_50 <- dats_forest |> filter(perc_gf < 50 & perc_gf >= 25)


# RAW 75

stusps_gwf75 <- ggplot(gwf75) +
  geom_bar(aes(x = STUSPS), fill = gFCol) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text()
  )

ggsave(filename = here(dir_figs, "stusps_gwf75.png"),
       plot = stusps_gwf75,
       units = "px",
       width = 1000,
       height = 750,
       bg = "white")



# Percent of events 75



state_gwf75_props <- dats_forest |>
  count(STUSPS, name = "n_total") |>
  left_join(
    gwf75 |>
      count(STUSPS, name = "n_gwf75"),
    by = "STUSPS"
  ) |>
  mutate(
    n_gwf75 = coalesce(n_gwf75, 0),
    pct_gwf75 = 100 * n_gwf75 / n_total
  )

stusps_gwf75_pct <- ggplot(state_gwf75_props) +
  geom_col(aes(x = STUSPS, y = pct_gwf75), fill = gFCol) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text()
  )

ggsave(
  filename = here(dir_figs, "stusps_gwf75_pct.png"),
  plot = stusps_gwf75_pct,
  units = "px",
  width = 1000,
  height = 750,
  bg = "white"
)






# Other subsets

stusps_gwf50_75 <- ggplot(gwf50_75) +
  geom_bar(aes(x = STUSPS)) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text()
  )

ggsave(filename = here(dir_figs, "stusps_gwf50_75.png"),
       plot = stusps_gwf50_75,
       units = "px",
       width = 1000,
       height = 750,
       bg = "white")


stusps_gwf25_50 <- ggplot(gwf25_50) +
  geom_bar(aes(x = STUSPS)) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text()
  )

ggsave(filename = here(dir_figs, "stusps_gwf25_50.png"),
       plot = stusps_gwf25_50,
       units = "px",
       width = 1000,
       height = 750,
       bg = "white")






ggplot(gwf75) +
  geom_histogram(aes(x = GIS_Hectar))


ggplot(gwf75) +
  geom_point(aes(y = GIS_Hectar, x = perc_gf, color = perc_gf)) +
  scale_y_log10(
    breaks = breaks_log(n = 6),
    labels = label_comma()
  ) +
  scale_color_gradient(
    low = totCol,
    high = gFCol,
    limits = c(0, 100),
    guide = "none"
  )


top_gwf_events |> 
  group_by(STUSPS) |>
  summarize(n = n()) 




