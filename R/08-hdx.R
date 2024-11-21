library(tidyverse)
library(terra)
library(sf)
library(ggtext)

data_path <- file.path("data", "HDX")

#' Manually download the raster data from 
#' https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates

raster_paths <- file.path(
  data_path, c("population_deu_2019-07-01.tif", "DEU_elderly_60_plus.tif", 
               "DEU_children_under_five.tif", "DEU_youth_15_24.tif"))
rasters <- map(raster_paths, terra::rast) 
raster_crs <- terra::crs(rasters[[1]])


# Downsample the rasters
downsample_raster <- function(raster, factor = 25) {
  terra::aggregate(raster, fact = factor, fun = sum, na.rm = TRUE)
}
raster_downsampled_pop_full <- downsample_raster(rasters[[1]])
raster_downsampled_pop_group <- downsample_raster(rasters[[4]])

# Combine the rasters: calculate the share of the subgroup 
raster_downsampled_combined <- raster_downsampled_pop_group / raster_downsampled_pop_full

# Transform to a data.frame for ggplot
raster_downsampled_combined_df <- as.data.frame(
  raster_downsampled_combined, xy = TRUE 
  ) |> 
  rename(population = `Population Count`) |> 
  mutate(population = replace_na(population, 0))

# Set breaks and labels for the contours
contour_breaks <- seq(
  floor(min(raster_downsampled_combined_df$population) * 100) / 100, 
  ceiling(max(raster_downsampled_combined_df$population) * 100) / 100, 0.02)
contour_labels <- sprintf(
  "%s-%s %%", 
  contour_breaks * 100,
  c(contour_breaks[2:length(contour_breaks)] * 100, ""))
contour_labels[2:length(contour_breaks)] <- paste0(
  ">", contour_labels[2:length(contour_breaks)])

p <- raster_downsampled_combined_df |> 
  ggplot() +
  geom_contour_filled(
    aes(x, y, z = population),
    breaks = contour_breaks) +
  scale_fill_brewer(labels = contour_labels, direction = -1) +
  coord_sf(crs = raster_crs) +
  guides(fill = guide_legend(
    title.position = "top",
    override.aes = list(color = "white", linewidth = 0.2))) +
  labs(
    title = "Where's the Youth?",
    subtitle = "Share of people aged 15 to 24 among the population<br>on a
    high-resolution grid",
    caption = "Source: Data for Good at Meta via The Humanitarian Data Exchange.
    Visualization: Ansgar Wolsing",
    fill = "Share of population aged 15-24 (%)<br>
    <i style='font-size:6pt'>Lighter shades indicate areas with a higher share 
    of young people</i>"
  ) +
  theme_void(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = "transparent", fill = "#121212"),
    text = element_text(color = "#FCFCFC"),
    legend.position = "bottom",
    legend.key.width = unit(3.5, "mm"),
    legend.key.height = unit(3.5, "mm"),
    legend.title = element_markdown(size = 7, lineheight = 1.1),
    legend.text = element_text(size = 6),
    plot.margin = margin(rep(4, 4)),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans SemiBold", size = 18),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9, lineheight = 1.1),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, margin = margin(t = 10, b = 2))
  )
ggsave(file.path("plots", "08-hdx.png"), width = 5, height = 5)
