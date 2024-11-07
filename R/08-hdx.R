library(tidyverse)
library(terra)
library(sf)
library(ggspatial)
library(ggtext)

data_path <- file.path("data", "HDX")

#' Manually download the raster data from 
#' https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates

raster_paths <- file.path(
  data_path, c("population_deu_2019-07-01.tif", "DEU_elderly_60_plus.tif", 
               "DEU_children_under_five.tif"))
rasters <- map(raster_paths, rast) 


raster_combined <- rasters[[2]] / rasters[[1]]
plot(raster_combined)

# Remove NAs
na_mask <- !is.na(raster_combined)
raster_no_na <- mask(raster_combined, na_mask)
plot(raster_no_na)
nlyr(raster_no_na)
ncell(raster_no_na)
res(rasters[[1]])

raster_crs <- crs(raster_downsampled_pop_full)

# Downsample the rasters
downsample_factor <- 20
raster_downsampled_pop_full <- 
  terra::aggregate(rasters[[1]], fact = downsample_factor, fun = sum, na.rm = TRUE)
raster_downsampled_pop_60_plus <- 
  terra::aggregate(rasters[[2]], fact = downsample_factor, fun = sum, na.rm = TRUE)
ncell(raster_downsampled_pop_full) / ncell(raster_no_na)

# ##
# downsample_factor <- 50
# current_res <- res(rasters[[1]])
# new_res <- current_res * downsample_factor
# names(new_res) <- c("x", "y")
# new_raster <- rast(ext(rasters[[1]]), 
#                    resolution = new_res,
#                    crs = raster_crs)
# raster_resampled <- resample(rasters[[1]], new_raster, method = "sum")
# ncell(raster_resampled)

# Combine by division
raster_downsampled_combined <- raster_downsampled_pop_60_plus / raster_downsampled_pop_full

# Transform to a data.frame for ggplot
raster_downsampled_combined_df <- as.data.frame(
  raster_downsampled_combined, xy = TRUE #, na.rm = FALSE
  ) |> 
  rename(population = `Population Count`) |> 
  mutate(population = replace_na(population, 0))

  
p <- raster_downsampled_combined_df |> 
  ggplot() +
  geom_contour_filled(
    aes(x, y, z = population)) +
  coord_sf(crs = raster_crs) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#121212", fill = "#121212"),
    text = element_text(color = "#FCFCFC")
  )
ggsave(file.path("plots", "08-hdx.png"), width = 6, height = 5)
