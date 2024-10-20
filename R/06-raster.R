library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

data_path <- file.path("data", "GHSL")

#' Manually download the raster data from 
#' https://human-settlement.emergency.copernicus.eu/download.php?ds=builtH

ghsl_raster_path <- file.path("GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19", 
                              "GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif")
ghsl_raster <- rast(file.path(data_path, ghsl_raster_path))

raster_crs <- crs(ghsl_raster)

st_bbox(ghsl_raster) |> 
  st_as_sfc() |> 
  st_transform(crs = "EPSG:4326")

# Define a bounding box for an area of interest

# xmin      ymin      xmax      ymax 
# 3.370794 51.343462  3.448647 51.391638 

bbox <- st_bbox(c(xmin = 3.15, xmax = 3.5, ymin = 51.27, ymax = 51.4), 
                crs = "EPSG:4326")
bbox <- st_bbox(c(xmin = 2.72, xmax = 3.5, ymin = 51.2, ymax = 51.4), 
                crs = "EPSG:4326")
# bbox <- st_bbox(c(xmin = 0.35, xmax = 0.5, ymin = 52.7, ymax = 52.8), 
#                 crs = "EPSG:4326")
bbox_sf <- st_as_sfc(bbox)

# Optionally, convert to the same coordinate reference system as the raster
bbox_sf <- st_transform(bbox_sf, crs = raster_crs)

ghsl_cropped <- crop(ghsl_raster, bbox_sf)

ghsl_df <- as.data.frame(ghsl_cropped, xy = TRUE, na.rm = TRUE) 

# ghsl_df <- as.data.frame(ghsl_cropped, xy = TRUE, na.rm = TRUE) |> 
#   st_as_sf(coords = c("x", "y"), crs = crs(ghsl_raster))


str(ghsl_df)

ghsl_df |> 
  mutate(height = GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19) |> 
  filter(height > 0) |> 
  ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_tile(
    aes(x, y, fill = height),
    alpha = 0.7) +
  scale_fill_viridis_c(
    option = "viridis", na.value = "grey" #, transform = "pseudo_log"
    ) +
  theme_void() +
  labs(
    fill = "Height (meters)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(crs = crs(ghsl_raster))  



town_name <- "Knokke-Heist, Belgium"
town_shp <- getbb(town_name, format_out = "sf_polygon")
st_crs(town_shp) <- "EPSG:4326"

town_name_2 <- "Oostende, Belgium"
town_shp_2 <- getbb(town_name_2, format_out = "sf_polygon")
st_crs(town_shp_2) <- "EPSG:4326"

# Combine the town shapes
town_shp_combined <- st_union(town_shp, town_shp_2)

mask_sf <- st_transform(town_shp_combined, crs = st_crs(ghsl_raster))

# Convert sf object to a SpatVector
mask_vect <- vect(mask_sf)

# Mask the cropped raster with the polygon to limit to the precise shape
masked_raster <- mask(ghsl_cropped, mask_vect)

ghsl_masked_df <- as.data.frame(masked_raster, xy = TRUE, na.rm = TRUE) 

ghsl_masked_df |> 
  mutate(height = GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19) |> 
  filter(height > 0) |> 
  ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_tile(
    aes(x, y, fill = height),
    alpha = 0.7) +
  scale_fill_viridis_c(
    option = "viridis", na.value = "grey", transform = "pseudo_log"
  ) +
  theme_void() +
  labs(
    fill = "Height (meters)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(crs = crs(ghsl_raster))  

