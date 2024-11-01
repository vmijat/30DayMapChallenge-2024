library(tidyverse)
library(sf)
library(osmdata)
library(tmap)
library(osrm)
library(gstat)
library(sp)
library(terra)

# Get city shape
city_name <- "Cologne, Germany"
city_shp <- getbb(city_name, format_out = "sf_polygon")

# Query OSM for the River Rhine
rhine_shp <- opq(bbox = c(1, 46, 9, 53)) %>%
  add_osm_feature(key = "name", value = "Rhein") %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

rhine_cgn_shp <- st_intersection(rhine_shp$osm_multilines, city_shp)
rhine_cgn_shp <- st_crop(rhine_shp$osm_multilines, st_bbox(city_shp))

# OSM query for supermarkets
supermarkets <- opq(bbox = st_bbox(city_shp)) %>%
  add_osm_feature(key = "shop", value = "supermarket") %>%
  osmdata_sf() %>%
  .$osm_points

supermarkets |> 
  st_drop_geometry() |> 
  filter(!is.na(name)) |>
  count(name, sort = TRUE)

# Keep REWE supermarkets only
rewe <- supermarkets |> 
  filter(str_detect(name, "(?i)rewe\\b")) |> 
  st_intersection(city_shp)
nrow(rewe)

ggplot() +
  geom_sf(data = city_shp) +
  geom_sf(data = rewe)


# Generate a grid of points within the city boundary 
grid_points <- st_make_grid(city_shp, cellsize = 0.005, what = "centers") %>%
  st_sf() %>%
  st_intersection(city_shp)
plot(grid_points)


# Calculate travel time from a point to the supermarkets
calculate_travel_time <- function(point, dest) {
  result <- osrmTable(src = st_sf(point), dst = dest, measure = "duration", 
                      osrm.profile = "foot")
  # Extract the duration (in minutes) of the closest supermarket
  min(result$duration[1, ], na.rm = TRUE)
}

calculate_travel_time(grid_points[1500, ], rewe[rewe$osm_id == "252043802", ])
calculate_travel_time(grid_points[1, ], rewe)

# Calculate walking times to the nearest supermarket for each grid point
# Set TRUE to run the query
path_time_to_rewe <- file.path("data", "time_to_rewe.rds")
if (FALSE) {
  grid_points$time_to_rewe <- map_dbl(
    seq_len(nrow(grid_points)), 
    function(x) calculate_travel_time(grid_points[x, ], rewe))
  write_rds(grid_points$time_to_rewe, path_time_to_rewe)
} else {
  grid_points$time_to_rewe <- read_rds(path_time_to_rewe)
}


ggplot() +
  geom_sf(data = city_shp) +
  geom_sf(
    data = grid_points,
    aes(color = time_to_rewe)
  ) +
  scale_color_gradient(transform = "pseudo_log")

# ----------


# Convert grid points to a data frame for IDW
grid_data <- st_coordinates(grid_points) %>%
  as.data.frame() %>%
  bind_cols(time_to_rewe = grid_points$time_to_rewe)

# Convert the data frame to a SpatialPointsDataFrame for IDW
coordinates(grid_data) <- ~X + Y

# Define the formula for the IDW interpolation
idw_formula <- time_to_rewe ~ 1

# Create a grid for interpolation using the city boundary extent
interpolation_grid <- st_make_grid(city_shp, cellsize = 0.001, what = "centers")
interpolation_grid <- as.data.frame(st_coordinates(interpolation_grid))
coordinates(interpolation_grid) <- ~X + Y
gridded(interpolation_grid) <- TRUE

# Perform IDW interpolation
idw_result <- idw(formula = idw_formula, locations = grid_data, 
                  newdata = interpolation_grid, idp = 3)

# Convert the IDW result back to an sf object
idw_sf <- st_as_sf(as.data.frame(idw_result), coords = c("X", "Y"), 
                   crs = st_crs(city_shp))
idw_sf$var1.pred <- idw_result$var1.pred

idw_sf2 <- st_intersection(idw_sf, city_shp)

idw_sf2$var1.pred_cat <- cut(idw_sf2$var1.pred, 
                             breaks = c(0, 2, 5, 10, 15, 20, 30, Inf),
                             include.lowest	= TRUE, right = FALSE)


grid_sf <- st_as_sf(grid_data, coords = c("X", "Y"), crs = st_crs(city_shp))
summary(grid_sf$time_to_rewe)

raster_template <- rast(
  extent = st_bbox(city_shp), # Set the extent to match the city boundary
  resolution = 0.001, # Set a reasonable resolution (adjust as needed)
  crs = st_crs(city_shp)
)

raster_interpolation <- terra::rasterize(
  grid_sf, raster_template, 
  field = "time_to_rewe",
  fun = mean 
)
summary(values(raster_interpolation))


ggplot() +
  geom_sf(data = city_shp, fill = "#FEE5D9", color = "white") +
  geom_contour_filled(
    data = as.data.frame(raster_interpolation, xy = TRUE),
    aes(x = x, y = y, z = mean, fill = after_stat(level)),
    breaks = c(0, 2, 5, 10, 15, 20, Inf)) +
  geom_sf(data = rhine_cgn_shp, col = "white", linewidth = 1) +
  geom_sf(data = rewe, color = "white", size = 0.35) +
  geom_sf(data = rewe, color = "#121212", size = 0.25) +
  # scale_fill_viridis_d() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  guides(fill = guide_legend()) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#424242", fill = "#424242")
  )
ggsave(file.path("plots", "xx-rewe-2.png"), width = 6, height = 5, dpi = 500)


####

ggplot() +
  geom_sf(data = city_shp, fill = NA, color = "black") +
  geom_sf(
    data = idw_sf2,
    aes(color = var1.pred_cat),
    linewidth = 0
  ) +
  geom_sf(data = rhine_cgn_shp, col = "white", linewidth = 1) +
  geom_sf(data = rewe, color = "red", size = 0.25) +
  scale_color_viridis_d(
    name = "Walking Time (min)", aesthetics = c("color", "fill")) +
  guides(color = guide_colorsteps()) +
  theme_minimal()


## Add street data -------------------------------------------------------------

# Get streets

highway_features <- opq(bbox = st_bbox(city_shp), timeout = 1200) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
write_rds(highway_features,
          file.path("data", "highway_features_cgn.rds"), compress = "gz")

highway_features_filtered <- highway_features$osm_lines %>%
  filter(., st_intersects(., city_shp, sparse = FALSE)[, 1]) %>%
  st_intersection(city_shp)|> 
  select(osm_id, highway)
write_rds(highway_features_filtered,
          here("data", glue("highway_features_filtered_cgn.rds")), compress = "gz")

# Delete highway features object
rm(highway_features)

street_types <- list(
  large = c("motorway", "primary", "motorway_link", "primary_link"),
  medium = c("secondary", "tertiary", "secondary_link", "tertiary_link"),
  small = c("residential", "living_street", "unclassified", "service", "footway")
)


p <- ggplot() +
  geom_sf(data = city_shp, fill = "white", color = "white") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$small),
          linewidth = 0.05, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$medium),
          linewidth = 0.1, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$large),
          linewidth = 0.25, col = "#434343") +
  geom_contour_filled(
    data = as.data.frame(raster_interpolation, xy = TRUE) |> 
      filter(mean <= 60),
    aes(x = x, y = y, z = mean, fill = after_stat(level)),
    breaks = c(0, 2, 5, 10, 15, 20, 30),
    alpha = 0.75) +
  # geom_sf(data = rhine_cgn_shp, col = "white", linewidth = 1) +
  geom_sf(data = rewe, color = "white", size = 0.35) +
  geom_sf(data = rewe, color = "#121212", size = 0.25) +
  # scale_fill_viridis_d() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  guides(fill = guide_legend()) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#888888", fill = "#888888")
  )
ggsave(file.path("plots", "xx-rewe-3.png"), width = 12, height = 10, dpi = 600)
