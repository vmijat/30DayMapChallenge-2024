library(tidyverse)
library(sf)
library(osmdata)
library(osrm)
library(terra)
library(tidygeocoder)

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


# Read dataset of REWE markets
rewe <- read_csv(file.path("data", "rewe-markets-cgn.csv"))

# Geocode the REWE markets' addresses
rewe <- geocode(rewe, address, method = "arcgis")
rewe <- st_as_sf(rewe, coords = c("long", "lat"), crs = st_crs(4326))
st_crs(rewe)

# Generate a grid of points within the city boundary 
buffer <- 1000
grid_points <- st_make_grid(city_shp, cellsize = 0.001, what = "centers") %>%
  st_sf() %>%
  st_intersection(st_buffer(city_shp, dist = buffer))


# Calculate walking times to the nearest supermarket for each grid point
path_time_to_rewe <- file.path("data", "time_to_rewe.rds")

chunk_size <- 100
grid_length <- nrow(grid_points)
chunk_start <- seq(1, grid_length, chunk_size)
chunk_end <- chunk_start + chunk_size - 1
chunk_end[which.max(chunk_end)] <- pmin(chunk_end[which.max(chunk_end)], grid_length)

foo <- map2(
  chunk_start, chunk_end,
  function(x, y) {
    routing_table <- osrmTable(
      src = st_geometry(grid_points[x:y, ]),
      dst = st_geometry(rewe)
    ) 
    # calculate the minimum per grid cell
    min_durations <- map_dbl(
      seq_len(nrow(routing_table$durations)),
      function(x) min(routing_table$durations[x,]))
    return(min_durations)
  }
)

grid_points$time_to_rewe <- unlist(foo)


raster_template <- rast(
  extent = st_bbox(st_buffer(city_shp, dist = buffer)), 
  resolution = 0.0001,
  crs = st_crs(city_shp)$wkt
)
raster_interpolation <- rasterize(grid_points, raster_template, field = "time_to_rewe", 
                        fun = mean)
summary(values(raster_interpolation))
raster_interpolation_masked <- mask(raster_interpolation, vect(city_shp))



## Add street data -------------------------------------------------------------

# Get streets

if (FALSE) {
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
            file.path("data", "highway_features_filtered_cgn.rds"), compress = "gz")
  # Delete highway features object
  rm(highway_features)
} else {
  highway_features_filtered <- read_rds(file.path("data", "highway_features_filtered_cgn.rds"))
}


street_types <- list(
  large = c("motorway", "primary", "motorway_link", "primary_link"),
  medium = c("secondary", "tertiary", "secondary_link", "tertiary_link"),
  small = c("residential", "living_street", "unclassified", "service", "footway")
)

bg_color <- "#888888"

p <- ggplot() +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$small),
          linewidth = 0.05, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$medium),
          linewidth = 0.1, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$large),
          linewidth = 0.25, col = "#434343") +
  geom_contour_filled(
    data = as.data.frame(raster_interpolation_masked, xy = TRUE) |> 
      filter(mean <= 60),
    aes(x = x, y = y, z = mean, fill = after_stat(level)),
    breaks = c(0, 1, 2, 5, 10, 15, 20, 30, Inf),
    alpha = 0.75) +
  geom_sf(data = city_shp, fill = "transparent", color = bg_color, linewidth = 1) +
  # geom_sf(data = rhine_cgn_shp, col = "white", linewidth = 1.5) +
  geom_sf(data = rewe, color = "white", size = 0.35) +
  geom_sf(data = rewe, color = "#121212", size = 0.25) +
  # geom_sf_text(data = rewe, aes(label = name), color = "#121212", size = 1.5) +
  # scale_fill_viridis_d() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  guides(fill = guide_legend()) +
  theme_void() +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color)
  )
ggsave(file.path("plots", "03-polygons.png"), width = 12, height = 10, dpi = 600)
