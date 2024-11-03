library(tidyverse)
library(sf)
library(osmdata)
library(osrm)
library(terra)
library(tidygeocoder)
library(ggtext)

# Get city shape
city_name <- "Cologne, Germany"
city_shp <- getbb(city_name, format_out = "sf_polygon")

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

if (TRUE) {
  chunk_size <- 100
  grid_length <- nrow(grid_points)
  chunk_start <- seq(1, grid_length, chunk_size)
  chunk_end <- chunk_start + chunk_size - 1
  chunk_end[which.max(chunk_end)] <- pmin(chunk_end[which.max(chunk_end)], grid_length)
  
  durations <- map2(
    chunk_start, chunk_end,
    function(x, y) {
      routing_table <- osrmTable(
        src = st_geometry(grid_points[x:y, ]),
        dst = st_geometry(rewe),
        osrm.profile = "foot"
      ) 
      # calculate the minimum per grid cell
      min_durations <- map_dbl(
        seq_len(nrow(routing_table$durations)),
        function(x) min(routing_table$durations[x,]))
      return(min_durations)
    }
  )
  write_rds(durations, path_time_to_rewe)
} else {
  durations <- read_rds(path_time_to_rewe)
}

grid_points$time_to_rewe <- unlist(durations)


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

bg_color <- "#CFCFCF"
isochrone_breaks <- c(0, 2, 5, 10, 15, 30, Inf)
isochrone_labels <- c(
  "Less than 2", "2-5", "5-10", "10-15", "15-30", "More than 30"
)


p <- ggplot() +
  ggfx::with_shadow(
    geom_sf(data = city_shp, fill = bg_color, color = bg_color, linewidth = 1),
    colour = "#727272", x_offset = 12, y_offset = 12
  ) +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$small),
          linewidth = 0.05, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$medium),
          linewidth = 0.1, col = "#434343") +
  geom_sf(data = filter(highway_features_filtered, highway %in% street_types$large),
          linewidth = 0.25, col = "#434343") +
  geom_contour_filled(
    data = as.data.frame(raster_interpolation_masked, xy = TRUE),
    aes(x = x, y = y, z = mean, fill = after_stat(level)),
    breaks = isochrone_breaks, alpha = 0.67) +
  geom_sf(data = city_shp, fill = "transparent", color = bg_color, linewidth = 1) +
  geom_sf(data = rewe, color = "white", fill = "#636363", shape = 21, size = 1) +
  scale_fill_brewer(
    palette = "Reds", direction = -1, labels = isochrone_labels) +
  guides(fill = guide_legend(title = "Walk time (in minutes)", title.position = "top")) +
  labs(
    title = "Why do you call it Cologne when it could also be REWE City?",
    subtitle = "Cologne is the home to 85 REWE supermarkets and the company's
    headquarters. Each dot on the map shows the location of a REWE supermarket.
    The coloured areas indicate how long it takes to get to the nearest REWE 
    supermarket by foot.",
    caption = "Source: REWE, OpenStreetMap contributors, ArcGIS.
    Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.9),
    legend.direction = "horizontal",
    plot.title = element_markdown(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, lineheight = 1.15),
    plot.caption = element_markdown(),
    plot.margin = margin(rep(4, 4))
  )
ggsave(file.path("plots", "03-polygons.png"), width = 6.5, height = 7.5, dpi = 300)
