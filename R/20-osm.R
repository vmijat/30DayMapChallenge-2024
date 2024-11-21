library(tidyverse)
library(osmdata)
library(sf)
library(osrm)
library(terra)
library(ggtext)
library(ggspatial)

city_name <- "Cologne, Germany"

shp <- getbb(city_name, format_out = "sf_polygon")
crs <- st_crs(shp)

hospitals <- opq(city_name, timeout = 1200) |> 
  add_osm_feature(key = "amenity", value = "hospital") |> 
  osmdata_sf()

hospitals_filtered <- st_filter(hospitals$osm_points, shp) |> 
  bind_rows(st_filter(hospitals$osm_polygons, shp)) |> 
  st_make_valid() |> 
  select(osm_id, name, geometry) |> 
  filter(!is.na(name)) |> 
  st_centroid(geometry)

hospitals_filtered <- hospitals_filtered |> 
  filter(!name %in% c("PhysioSport Performance Athletic Center",
                      "Forensische Psychiatrie der LVR-Klinik Köln"))

# Generate a grid of points within the city boundary 
buffer <- 1000
grid_points <- st_make_grid(shp, cellsize = 0.001, what = "centers") %>%
  st_sf() %>%
  st_filter(st_buffer(shp, dist = buffer))

# Calculate walking times to the nearest supermarket for each grid point
path_time_to_next_hospital <- file.path("data", "time-to-next-hospital.rds")

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
        dst = st_geometry(hospitals_filtered),
        osrm.profile = "car"
      ) 
      # calculate the minimum per grid cell
      min_durations <- map_dbl(
        seq_len(nrow(routing_table$durations)),
        function(x) min(routing_table$durations[x,]))
      return(min_durations)
    }
  )
  write_rds(durations, path_time_to_next_hospital)
} else {
  durations <- read_rds(path_time_to_next_hospital)
}

grid_points$duration <- unlist(durations)

raster_template <- rast(
  extent = st_bbox(st_buffer(shp, dist = buffer)), 
  resolution = 0.0001,
  crs = st_crs(shp)$wkt
)
raster_interpolation <- rasterize(grid_points, raster_template, field = "duration", 
                                  fun = mean)
summary(values(raster_interpolation))
raster_interpolation_masked <- mask(raster_interpolation, vect(shp))


## Retrieve building data 

# Get buildings
buildings <- opq(city_name) %>%
  add_osm_feature(key = "building") |> 
  osmdata_sf()
write_rds(buildings, file.path("data", "osm-buildings-cgn.rds"), compress = "gz")

# Filter to city shape
buildings_combined <- bind_rows(buildings$osm_polygons, buildings$osm_multipolygons)
buildings_combined <- buildings_combined |> select(osm_id, name, geometry)
buildings_combined_filtered <- buildings_combined |> 
  st_make_valid() |> 
  st_filter(shp)


isochrone_breaks <- c(0, 3, 5, 10, 15, Inf)
isochrone_labels <- c(
  "Less than 3", "3-5", "5-10", "10-15", "More than 15"
)


raster_interpolation_masked_df <- raster_interpolation_masked |> 
  as.data.frame(xy = TRUE) |> 
  mutate(
    isochrone_cat = cut(mean, breaks = isochrone_breaks, include.lowest = TRUE,
                        right = TRUE)) |> 
  st_as_sf(coords = c("x", "y"), crs = crs(raster_interpolation_masked))
head(raster_interpolation_masked_df)

# Add isochrone information to each building
buildings_with_isochrones <- st_join(buildings_combined_filtered, 
                                     raster_interpolation_masked_df, 
                                     join = st_nearest_feature)


p <- ggplot() +
  ggfx::with_shadow(
    geom_sf(data = shp, fill = "black", color = "black", linewidth = 1),
    colour = "#222", x_offset = 10, y_offset = 8
  ) +
  geom_sf(
    data = buildings_with_isochrones,
    aes(fill = isochrone_cat),
    linewidth = 0.01, color = "white"
  ) +
  geom_sf(
    data = hospitals_filtered, 
    aes(color = "Hospital"),
    fill = "white", shape = 22, size = 1.5) +
  annotation_scale(
    location = "bl", width_hint = 0.15, height = unit(2, "mm"),
    text_family = "Roboto"
  ) +
  scale_color_manual(values = c("black")) +
  scale_fill_manual(
    values =  scales::pal_viridis(
      begin = 0.15, end = 1, option = "D", direction = -1)(length(isochrone_breaks - 1)),
    labels = isochrone_labels
  ) +
  guides(fill = guide_legend(
    title = "Travel time by car (in minutes)", title.position = "top", nrow = 2,
    direction = "horizontal", order = 1)) +
  guides(color = guide_legend(title = NULL, override.aes = list("size" = 3),
                              order = 2)) +
  labs(
    title = "How long does it take to the next hospital in Cologne?",
    subtitle = "Each dot on the map shows the location of a hospital.
    The colour of a building indicates how accessible the hospitals are by car, 
    assuming average conditions.",
    caption = "Source: OpenStreetMap contributors. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Roboto") +
  theme(
    plot.background = element_rect(
      color = "#B6B6B6", fill = "#B6B6B6"),
      legend.position = "inside",
  legend.position.inside = c(0.75, 0.875),
  legend.direction = "horizontal",
  text = element_text(color = "black"),
  plot.title = element_markdown(face = "bold", size = 16),
  plot.title.position = "plot",
  plot.subtitle = element_textbox(width = 1, lineheight = 1.15),
  plot.caption = element_markdown(),
  plot.margin = margin(rep(4, 4))
)
ggsave(file.path("plots", "20-osm.png"), width = 6.5, height = 7.5, dpi = 600)
