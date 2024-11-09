library(sf)
library(ggplot2)
library(ggtext)
library(giscoR)

# Load the map data for the contiguous United States
us <- ne_states(country = "United States of America", returnclass = "sf") 
# us <- giscoR::gisco_get_countries(
#   country = "United States of America", year = "2020", resolution = "60") 
contiguous_us <- us[!us$name %in% c("Hawaii", "Alaska"), ]
contiguous_us <- st_transform(contiguous_us, crs = "EPSG:4269")
crs <- st_crs(contiguous_us)

# Generate sample points within the shape
set.seed(42)
points <- st_sample(contiguous_us, size = 1000, type = "random")

points <- st_make_grid(contiguous_us, n = c(33, 33)) |> 
  st_centroid() |>
  st_as_sf() |> 
  st_filter(contiguous_us, .predicate = st_intersects)

# Convert points to a data frame
points_df <- as.data.frame(st_coordinates(points))
points_df$text <- sample(c("U", "S", "A"), nrow(points_df), replace = TRUE)


points_df |> 
  ggplot() +
  geom_text(
    aes(x = X, y = Y, label = text, color = text),
    size = 2, family = "Fira Sans SemiBold", show.legend = FALSE) + 
  scale_color_manual(values = c("red", "blue", "white")) +
  coord_sf(crs = crs) +
  theme_void(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = "grey40", fill = "grey40")
  )
ggsave(file.path("plots", "19-typography.png"), width = 5, height = 4)

