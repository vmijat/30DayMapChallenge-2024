library(sf)
library(ggplot2)
library(ggspatial)
library(giscoR)
library(magick)
library(ggpattern)

shp <- giscoR::gisco_get_nuts(
  year = "2021", country = "Germany", resolution = "10", nuts_level = "2")
st_crs(shp)

# Set fill colors randomly for the geometries
shp$fill_color <- sample(
  c("#e1d9c7", "#d4c9b1", "#f3efdd", "#f3e3c3", "#e6dec4"), 
  size = nrow(shp), replace = TRUE)

# Move label positions
map_labels <- shp |> 
  select(NUTS_ID, NUTS_NAME, geometry) |> 
  mutate(
    geometry = st_centroid(geometry),
    geometry = st_sfc(
      ifelse(
        map_labels$NUTS_ID %in% c("DEE0", "DEC0"),
        geometry - st_sfc(st_point(c(0, 0.2))),
        geometry
      )),
      geometry = st_sfc(
        ifelse(
          map_labels$NUTS_ID %in% c("DE40"),
          geometry - st_sfc(st_point(c(-0.2, 0.3))),
          geometry
        )
      )
    )
st_crs(map_labels) <- st_crs(shp)

# Create the map
map <- shp |> 
  ggplot() +
  ggfx::with_shadow(
    geom_sf_pattern(
      aes(
        fill = fill_color,
        # pattern_type = as.character(sample(1:30, size = nrow(shp), replace = TRUE))
        pattern_type = NUTS_ID
        ),
      colour = "black", pattern_fill = "black", pattern_aspect_ratio = 0.25,
      size = 0.1, pattern = "magick", show.legend = FALSE),
    colour = "#4a3b2b", x_offset = 3, y_offset = 3
  ) +
  geom_sf_label(
    data = map_labels,
    aes(label = NUTS_NAME),
    angle = 0, hjust = 0.5, vjust = 0.5, size = 1.6,
    family = "Cormorant Garamond", label.size = 0.1
  ) +
  annotation_scale(
    location = "bl", width_hint = 0.2, text_col = "#7b6d58",
    text_family = "Cormorant Garamond") +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("#7b6d58", "#f3efdd"), line_col = "#2d2d2a")) +
  scale_pattern_type_discrete(choices = gridpattern::names_magick) +
  scale_fill_identity() +
  labs(
    title = "Government regions in Germany",
    caption = "Source: GISCO. Visualization: Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Cormorant Garamond") + 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 9, color = "#7b6d58"),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "#d4c9b1", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 20, color = "#4a3b2b", hjust = 0.5, face = "bold", lineheight = 0.8),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 6)
  )
map

# Create a magick object from the map
map_img <- image_graph(width = 1600, height = 1600, res = 300)
map
dev.off()

map_desaturated <- image_modulate(map_img, brightness = 100, saturation = 60, hue = 100)

# Blend the map with the paper texture
texture <- image_read(file.path("input", "DALL E old paper.png"))
texture <- image_resize(texture, geometry = image_info(map_desaturated)[c("width", "height")])
map_texture <- image_composite(map_desaturated, texture, operator = "blend", compose_args = "25x75")

image_write(map_texture, file.path("plots", "07-vintage.png"))
