library(ggplot2)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(giscoR)
library(patchwork)
library(ggtext)

# Load shapes of Ukraine and Russia from both sources
gisco_shp <- giscoR::gisco_get_countries(
  year = "2020", resolution = "1", country = c("Ukraine", "Russian Federation"))

ne_shp <- rnaturalearth::ne_countries(
  scale = 10, country = c("Ukraine", "Russia"))

ggplot(gisco_shp) + geom_sf()
ggplot(ne_shp) + geom_sf()

# Crop to relevant view
bbox <- st_bbox(c(xmin = 19.7, ymin = 42, xmax = 42.5, ymax = 52))

p1 <- gisco_shp |> 
  st_crop(bbox) |> 
  ggplot() + 
  annotation_map_tile(
    type = "osm", zoom = 8, alpha = 1, interpolate = TRUE, cachedir = tempdir()) +
  geom_sf(
    aes(fill = stage(NAME_ENGL, after_scale = alpha(fill, 0.15))),
    linewidth = 0, show.legend = FALSE) +
  annotate(
    "richtext",
    x = 22.5, y = 44.7, label = "<b style='font-size:24pt'>GISCO</b><br>
    {giscoR} package",
    color = "white", fill = "black", family = "Roboto",
    size = 4, hjust = 0, label.r = unit(0, "mm"), label.size = 0, alpha = 0.9
  ) +
  annotate(
    "richtext",
    x = 26.5, y = 47.5,
    label = "**Crimea** annexed by Russia in 2014, <br>**internationally recognized as part<br>
    of Ukraine** by most countries.",
    family = "Roboto", hjust = 0, fill = alpha("white", 0.3), label.size = 0
  ) +
  annotate(
    GeomCurve,
    x = 33, xend = 34, y = 46.9, yend = 45.5, curvature = -0.25, linewidth = 0.2,
    arrow = arrow(angle = 25, length = unit(0.25, "cm"))
  ) +
  scale_fill_manual(values = c("#E4181C", "#FFDD00")) +
  coord_sf(xlim = c(22, 42), ylim = c(44, 50), expand = FALSE) +
  theme_void(base_family = "Roboto")

p2 <- ne_shp |> 
  st_crop(bbox) |> 
  ggplot() + 
  annotation_map_tile(
    type = "osm", zoom = 8, alpha = 1, interpolate = TRUE, cachedir = tempdir()) +
  geom_sf(
    aes(fill = stage(sovereignt, after_scale = alpha(fill, 0.15))),
    linewidth = 0, show.legend = FALSE) +
  annotate(
    "richtext",
    x = 22.5, y = 44.7,label = "<b style='font-size:24pt'>Natural Earth</b><br>
    {rnaturalearth} package",
    color = "white", fill = "black", family = "Roboto", 
    size = 4, hjust = 0, label.r = unit(0, "mm"), label.size = 0, alpha = 0.9
  ) +
  annotate(
    "richtext",
    x = 29, y = 47.3,
    label = "Natural Earth applies an<br>\"on the ground\" control policy",
    family = "Roboto", hjust = 0, fill = alpha("white", 0.3), label.size = 0
  ) +
  annotate(
    GeomCurve,
    x = 33, xend = 34, y = 46.9, yend = 45.5, curvature = -0.25, linewidth = 0.2,
    arrow = arrow(angle = 25, length = unit(0.25, "cm"))
  ) +
  scale_fill_manual(values = c("#E4181C", "#FFDD00")) +
  coord_sf(xlim = c(22, 42), ylim = c(44, 50), expand = FALSE) +
  theme_void(base_family = "Roboto")

p <- p1 / p2 +
  plot_annotation(
    title = "Choosing a geospatial data provider matters",
    subtitle = "How two major geospatial data providers differ in representing Crimea.",
    caption = "GISCO, Natural Earth, Open Street Map contributors. Visualization: Ansgar Wolsing"
  ) &
  theme(
    plot.margin = margin(t = 3, b = 3),
    text = element_text(family = "Roboto"),
    plot.title = element_text(
      face = "bold", size = 18, margin = margin(t = 12, l = 4, r = 4, b = 6)),
    plot.subtitle = element_text(margin = margin(l = 4, r = 4, b = 4)),
    plot.caption = element_text(margin = margin(2, 2, 2, 2))
  )
ggsave(file.path("plots", "21-conflict.png"), width = 7, height = 7)
