library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(purrr)
library(ggtext)

data_path <- file.path("data", "GHSL")

#' Manually download the raster data from 
#' https://human-settlement.emergency.copernicus.eu/download.php?ds=builtH

ghsl_raster_paths <- c(
  file.path("GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19", 
            "GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"),
  file.path("GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R4_C19", 
            "GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif")
)
ghsl_rasters <- map(file.path(data_path, ghsl_raster_paths), rast) 
ghsl_raster_merged <- merge(ghsl_rasters[[1]], ghsl_rasters[[2]])
raster_crs <- crs(ghsl_raster_merged)

st_bbox(ghsl_raster_merged) |> 
  st_as_sfc() |> 
  st_transform(crs = "EPSG:4326")

# Define a bounding box for an area of interest
bbox <- st_bbox(c(xmin = 2.5, xmax = 4.4, ymin = 50.9, ymax = 52.5), 
                crs = "EPSG:4326")
bbox_sf <- st_transform(st_as_sfc(bbox), crs = raster_crs)

# Aspect ratio of bounding box
aspect_ratio <- abs(bbox$xmax - bbox$xmin) / abs(bbox$ymax - bbox$ymin)

ghsl_cropped <- crop(ghsl_raster_merged, bbox_sf)
ghsl_df <- as.data.frame(ghsl_cropped, xy = TRUE, na.rm = TRUE)
colnames(ghsl_df) <- c("x", "y", "height")
str(ghsl_df)

contour_breaks <- c(0, 0.1, 5, 10, 15, 20, 25, 30, 50, Inf)
ghsl_df$height_cat <- cut(ghsl_df$height, breaks = contour_breaks, right = FALSE)
table(ghsl_df$height_cat, useNA = "ifany")
  
p <- ghsl_df |> 
  filter(height > 0) |> 
  ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 3) +
  annotation_north_arrow(
    height = unit(0.5, "cm"), width = unit(0.5, "cm"), location = "bl",
    pad_y = unit(2.5, "cm"), style = north_arrow_fancy_orienteering(
      text_family = "Fira Sans"
    )
  ) +
  geom_tile(
    aes(x, y, fill = height),
    alpha = 0.85) +
  annotate(
    "text",
    x = st_bbox(bbox_sf)$xmin + 2000, y = st_bbox(bbox_sf)$ymax - 26000,
    label = "Built-up height at the Belgian and Dutch coast",
    family = "Fira Sans SemiBold", hjust = 0, size = unit(7, "pt")
  ) +
  annotate(
    "text",
    x = st_bbox(bbox_sf)$xmin + 2000, y = st_bbox(bbox_sf)$ymax - 33000,
    label = "Average of the Net Building Height in a 100 m grid",
    family = "Fira Sans", hjust = 0, size = unit(4.5, "pt")
  ) +
  scale_fill_viridis_c(
    option = "A", na.value = "grey"
    ) +
  labs(
    fill = "Average height (meters)",
    x = "Longitude",
    y = "Latitude",
    caption = "<span style='font-family:\"Fira Sans SemiBold\"'>Source:</span>
    Pesaresi, Martino; Politis, Panagiotis (2023): 
    GHS-BUILT-S R2023A - GHS built-up surface grid, derived from
    Sentinel2 composite and Landsat, multitemporal (1975-2030).
    European Commission, Joint Research Centre. Base map: CARTO.
    <span style='font-family:\"Fira Sans SemiBold\"'>Visualization:</span> Ansgar Wolsing"
  ) +
  coord_sf(crs = raster_crs, expand = FALSE) +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_void(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.35),
    legend.direction = "horizontal",
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(3, "mm"),
    plot.margin = margin(t = 0, l = 0, r = 0, b = 3),
    plot.caption = element_textbox(
      width = 1, lineheight = 1.2, margin = margin(t = 5, l = 4, r = 4, b = 2))
  )
ggsave(file.path("plots", "06-raster.png"), width = 5.6, height = 6, dpi = 600,
       scale = 1.2)
