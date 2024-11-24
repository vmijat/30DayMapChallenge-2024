library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(ggtext)

data_path <- file.path("data", "GHSL")

#' Download the population density data from 
#' https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
#' https://ghsl.jrc.ec.europa.eu/download/GHSL_data_54009_shapefile.zip

ghsl_raster_path <- 
  file.path(data_path,
            "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0", 
            "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0.tif")
ghsl_raster <- rast(ghsl_raster_path)
raster_crs <- crs(ghsl_raster)

# Aggregate the raster dataset
raster_aggregated <- aggregate(ghsl_raster, fact = 200, 
                               fun = sum, na.rm = TRUE)
names(raster_aggregated) <- "pop"

# Transform raster data to sf dataframe
raster_df <- as.data.frame(raster_aggregated, xy = TRUE) |> 
  st_as_sf(coords = c("x", "y"), crs = crs(ghsl_raster))
colnames(raster_df) <- c("pop", "geometry")

# Change the projection and remove cells with 0 inhabitants.
raster_df <- raster_df |> 
  # st_transform(crs = "+proj=laea +lat_0=10 +lon_0=40 +datum=WGS84") |> 
  st_transform(crs = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs") |> 
  filter(pop > 0)

ggplot(raster_df) +
  geom_sf(aes(color = pop), size = 0.001, linewidth = 0) +
  scale_size_area(max_size = 0.5)
summary(raster_aggregated)
summary(raster_df$pop)

graticules <- st_graticule(raster_df)

colors <- c("#ffe5b4", "#008080")

ggplot() + 
  geom_sf(
    data = graticules,
    color = colors[1], linewidth = 0.1, linetype = "solid"
  ) +
  geom_sf(
    data = filter(raster_df, pop <= 10e6),
    aes(size = pop),
    stroke = 0, color = colors[1]
    ) +
  geom_sf(
    data = filter(raster_df, pop > 10e6),
    aes(size = pop),
    stroke = 0.1, shape = 21, color = colors[2], fill = colors[1]
  ) +
  scale_shape_identity() +
  scale_color_identity(aesthetics = c("color", "fill")) +
  scale_size_continuous(
    range = c(0.2, 2.5),
    breaks = c(1e3, 1e6, 5e6, 10e6, 20e6, 40e6, 60e6),
    labels = scales::label_number(scale_cut = scales::cut_long_scale())) +
  guides(
    size = guide_legend(
      position = "bottom", title.position = "top", nrow = 1, 
      override.aes = list("color" = colors[1]))) +
  labs(
    title = "World Population Density",
    caption = "<span style='font-family:\"Cabin Condensed SemiBold\"'>Source:</span>
    GHSL (GHS-POP, 2020), Freire et al. (2016). 
    <span style='font-family:\"Cabin Condensed SemiBold\"'>Visualization:</span>
    Ansgar Wolsing",
    size = "Inhabitants (millions)"
  ) +
  theme_void(base_family = "Cabin Condensed") +
  theme(
    plot.background = element_rect(color = colors[2], fill = colors[2]),
    text = element_text(color = colors[1]),
    plot.title = element_text(
      family = "Cabin Condensed SemiBold", hjust = 0.5, size = 20),
    plot.caption = element_markdown(size = 6, hjust = 0.5),
    legend.title = element_text(size = 8, margin = margin(b = 0)),
    legend.text = element_text(size = 7),
    legend.key.spacing.y = unit(-10, "mm"),
    legend.key.width = unit(0.5, "mm"),
    plot.margin = margin(rep(4, 4))
  ) 
ggsave(file.path("plots", "22-2-colours.png"), width = 5, height = 3)
