library(tidyverse)
library(sf)
library(countrycode)

world <- rnaturalearth::ne_countries()

# Split France Metropolitan and French Guayana
france_metropolitan <- world |> 
  filter(sovereignt == "France") |> 
  st_crop(st_bbox(c(xmin = 1, xmax = 10, ymin = 40, ymax = 50), crs = st_crs(4326)))
french_guayana <- world |> 
  filter(sovereignt == "France") |> 
  st_crop(st_bbox(c(xmin = -60, xmax = -50, ymin = -5, ymax = 5), crs = st_crs(4326))) |> 
  mutate(geounit = "French Guayana")

# Greenland
greenland <- world |> 
  filter(geounit == "Greenland") |> 
  mutate(geometry = geometry + st_sfc(st_point(c(-18, 0)))) 
st_crs(greenland) <- st_crs(world)

# Add the units again
world <- world |> 
  filter(sovereignt != "France", geounit != "Greenland") |> 
  bind_rows(france_metropolitan) |> 
  bind_rows(french_guayana) |> 
  bind_rows(greenland)

# Projection centered around the Pacific
crs <- "+proj=robin +lon_0=150"
world <- world |> 
  # filter(sovereignt == admin) |> 
  st_transform(crs = crs)

ggplot(world) + geom_sf()

# Calculate the bounding boxes
geometries <- world$geometry
bboxes <- map(geometries, st_bbox) |> 
  map_dfr(function(x) {
      df <- matrix(x, ncol = 4) |> as.data.frame()
      colnames(df) <- c("xmin", "ymin", "xmax", "ymax")
      df
    }) 

bboxes <- bboxes |> 
  mutate(
    country_name = world$geounit,
    country_code = countrycode(
      country_name, origin = "country.name", destination = "iso3c"),
    continent = countrycode(
      country_name, origin = "country.name", destination = "continent"),
    continent = case_match(
      country_name,
      "French Guayana" ~ "Americas", 
      "Kosovo" ~ "Europe",
      "Somaliland" ~ "Africa",
      .default = continent),
    xmin = xmin + ifelse(country_name == "Greenland", 5e6, 0),
    xmax = xmax + ifelse(country_name == "Greenland", 5e6, 0)
  ) |> 
  arrange(xmax * ymax)
  

# Which bboxes have the largest x extent?
bboxes |> 
  mutate(x_range = xmax - xmin) |> 
  arrange(-x_range) |> 
  select(country_name, x_range, xmin, xmax) |> 
  head(10)

p <- bboxes |> 
  ggplot() +
  coord_sf(crs = crs) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#F5F5F5", fill = "#F5F5F5")
  )

# Different colors
p + geom_rect(
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
      color = country_name, fill = stage(country_name, after_scale = alpha(fill, 0.1))),
  linewidth = 0.1, show.legend = FALSE)

# Monochrome
p + geom_rect(
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  color = "#222222", fill = "#99999911",
  linewidth = 0.1, show.legend = FALSE)

# Mondrian
set.seed(123)
mondrian_colors <- sample(
  c("#FF1B1C", "#FEFF1C", "#1B1BFF", "#FFFFFF", "black"), 
  size = nrow(bboxes), replace = TRUE)

p + geom_rect(
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      fill = stage(mondrian_colors, after_scale = alpha(fill, 0.5))),
  color = "black",
  linewidth = 0.3, show.legend = FALSE) +
  scale_fill_identity() +
  labs(
    title = toupper("A World Map in the Style of Piet Mondrian"),
    caption = "Source: Natural Earth. Visualization: Ansgar Wolsing" 
  ) +
  theme(
    plot.title = element_text(family = "Cabin Condensed SemiBold", size = 16, hjust = 0.5),
    plot.caption = element_text(family = "Cabin Condensed", size = 6, hjust = 0.5)
  )

ggsave(file.path("plots", "14-a-world-map.png"), width = 6, height = 4)
