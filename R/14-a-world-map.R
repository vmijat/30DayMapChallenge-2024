library(tidyverse)
library(ggtext)
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
crs <- "+proj=eqearth +lon_0=150"
world <- world |> 
  st_transform(crs = crs)

ggplot(world) + geom_sf()

# Calculate the bounding boxes
bboxes <- map(world$geometry, st_bbox) |>
  map_dfr(function(x) {
    coordinates <- matrix(
      c(
        x["xmin"], x["ymin"],
        x["xmin"], x["ymax"],
        x["xmax"], x["ymax"],
        x["xmax"], x["ymin"],
        x["xmin"], x["ymin"]
      ),
      ncol = 2,
      byrow = TRUE
    )
    sf_polygon <- st_polygon(list(coordinates))
    st_as_sf(data.frame(geometry = st_sfc(sf_polygon)))
  })
st_crs(bboxes) <- crs

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
      .default = continent)) 

# Modify the bounding box for Greenland
greenland_bbox <- bboxes %>% filter(country_name == "Greenland")
other_bboxes <- bboxes %>% filter(country_name != "Greenland")

st_bbox(greenland_bbox)

shift_greenland_x <- 18 * 62000
greenland_bbox$geometry <- st_sfc(st_polygon(list(matrix(
  c(
     7117490 + shift_greenland_x, 6927197,
     7117490 + shift_greenland_x, 8316222,
    11023656 + shift_greenland_x, 8316222,
    11023656 + shift_greenland_x, 6927197,
     7117490 + shift_greenland_x, 6927197
  ),
  ncol = 2,
  byrow = TRUE
))),
  crs = crs
)

combined_bboxes <- bind_rows(other_bboxes, greenland_bbox) |> 
  st_as_sf(sf_column_name = "geometry")


# Base plot
p <- combined_bboxes |> 
  ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#F9F9F9", fill = "#F9F9F9")
  )

# Different colors
p + geom_sf(
  aes(color = country_name, fill = stage(country_name, after_scale = alpha(fill, 0.1))),
  linewidth = 0.1, show.legend = FALSE)

# Monochrome
p + geom_sf(
  color = "#222222", fill = "#99999911",
  linewidth = 0.1, show.legend = FALSE)

# Mondrian style
set.seed(8)
mondrian_colors <- sample(
  c("#FF1B1C", "#FEFF1C", "#1B1BFF", "#FFFFFF", "black"), 
  size = nrow(bboxes), replace = TRUE,
  prob = c(0.3, 0.25, 0.25, 0.15, 0.05))

p + geom_sf(
  aes(fill = stage(mondrian_colors, after_scale = alpha(fill, 0.5))),
  color = "black",
  linewidth = 0.3, show.legend = FALSE) +
  scale_fill_identity() +
  labs(
    title = "A WORLD MAP<br>
    <span style='font-size: 10pt'>IN THE STYLE OF PIET MONDRIAN</span>",
    caption = "Source: Natural Earth. Visualization: Ansgar Wolsing" 
  ) +
  theme(
    plot.title = element_markdown(family = "Cabin Condensed SemiBold", size = 16, hjust = 0.5),
    plot.caption = element_text(family = "Cabin Condensed", size = 6, hjust = 0.5)
  )
ggsave(file.path("plots", "14-a-world-map.png"), width = 6, height = 4)
