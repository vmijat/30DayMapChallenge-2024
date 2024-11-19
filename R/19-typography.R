library(tidyverse)
library(sf)
library(ggtext)
library(giscoR)

europe_shp <- giscoR::gisco_get_countries(year = "2020", region = "Europe", epsg = "3035")
cyprus_shp <- giscoR::gisco_get_countries(year = "2020", country = "Cyprus", epsg = "3035")
europe_shp <- bind_rows(europe_shp, cyprus_shp)

# EU27 country codes
eu_iso3 <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

eu27_shp <- europe_shp |> 
  filter(ISO3_CODE %in% eu_iso3)
unique(eu27_shp$NAME_ENGL)[order(unique(eu27_shp$NAME_ENGL))]
nrow(eu27_shp)

# Crop to mainland
st_bbox(eu27_shp)
crs <- "EPSG:3035"
mainland_bbox <- st_bbox(c(xmin = 1.9e6, xmax = unname(st_bbox(eu27_shp)$xmax),
          ymin = 1.6e6, ymax = unname(st_bbox(eu27_shp)$xmax)), crs = crs)
eu27_shp_cropped <- st_crop(eu27_shp, mainland_bbox)
ggplot(eu27_shp_cropped) + geom_sf()

# Create a grid
grid <- st_make_grid(eu27_shp_cropped, n = c(90, 90)) |> 
  st_centroid() |>
  st_as_sf() |> 
  st_filter(eu27_shp_cropped, .predicate = st_intersects)

# Join grid with country shapes
grid_countries <- grid |> 
  st_join(eu27_shp_cropped, join = st_intersects)

ggplot(grid_countries) + 
  geom_sf(
    aes(color = NAME_ENGL), size = 0.001, show.legend = FALSE)

# Spread the letters that make up the country names across the countries' part 
# of the grid
grid_countries_letters <- grid_countries |> 
  group_split(NAME_ENGL, .keep = TRUE) |> 
  map(function(x) {
    x |> 
    mutate(
      row = row_number(),
      row_rest = (row_number() - 1) %% str_length(NAME_ENGL) + 1,
      foo = NAME_ENGL,
      letter = unlist(str_split(foo, pattern = ""))[row_rest]
    )
  }) |> 
  bind_rows()

# Font colors
set.seed(42)
country_colors <- sample(c("black", "grey70", "grey25", "grey50"), size = 27, replace = TRUE)
names(country_colors) <- unique(grid_countries_letters$NAME_ENGL)


ggplot(grid_countries_letters) + 
  geom_sf_text(
    aes(
      label = toupper(letter),
      color = NAME_ENGL), 
    size = 2, family = "Source Sans Pro SemiBold", show.legend = FALSE) +
  scale_color_manual(values = country_colors) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "#FAFAFA", fill = "#FAFAFA")
  )
ggsave(file.path("plots", "19-typography.png"), width = 6, height = 6)

