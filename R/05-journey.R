library(tidyverse)
library(sf)
library(ggtext)

#' Source: 
#' https://datarepository.movebank.org/handle/10255/move.1660
#' Andrews-Goff V, Gales N, Childerhouse SJ, Laverick SM, Polanowski AM, Double MC. 2023.
#' Data from: Australia’s east coast humpback whales: 
#' satellite tag derived movements on breeding grounds, feeding grounds and 
#' along the northern and southern migration.
#' Movebank Data Repository. https://doi.org/10.5441/001/1.294

df <- read_csv(
  file.path("data", "Movebank", "Movements of Australia's east coast humpback whales.csv"),
  name_repair = janitor::make_clean_names)

table(df$individual_local_identifier)
summary(unclass(table(df$individual_local_identifier)))

df |> 
  filter(individual_local_identifier == 88741) |> 
  ggplot(aes(location_long, location_lat)) +
  geom_path()

world <- giscoR::gisco_coastallines
proj <- "+proj=laea +lon_0=160 +lat_0=-90"
world <- st_transform(world, crs = proj)

whale_path <- df |> 
  filter(individual_local_identifier == 88741) |> 
  filter(is.na(algorithm_marked_outlier)) |> 
  arrange(timestamp) |> 
  select(location_long, location_lat) |> 
  as.matrix() |> 
  st_linestring() |> 
  st_sfc(crs = 4326) |> 
  st_transform(crs = proj)

ggplot() +
  geom_sf(
    data = world,
    fill = "#FFFFFF33", color = "white", linewidth = 0.2) +
  geom_sf(
    data = whale_path,
    size = 0.5, col = "green") +
  coord_sf(crs = proj, xlim = c(-4800000, 2500000), ylim = c(1000000, 7200000)) +
  guides(col = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )
ggsave(file.path("plots", "05-journey-transparent-export.png"), width = 8, height = 8)


df |> 
  filter(is.na(algorithm_marked_outlier)) |> 
  distinct(individual_local_identifier, timestamp, location_long, location_lat) |> 
  st_as_sf(coords = c("location_long", "location_lat"), crs = "EPSG:4326") |> 
  st_transform(crs = proj) |> 
  filter(individual_local_identifier == 88741) |> 
  ggplot() +
  geom_sf(
    data = world,
    fill = "#FFFFFF33", color = "white", linewidth = 0.15) +
  geom_sf(size = 0.1, col = "#06FFE2") +
  coord_sf(crs = proj, xlim = c(-4800000, 2500000), ylim = c(1000000, 7200000)) +
  guides(col = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )
ggsave(file.path("plots", "05-journey-transparent-export-dots.png"), width = 8, height = 8)


df |> 
  filter(individual_local_identifier == 88741) |> 
  summarize(min(timestamp), max(timestamp))
# 1 2008-11-02 12:33:00 2009-04-04 13:46:00

st_length(whale_path)
# 14969741 [m]



## ALL WHALES

whale_paths <- df |> 
  filter(is.na(algorithm_marked_outlier)) |> 
  add_count(individual_local_identifier) |> 
  nest(data = -c(individual_local_identifier, n)) |> 
  # remove the 20 % with fewest events
  filter(n >= quantile(n, probs = 0.2)) |> 
  unnest(data) |> 
  arrange(individual_local_identifier, timestamp) |> 
  distinct(individual_local_identifier, timestamp, location_long, location_lat) |> 
  group_by(individual_local_identifier) |> 
  group_by(individual_local_identifier) |> 
  summarize(
    first_timestamp = min(timestamp),
    last_timestamp = max(timestamp),
    path = st_sfc(
      st_linestring(
        as.matrix(select(cur_data(), location_long, location_lat))
      ),
      crs = 4326
    )
  ) |> 
  ungroup() |>
  st_as_sf() |> 
  mutate(
    path_length = st_length(path),
    duration = last_timestamp - first_timestamp) |> 
  filter(duration >= days(30)) |> 
  st_transform(crs = proj) 


whale_paths |> 
  ggplot() +
  geom_sf(
    data = world,
    fill = "#FFFFFF33", color = "white", linewidth = 0.15) +
  geom_sf(size = 0.1, col = "#06FFE2") +
  coord_sf(crs = proj, xlim = c(-4800000, 2500000), ylim = c(1000000, 7200000)) +
  guides(col = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )
ggsave(file.path("plots", "05-journey-transparent-export-lines-all-whales.png"), width = 8, height = 8)


## Smooth the linestrings / paths

library(smoothr)

whale_paths_smoothed <- whale_paths %>%
  mutate(
    path = map(path, 
               function(geometry) smooth(geometry, method = "chaikin",
                                         refinements = 7)),
    path = st_sfc(path, crs = st_crs(whale_paths)))
st_crs(whale_paths_smoothed)
nrow(whale_paths_smoothed)

whale_paths_smoothed |> 
  ggplot() +
  geom_sf(
    data = world,
    fill = "#FFFFFF33", color = "white", linewidth = 0.15) +
  geom_sf(size = 0.075, col = "#06FFE2") +
  coord_sf(crs = proj, xlim = c(-4800000, 2500000), ylim = c(1000000, 7200000)) +
  guides(col = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )
ggsave(file.path("plots", "05-journey-transparent-export-lines-all-whales.png"), width = 8, height = 8)


# with dots
df |> 
  filter(is.na(algorithm_marked_outlier)) |> 
  arrange(individual_local_identifier, timestamp) |> 
  distinct(individual_local_identifier, timestamp, location_long, location_lat) |> 
  group_by(individual_local_identifier) |> 
  mutate(
    first_timestamp = min(timestamp),
    last_timestamp = max(timestamp),
    duration = last_timestamp - first_timestamp) |> 
  filter(duration >= days(30)) |> 
  ungroup() |>
  st_as_sf(coords = c("location_long", "location_lat"), crs = "EPSG:4326") |> 
  st_transform(crs = proj) |> 
  # filter(individual_local_identifier == 88741) |> 
  ggplot() +
  geom_sf(
    data = world,
    fill = "#FFFFFF33", color = "white", linewidth = 0.15) +
  geom_sf(size = 0.01, col = "#06FFE2") +
  coord_sf(crs = proj, xlim = c(-4800000, 2500000), ylim = c(1000000, 7200000)) +
  guides(col = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )
ggsave(file.path("plots", "05-journey-transparent-export-all-whales-dots.png"), width = 8, height = 8)
