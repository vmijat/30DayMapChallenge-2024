
library(tigris)
library(leaflet)

# options(tigris_use_cache = TRUE)

cd118 <- congressional_districts(cb = TRUE, resolution = '20m', year = 2022) |> 
  shift_geometry()

leaflet(cd118) %>%
  addTiles() %>%
  addPolygons()



library(sf)
library(tidyverse)
library(gganimate)
library(ggtext)

# Daily Kos Hexmap for the 2022 congressional districts
hexmap_shp <- st_read(file.path("data", "HexCDv30", "HexCDv30.shp"))

st_crs(cd118)
st_crs(hexmap_shp)

# Transform hexmap to the same projection as the congressional district map
hexmap_shp <- st_transform(hexmap_shp, crs = st_crs(cd118))

ggplot() +
  geom_sf(data = cd118)

ggplot() +
  geom_sf(data = hexmap_shp)


# Combine the datasets (basic version)
cd118_basic <- select(cd118, GEOID, geometry)
hexmap_shp_basic <- select(hexmap_shp, GEOID, geometry)
cd118_hexmap_combined <- cd118_basic |> 
  mutate(map_type = "map") |> 
  bind_rows(mutate(hexmap_shp_basic, map_type = "hexmap")) |> 
  mutate(map_type = factor(map_type, levels = c("map", "hexmap"))) |> 
  arrange(map_type, GEOID) |> 
  add_count(GEOID) |> 
  filter(n == 2) |> 
  select(-n)

cd118_hexmap_combined |> 
  ggplot() +
  geom_sf() +
  facet_wrap(vars(map_type))

cd118_hexmap_combined |> 
  st_drop_geometry() |> 
  count(GEOID) |> 
  arrange(n) |> 
  filter(n == 1)

library(gganimate)

cd118_hexmap_combined |> 
  ggplot() +
  geom_sf(aes(fill = str_sub(GEOID, 1, 2))) +
  guides(fill = "none") +
  theme_void() +
  transition_states(map_type)


cd118_hexmap_combined |> 
  filter(str_detect(GEOID, "^01")) |> 
  ggplot() +
  geom_sf(aes(fill = GEOID)) +
  guides(fill = "none") +
  theme_void() +
  transition_states(map_type)


# Election results US Congress
df_congress <- read_csv(file.path("data", "1976-2022-house.csv"))

df_maps_vote_share <- df_congress |> 
  filter(year == 2022) |> 
  mutate(
    GEOID = paste0(str_pad(state_fips, 2, "left", "0"),
                   str_pad(district, 2, "left", "0")),
    vote_share = candidatevotes / totalvotes
    ) |> 
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) |> 
  select(GEOID, state, state_po, state_fips, district, party, vote_share) |> 
  group_by(GEOID) |> 
  slice_max(vote_share, n = 1) |> 
  ungroup() |> 
  inner_join(cd118_hexmap_combined, by = "GEOID") |>
  st_as_sf()


p <- df_maps_vote_share|> 
  ggplot() +
  geom_sf(
    aes(fill = party, alpha = vote_share),
    linewidth = 0.1, color = "#f0efeb") +
  scale_fill_manual(values = c("DEMOCRAT" = "#2054e3", "REPUBLICAN" = "#d91111")) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  guides(
    fill = guide_legend(
      title.position = "top"),
    alpha = guide_legend(
      title.position = "top", nrow = 1, override.aes = list("fill" = "#121212"))) +
  labs(
    title = "Land does note vote, people do",
    subtitle = "Party affiliation of the elected candidated in the 2022 Congressional
    Election (House of Representatives).",
    caption = "Source: Census.gov (via tigris R package), hexagon map: Daily Kos (https://dkel.ec/map).
    Visualization: Ansgar Wolsing",
    fill = "Party\nof the winning candidate",
    alpha = "Vote share\nof the winning candidate"
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#f0efeb", fill = "#f0efeb"),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    legend.position = "bottom"
  ) # +
  # facet_wrap(vars(map_type))
 
p_anim <- p + transition_states(map_type)

animate(p_anim, res = 200, width = 1200, height = 800, units = "px", bg = "#f0efeb")
anim_save(file.path("plots", "04-hexagons.gif"))
