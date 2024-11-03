library(tidyverse)
library(ggtext)
library(sf)
library(tigris)
library(gganimate)

#' Daily Kos Hexmap for the 2022 congressional districts
#' Download manually via https://docs.google.com/spreadsheets/d/13XkF59JKzvw4SeSq5mbgIFrJfYjK4amg9JoQE5e9grQ/edit?gid=1250379179#gid=1250379179
hexmap_shp <- st_read(file.path("data", "HexCDv30wm", "HexCDv30wm.shp"))
ggplot(hexmap_shp) + geom_sf()

# Congressional districts shapefile
cd118 <- congressional_districts(cb = TRUE, resolution = '20m', year = 2022) 
st_crs(cd118)

# Shift Alaska and Hawaii
cd118_shifted <- cd118 |> 
  shift_geometry()

st_crs(cd118_shifted)
st_crs(hexmap_shp)

# Transform congressional district map to the same projection as the hexmap
# hexmap_shp <- st_transform(hexmap_shp, crs = st_crs(cd118))
# cd118 <- st_transform(cd118, crs = st_crs(hexmap_shp))
cd118_shifted <- st_transform(cd118_shifted, crs = st_crs(hexmap_shp))
ggplot(hexmap_shp) + geom_sf()
ggplot(cd118_shifted) + geom_sf()



# Combine the datasets (basic version)
cd118_shifted_basic <- select(cd118_shifted, GEOID, geometry)
hexmap_shp_basic <- select(hexmap_shp, GEOID, geometry)
cd118_shifted_hexmap_combined <- cd118_shifted_basic |> 
  mutate(map_type = "map") |> 
  bind_rows(mutate(hexmap_shp_basic, map_type = "hexmap")) |> 
  mutate(map_type = factor(map_type, levels = c("map", "hexmap"))) |> 
  arrange(map_type, GEOID) |> 
  add_count(GEOID) |> 
  filter(n == 2) |> 
  select(-n)

# Transform the projection to Albers Equal Area Conic
cd118_shifted_hexmap_combined <- cd118_shifted_hexmap_combined |> 
  st_transform(crs = "EPSG:5070")

cd118_shifted_hexmap_combined |> 
  ggplot() +
  geom_sf() +
  facet_wrap(vars(map_type))

# Check if there are any districts missing
cd118_shifted_hexmap_combined |> 
  st_drop_geometry() |> 
  count(GEOID) |> 
  arrange(n) |> 
  filter(n == 1)

cd118_shifted_hexmap_combined |> 
  ggplot() +
  geom_sf(aes(fill = str_sub(GEOID, 1, 2))) +
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
  mutate(party = str_to_title(party)) |> 
  select(GEOID, state, state_po, state_fips, district, party, vote_share) |> 
  group_by(GEOID) |> 
  slice_max(vote_share, n = 1) |> 
  ungroup() |> 
  inner_join(cd118_shifted_hexmap_combined, by = "GEOID") |>
  st_as_sf()
st_crs(df_maps_vote_share)

p <- df_maps_vote_share|> 
  ggplot() +
  geom_sf(
    aes(fill = party, alpha = vote_share),
    linewidth = 0.1, color = "#f0efeb") +
  scale_fill_manual(values = c("Democrat" = "#2054e3", "Republican" = "#d91111")) +
  scale_alpha_continuous(
    range = c(0.2, 1), labels = scales::label_percent()) +
  guides(
    fill = guide_legend(
      title.position = "top", override.aes = list(size = 1)),
    alpha = guide_legend(
      title.position = "top", nrow = 1, 
      override.aes = list("fill" = "#121212", size = 1))) +
  labs(
    title = "Land does note vote",
    subtitle = "Party affiliation of the elected candidated in the 2022 Congressional
    Election (House of Representatives). The first map shows the congressional 
    districts in their natural shape. The second one shows them equally sized.",
    caption = "Source: Census.gov (via tigris R package), hexagon map: Daily Kos (https://dkel.ec/map).
Visualization: Ansgar Wolsing",
    fill = "Party\nof the winning candidate",
    alpha = "Vote share\nof the winning candidate"
  ) +
  theme_void(base_family = "Roboto Condensed", base_size = 7) +
  theme(
    plot.background = element_rect(color = "#f0efeb", fill = "#f0efeb"),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_textbox(hjust = 0.5, width = 0.8),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.key.size = unit(2, "mm")
  )
p 

p_anim <- p + transition_states(map_type)

animate(p_anim, res = 200, width = 800, height = 800, units = "px", bg = "#f0efeb")
anim_save(file.path("plots", "04-hexagons.gif"))
