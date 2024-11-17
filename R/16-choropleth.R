library(tidyverse)
library(sf)
library(readxl)
library(giscoR)

#' Source: Statistische Ämter des Bundes und der Länder
#' Manually download Excel file "Einkommensverteilung (Kreise)" from 
#' https://www.statistikportal.de/de/vgrdl/ergebnisse-kreisebene/einkommen-kreise

file <- file.path("data", "vgrdl_r2b3_bs2023_1.xlsx")
excel_sheets(file)

view_sheet <- function(sheet, excel_file = file) {
  readxl::read_xlsx(excel_file, sheet = sheet) |> View(sheet)
}

view_sheet("Inhalt")
view_sheet("Impressum")
view_sheet("Definitionen")
view_sheet("1.4")
view_sheet("2.4")

df_all <- readxl::read_xlsx(file, sheet = "2.4", skip = 4, 
                        .name_repair = janitor::make_clean_names) |> 
  filter(!is.na(lfd_nr))

df <- df_all |> 
  filter(!is.na(nuts_3)) |> 
  select(NUTS_ID = eu_code, id = regional_schlussel, name = gebietseinheit, disposable_income = x2022)
nrow(df)

# Load shapes
shp <- giscoR::gisco_get_nuts(year = "2021", nuts_level = "3", country = "Germany",
                              resolution = "03")
nrow(shp)
ggplot(shp) + geom_sf()

# Check non-matching entities between the data sources
shp |> 
  st_drop_geometry() |> 
  anti_join(df, by = join_by(NUTS_ID)) |> 
  select(NUTS_ID, NAME_LATN)
df |> 
  anti_join(shp, by = join_by(NUTS_ID)) |> 
  select(NUTS_ID, name)

df <- df |> 
  mutate(NUTS_ID = case_match(
    NUTS_ID,
    "DEG0R" ~ "DEG0P",
    "DE3" ~ "DE300",
    "DEG0S" ~ "DEG04",
    "DE6" ~ "DE600",
    "DEG0Q" ~ "DEG0B",
    "DEG0T" ~ "DEG0F",
    "DEG0V" ~ "DEG0H",
    "DEG0U" ~ "DEG0I",
    .default = NUTS_ID
  ))

# check again
shp |> 
  st_drop_geometry() |> 
  anti_join(df, by = join_by(NUTS_ID)) |> 
  select(NUTS_ID, NAME_LATN)

#' Eisenach (DEG0N)
#' Die frühere kreisfreie Stadt Eisenach gehört seit 1. Juli 2021 zum Wartburgkreis. 
#' Seit 1. Januar 2022  ist Eisenach nun kreisangehörige Kommune (...).
#' Source: https://www.eisenach.de/rathaus/fusion-der-stadt-eisenach/

# Merge Eisenach (DEG0N) and Wartburgkreis (DEG0P)
wartburgkreis_nuts_ids <- c("DEG0P", "DEG0N")
wartburgkreis_geometry <- shp |> 
  filter(NUTS_ID %in% wartburgkreis_nuts_ids) |> 
  st_union()

wartburgkreis_shp <- shp |> 
  filter(NUTS_ID == "DEG0P") |> 
  mutate(
    geometry = st_geometry(wartburgkreis_geometry)
  )

# Fixed Wartburgkreis/Eisenach and match with dataframe
combined_shp <- shp |> 
  filter(!NUTS_ID %in% wartburgkreis_nuts_ids) |> 
  bind_rows(wartburgkreis_shp) |> 
  inner_join(df, by = join_by(NUTS_ID)) |> 
  mutate(
    disposable_income_deviation_abs = disposable_income - disposable_income_de,
    disposable_income_deviation_rel = disposable_income_deviation_abs / disposable_income_de) 
nrow(combined_shp)

# Available income in Germany overall
disposable_income_de <- df_all |> 
  filter(eu_code == "DE") |> 
  pull(x2022)

# Aggregate geometries of the federal states for some separation on the map
federal_states_shp <- shp |> 
  mutate(state_id = str_sub(NUTS_ID, 1, 3)) |> 
  group_by(state_id) |> 
  summarize(geometry = st_union(geometry)) |> 
  st_make_valid()

combined_shp |> 
  st_drop_geometry() |> 
  mutate(state_id = str_sub(NUTS_ID, 1, 3)) |> 
  select(state_id, NUTS_ID, NUTS_NAME, starts_with("disposable_")) |> 
  group_by(state_id) |> 
  summarize(
    above = sum(disposable_income_deviation_rel > 0),
    below = sum(disposable_income_deviation_rel < 0),
    total = n()) |> 
  arrange(above)

combined_shp |> 
  ggplot() +
  geom_sf(
    aes(fill = disposable_income_deviation_rel),
    color = "#222", linewidth = 0.1) +
  geom_sf(
    data = federal_states_shp,
    color = "#222", fill = "transparent", linewidth = 0.3
  ) +
  colorspace::scale_fill_continuous_diverging(
    palette = "Purple-Green", 
    breaks = seq(-1, 1, 0.1),
    labels = scales::label_percent()) +
  guides(fill = guide_colorbar(
    title = "Deviation from national average (in %)",
    title.position = "top")) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(3, "mm")
  )
ggsave(file.path("plots", "16-choropleth-raw.png"), width = 5, height = 5,
       scale = 1.2)
