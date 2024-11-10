################################################################################
#                    PREPARATION FOR THE PEN & PAPER MAP                       #
################################################################################

library(tidyverse)
library(sf)
library(osmdata)

#' Source: https://www.offenedaten-koeln.de/dataset/vornamen-2023
data_url <- "https://www.offenedaten-koeln.de/node/43214/download"
df <- read_csv2(data_url)

# First name position 1 only

firstnames <- df |> 
  arrange(-anzahl) |> 
  filter(position == 1) 
quantile(firstnames$anzahl, 
         probs = c(0, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95,  0.97, 0.99, 1))

firstnames <- firstnames |> 
  mutate(category = case_when(
    anzahl >= 60 ~ 1,
    anzahl >= 50 ~ 2,
    anzahl >= 40 ~ 3,
    anzahl >= 30 ~ 4
  ))
count(firstnames, category)

firstnames |> 
  filter(!is.na(category)) |> View()


# Shape of Cologne
cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
ggplot(cgn) + geom_sf()         

# Shape of the Rhine in Cologne
waterways <- opq(bbox = st_bbox(cgn)) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
rhine <- waterways$osm_multilines |> 
  filter(name == "Rhein") |> 
  st_crop(cgn)

# Cologne with Rhine
ggplot() + 
  geom_sf(data = cgn) +
  geom_sf(data = rhine, color = "blue", linewidth  = 1)
