library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(ggspatial)
library(leaflet)


# Define the Overpass query
overpass_url <- "https://overpass-api.de/api/interpreter"
relation_id <- "3348003"
query <- sprintf("[out:json][timeout:25];rel(%s);(._;>>;);out;", relation_id)

# Send the GET request to the Overpass API
response <- GET(url = overpass_url, query = list(data = query))

# Check if the request was successful
if (status_code(response) == 200) {
  osm_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  elements <- osm_data$elements
} else {
  print(paste("Error:", status_code(response)))
}

# Separate nodes and ways from the elements dataframe
nodes <- elements %>% filter(type == "node")
ways <- elements %>% filter(type == "way")

# Function to create a LINESTRING object from the nodes of a way
create_linestring <- function(way_nodes_ids, nodes) {
  # Filter the nodes that belong to this way using the node IDs
  way_nodes <- nodes |> 
    filter(id %in% way_nodes_ids) |> 
    arrange(match(id, way_nodes_ids))
  
  oords <- as.matrix(way_nodes[, c("lon", "lat")])
  
  st_linestring(coords)
}

# Create linestrings for each way and combined them in an sf object
linestrings <- map(ways$nodes, create_linestring, nodes)
linestrings_sfc <- st_sfc(linestrings, crs = 4326)
ways_sf <- st_sf(geometry = linestrings_sfc)


# Identify stops
tram_stops_distinct <- nodes |> 
  filter(tags$railway == "tram_stop") |> 
  group_by(tags$name) |> 
  slice_head(n = 1) |> 
  ungroup()


# Option of basemaps
rosm::osm.types()


# Annotation
annotation_text <- "
<p style='font-size:16pt; font-weight:600; color:#6a3d9a'>The Kusttram</p>
<p>(engl. 'Coast Tram') is a light rail public transport service<br>
connecting the cities and towns along the Belgian coast<br>
between De Panne (near the French border) and Knokke-Heist<br>(Dutch Border).</p>
<p>With 67 km, it's the <span style='font-weight:500'>second-longest light rail 
service worldwide</span>.</p>
"


leaflet(
  options = leafletOptions(minZoom = 5)
) |> 
  addProviderTiles(providers$Stadia.AlidadeSmooth,
                   options = providerTileOptions(opacity = 0.9)) %>%  
  addPolylines(data = st_transform(ways_sf, crs = 4326), color = "#6a3d9a",
               opacity = 1, weight = 2.5) |> 
  addCircleMarkers(
    lng = tram_stops_distinct$lon,
    lat = tram_stops_distinct$lat,
    radius = 3, weight = 1,
    color = "#6a3d9a", fillOpacity = 0.5,
    label = tram_stops_distinct$`tags$name`
  ) |> 
  # Add text annotation without a marker
  addLabelOnlyMarkers(
    lng = 2.65, 
    lat = 51.25,
    label = htmltools::HTML(annotation_text),  
    labelOptions = labelOptions(
      noHide = TRUE, direction = "top", textOnly = TRUE,
      style = list(
        "font-family" = "Roboto Condensed",
        "font-weight" = 300,
        "font-size" = "9pt",
        "text-align" = "left",
        "line-height" = "115%"
      ))
  ) |> 
  setView(lng = 2.8, lat = 51.23, zoom = 10)
