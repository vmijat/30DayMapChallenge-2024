library(tidyverse)
library(mapgl)

coords_borsigplatz <- c(7.4815508, 51.5234998)
coords_westfalenstadion <- c(7.4503768, 51.4927892)

mapboxgl(
  center = coords_borsigplatz,
  zoom = 18,
  style = "mapbox://styles/mapbox/standard"
) |> 
  fly_to(
    center = coords_westfalenstadion,
    zoom = 17,
    pitch = 50,
    bearing = 136.8,
    duration = 12000,
    minZoom = 11
  ) 
