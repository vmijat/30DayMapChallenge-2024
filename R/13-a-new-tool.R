library(tidyverse)
library(mapgl)

# Set coordinates of the Anatomiegebouw in Utrecht
coords_anatomiegebouw <- c(5.134275154641698, 52.09997314327548)
coords_cgn_hbf <- c(6.95905385523892, 50.9428264084386)

mapboxgl(
  center = coords_cgn_hbf,
  zoom = 14.2,
  style = "mapbox://styles/mapbox/standard"
  ) |> 
  # Fly to the center of Utrecht
  fly_to(
    center = coords_anatomiegebouw,
    zoom = 15.5,
    pitch = 70,
    bearing = 136.8,
    duration = 12000,
    minZoom = 6.8 # i.e. highest "altitude" of the flight animation
  ) %>%
  # Add a globe at the top right
  add_globe_minimap(
    position = "top-right",
    globe_size = 70,
    marker_color = "darkblue"
  ) |> 
  # Add a marker for the Anatomiegebouw
  add_markers(
    coords_anatomiegebouw,
    color = "darkblue",
    popup = "Anatomiegebouw",
    draggable = FALSE,
    marker_id = "marker_1"
  ) 
