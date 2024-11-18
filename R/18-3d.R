library(terra)
library(sf)
library(dplyr)
library(purrr)
library(rayshader)

# Load the raster data and merge
data_path <- file.path("data", "GHSL")
ghsl_raster_paths <- c(
  file.path("GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19", 
            "GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R3_C19.tif"),
  file.path("GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R4_C19", 
            "GHS_BUILT_H_ANBH_E2018_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif")
)
ghsl_rasters <- map(file.path(data_path, ghsl_raster_paths), rast) 
ghsl_raster_merged <- merge(ghsl_rasters[[1]], ghsl_rasters[[2]])

# Define the bounding box and crop the raster
bbox <- st_bbox(c(xmin = 2.5, xmax = 4.4, ymin = 50.9, ymax = 52.0), crs = "EPSG:4326")
bbox_sf <- st_transform(st_as_sfc(bbox), crs = crs(ghsl_raster_merged))
ghsl_cropped <- crop(ghsl_raster_merged, bbox_sf)

# Convert the cropped raster to a matrix suitable for rayshader
height_matrix <- raster_to_matrix(ghsl_cropped)
height_matrix[height_matrix == 0] <- NA


# Generate the 3D map using rayshader
colors <- MetBrewer::met.brewer("Pissaro", direction = -1)

height_matrix %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(
    height_matrix,
    solid = FALSE,
    zscale = 0.4, 
    zoom = 0.5,
    fov = 0.3, 
    theta = 270, 
    phi = 38, 
    # shadowcolor = "green",
    soil = TRUE,
    background = "#e8e5dc",
    windowsize = c(1200, 1200)
  )

render_camera()

render_highquality(
  file.path("plots", "18-3d-high-res.png"), 
  parallel = TRUE, 
  samples = 256,
  light = TRUE,
  interactive = FALSE,
  width = 4000, 
  height = 4000
)

rgl::close3d()
