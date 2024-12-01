library(magick)
library(purrr)

input_path <- "plots"
output_path <- file.path("docs", "gallery", "maps")
if(!dir.exists(output_path)) dir.create(output_path)

# Read the image files
filenames <- list.files(input_path, pattern = "\\.png")
imgs <- map(file.path(input_path, filenames), image_read, density = 300)

resize <- function(img, target_width = 1000) {
  dim <- image_info(img)[, c("width", "height")]
  dim / 2
  
  dim$height <- dim$height * target_width / dim$width
  dim$width <- target_width
  
  resize_str <- paste(dim, collapse = "x")
  image_resize(img, geometry = resize_str)
}

imgs_resized <- map(imgs, resize)

output_filenames <- gsub("\\.png", "-thumbnail.png", filenames)
walk2(imgs_resized, file.path(output_path, output_filenames), 
      image_write,
      density = 300)
