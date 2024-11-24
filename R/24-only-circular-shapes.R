library(tidyverse)
library(sf)
library(cartogram)
library(ggtext)

world <- giscoR::gisco_get_countries()

world <- world |> 
  mutate(area = as.numeric(st_area(geometry))) |> 
  filter(NAME_ENGL != "Antarctica") |> 
  st_transform(crs = "+proj=eqearth")

world_dorling <- cartogram_dorling(world, weight = "area", k = 3)

gradient_fill <- grid::linearGradient(c("#12142b", "#282c69"))

ggplot(world_dorling) + 
  geom_sf(
    fill = NA, color = "grey79"
  ) +
  labs(
    title = "The Countries of the World",
    caption = "**Note:** Each bubble represents a country. The size of the bubble 
    is proportional to the area.
    **Source:** GISCO, Eurostat. **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
    text = element_text(color = "white"),
    plot.title = element_text(
      hjust = 0.5, family = "Source Serif Pro SemiBold", size = 16),
    plot.caption = element_markdown(size = 6, hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )
ggsave(file.path("plots", "24-only-circular-shapes.png"), width = 6, height = 3.43)
