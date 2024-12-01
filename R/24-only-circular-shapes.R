library(tidyverse)
library(sf)
library(cartogram)
library(ggtext)

world <- giscoR::gisco_get_countries(resolution = "10")
taiwan <- rnaturalearth::ne_countries(scale = 10, country = "Taiwan") |> 
  select(CNTR_ID = fips_10, ISO3_code = sov_a3, NAME_ENGL = sovereignt, 
         geometry) |> 
  st_transform(crs = st_crs(world))

world <- world |> 
  select(CNTR_ID, ISO3_CODE, NAME_ENGL, geometry) |> 
  bind_rows(taiwan) |> 
  mutate(area = as.numeric(st_area(geometry))) |> 
  st_transform(crs = "+proj=eqearth")

world_dorling <- cartogram_dorling(world, weight = "area", k = 3)

bg_gradient_fill <- grid::linearGradient(c("#12142b", "#282c69"))
bubble_gradient_fill <- grid::radialGradient(c("#282c6911", "#282c6966"))

p_base <- ggplot(world_dorling) + 
  labs(
    title = "A World of Bubbles",
    caption = "**Note:** Each bubble represents a country. The size of the bubble 
    is proportional to the area.
    **Source:** GISCO, Eurostat. **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = bg_gradient_fill, fill = bg_gradient_fill),
    text = element_text(color = "white"),
    plot.title = element_text(
      hjust = 0.5, family = "Source Serif Pro SemiBold", size = 16),
    plot.caption = element_markdown(size = 6, hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )

p_base + 
  geom_sf(
    fill = bubble_gradient_fill, color = "grey79"
  )
ggsave(file.path("plots", "24-only-circular-shapes.png"), width = 6, height = 4.06)


bubble_gradient_fill <- grid::radialGradient(c("#1F214F", "#202453"))
p <- p_base + 
  ggfx::with_shadow(
    geom_sf(
      fill = bubble_gradient_fill, color = "grey79"
    ),
    colour = "white", x_offset = 2.5, y_offset = 2.5
  )
ggsave(file.path("plots", "24-only-circular-shapes-shadows.png"), width = 6, height = 4.06)
