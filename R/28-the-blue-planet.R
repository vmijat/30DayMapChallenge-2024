library(tidyverse)
library(ggtext)
library(treemapify)

#' Land: 29 %
#' Water: 71 %
#'  thereof: 
#'    96.5 % salt water in oceans --> 68.515 % of total
#'     3.5 % freshwater and frozen water locked up in glaciers and the polar ice caps 
#'      ---> 2.485 % of total
#'      thereof:
#'        69 % ice --> 1.71465 % of total
#'        31 % freshwater --> 0.77035 % of total
#'        

df <- tibble(
  category1 = c("land", "water", "water", "water"),
  category2 = c("land", "salt water in oceans",
                "freshwater and frozen water", "freshwater and frozen water"),
  category3 = c("land", "salt water in oceans",
                "frozen water", "freshwater"),
  share_of_surface = c(0.29, 0.68515, 0.0171465, 0.0077035)
)
df

sum(df$share_of_surface)

#' Ideen:
#' Die verschiedenen Oberfläche mit Bildern versehen. ggpattern? 
#' Also Ocean --> Meerwasser-Bild oder Kartenausschnitt, Ice --> Gletscher etc.

p <- df |> 
  ggplot(
    aes(area = share_of_surface, subgroup = category1,
        subgroup2 = category2,
        subgroup3 = category3)
  ) +
  geom_treemap_subgroup_border(size = 1) +
  geom_treemap_subgroup2_border(size = 0.5) +
  geom_treemap_subgroup3_border(size = 0.25) +
  theme(
    plot.margin = margin(rep(0, 4))
  )
ggsave(file.path("plots", "28-the-blue-planet-grid.png"), width = 5, height = 4)
