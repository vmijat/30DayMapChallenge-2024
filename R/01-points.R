library(tidyverse)
library(here)
library(ggtext)
library(rvest)
library(tidygeocoder)
library(sf)
library(patchwork)
library(gt)
library(cartogram)
library(ggiraph)


# Scrape the all-time table from Wikipedia
wiki_url <- "https://de.wikipedia.org/wiki/Ewige_Tabelle_der_Fu%C3%9Fball-Bundesliga"
page <- read_html(wiki_url)
table <- page |> 
  html_node("table.wikitable.sortable")
df_raw <- html_table(table)

# Prepare the data
df <- df_raw |>
  janitor::clean_names() |> 
  rename(tore = t, gegentore = t_2) |> 
  filter(str_detect(pl, "^\\d+\\.$")) |> 
  mutate(
    aktuell_bl = str_detect(jahre, "\\d+\\+"),
    jahre = str_remove(jahre, "\\+"),
    diff = str_remove(diff, "\\+"),
    across(c(pl, sp, jahre, s, u, n, tore, gegentore, diff, punkte, bl_titel, auf, ab), as.integer),
    verein = str_remove(verein, "\\[.+?\\]")
    ) 


# Locations of the clubs
clubs <- c("FC Bayern München", "Borussia Dortmund", "Werder Bremen", 
             "VfB Stuttgart", "Borussia Mönchengladbach", "Hamburger SV", 
             "FC Schalke 04", "Eintracht Frankfurt", "1. FC Köln", "Bayer 04 Leverkusen", 
             "1. FC Kaiserslautern", "Hertha BSC", "VfL Bochum", "1. FC Nürnberg", 
             "VfL Wolfsburg", "Hannover 96", "MSV Duisburg", "Fortuna Düsseldorf", 
             "SC Freiburg", "Karlsruher SC", "Eintracht Braunschweig", "TSV 1860 München", 
             "1. FSV Mainz 05", "TSG 1899 Hoffenheim", "Arminia Bielefeld", 
             "KFC Uerdingen 05", "RB Leipzig", "FC Augsburg", "Hansa Rostock", 
             "SV Waldhof Mannheim", "Kickers Offenbach", "Rot-Weiss Essen", 
             "FC St. Pauli", "1. FC Union Berlin", "Energie Cottbus", "Alemannia Aachen", 
             "SG Wattenscheid 09", "1. FC Saarbrücken", "Dynamo Dresden", 
             "Rot-Weiß Oberhausen", "SV Darmstadt 98", "Wuppertaler SV", 
             "Borussia Neunkirchen", "FC 08 Homburg", "SpVgg Unterhaching", 
             "Stuttgarter Kickers", "FC Ingolstadt 04", "1. FC Heidenheim", 
             "SC Paderborn 07", "Tennis Borussia Berlin", "SpVgg Greuther Fürth", 
             "SSV Ulm 1846", "SC Fortuna Köln", "Preußen Münster", "Blau-Weiß 90 Berlin", 
             "VfB Leipzig", "SC Tasmania 1900 Berlin", "Holstein Kiel")

cities <- c("Munich", "Dortmund", "Bremen", 
            "Stuttgart", "Mönchengladbach", "Hamburg", 
            "Gelsenkirchen", "Frankfurt", "Cologne", "Leverkusen", 
            "Kaiserslautern", "Berlin", "Bochum", "Nuremberg", 
            "Wolfsburg", "Hanover", "Duisburg", "Düsseldorf", 
            "Freiburg", "Karlsruhe", "Braunschweig", "Munich", 
            "Mainz", "Sinsheim", "Bielefeld", 
            "Krefeld", "Leipzig", "Augsburg", "Rostock", 
            "Mannheim", "Offenbach", "Essen", 
            "Hamburg", "Berlin", "Cottbus", "Aachen", 
            "Bochum", "Saarbrücken", "Dresden", 
            "Oberhausen", "Darmstadt", "Wuppertal", 
            "Neunkirchen", "Homburg", "Unterhaching", 
            "Stuttgart", "Ingolstadt", "Heidenheim", 
            "Paderborn", "Berlin", "Fürth", 
            "Ulm", "Cologne", "Münster", "Berlin", 
            "Leipzig", "Berlin", "Kiel")

df_clubs_cities <- data.frame(verein = clubs, city = cities)


df_geocoded <- df |> 
  inner_join(df_clubs_cities, by = "verein") |> 
  geocode(city, method = "arcgis") |> 
  arrange(-punkte) |> 
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |> 
  st_transform(crs = "EPSG:3857")

df_geocoded |> 
  ggplot() +
  geom_sf(aes(size = punkte),
          shape = 21, color = "white", fill = "#121212") +
  scale_size_area() +
  coord_sf()

# Shape of Germany
shp_de <- giscoR::gisco_get_countries(country = "Germany") |> 
  st_transform(crs = "EPSG:3857")

# Get the club logos from the HTML content
imgs <- table |> 
  html_nodes("img.mw-file-element") |> 
  html_attr("src")

# Distribute the dots so that they don't overlap on the map
df_geocoded_dorling <- cartogram_dorling(df_geocoded, weight = "punkte", k = 1)

# Raw plot with club labels
df_geocoded_dorling |> 
  arrange(-punkte) |> 
  ggplot() +
  geom_sf(data = shp_de) +
  geom_sf(
    color = "green", fill = "#121212", linewidth = 0.2) +
  geom_sf_label(aes(label = verein), size = 2) +
  coord_sf() +
  theme_void()


p <- df_geocoded_dorling |> 
  arrange(-punkte) |> 
  bind_cols(img = imgs) |> 
  mutate(img = paste0("https:", img)) |> 
  mutate(tooltip_text = sprintf(
    "<img src='%s' height=25><br><b>%s</b> (#%d)<br>%d points<br>%d season%s", 
      img, verein, pl, punkte, jahre,
      # "season" if only a single season, "seasons" if more than one
      ifelse(jahre > 1, "s", "")
    )) |> 
  ggplot() +
  geom_sf(data = shp_de) +
  geom_sf_interactive(
    aes(tooltip = tooltip_text),
    color = "green", fill = "#121212", linewidth = 0.15) +
  geom_sf_text(
    data = ~subset(., pl <= 10),
    aes(label = pl),
    family = "Roboto Condensed", fontface = "bold", size = 2, color = "white"
  ) +
  coord_sf(expand = FALSE) +
  theme_void()
p 


plot_title = "
<b style='font-size: 12pt'>A Map of All Clubs In Bundesliga History</b>
<br>
<br>
59 clubs have played in the Bundesliga until the 2024-25 season.<br>
Hover over the dots for more information.
<br>
<br>
These are the top 10 clubs:
"

p_title <- ggplot() +
  annotate(
    "richtext",
    x = 0, y = 1,
    label = plot_title,
    family = "Roboto Condensed", size = 2.5,
    label.size = 0, label.color = NA, fill = NA,
    hjust = 0, vjust = 1
  ) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 1.5), expand = FALSE) +
  theme_void()


# Top 10 clubs table

top10_clubs <- df |> 
  filter(pl <= 10) |> 
  select(pl, verein, punkte)

top10_gt <- top10_clubs |>
  rename(Rank = pl, Club = verein, `Total Points` = punkte) |>
  gt() |>
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_body(columns = Rank)
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(Rank, `Total Points`))
  ) |> 
  tab_options(
    table.font.size = "7px",
    table.font.names = "Roboto Condensed",
    data_row.padding = 4)

# Layout of the chart elements
patchwork_design <- "
1#
12
12
13
13
13
"
p_combined <- p + p_title + top10_gt +
  plot_layout(design = patchwork_design) +
  theme(plot.margin = margin(l = 20))
p_combined

tooltip_css <- "font-family: 'Roboto Condensed';
font-size: 11pt;
background-color: #787878;
color: white;  
padding: 3px;
border-radius: 2px;
lineheight: 50%"

girafe(ggobj = p_combined, 
       options = list(
         opts_tooltip(css = tooltip_css, opacity = 1),
         opts_sizing(width = 0.8)))
