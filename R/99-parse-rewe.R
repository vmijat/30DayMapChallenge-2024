library(tidyverse)
library(rvest)

#' Go to https://www.rewe.de/marktsuche/koeln/ and show all markets
#' Save the page as HTML and place it in the input folder
page_file_path <- file.path("input", "REWE Supermärkte in Köln - REWE.de.htm")
page <- read_html(page_file_path)

li_div_elements <- page |> 
  html_nodes(css = "li > div:nth-child(2)") 

li_div_elements_text <- html_text(li_div_elements)

df_rewe <- li_div_elements_text |> 
  str_split_fixed(",", 3) |> 
  data.frame() |> 
  mutate(across(everything(), str_squish)) |> 
  rename(name = 1, street = 2, postcode_city = 3) |> 
  filter(street != "") |> 
  mutate(address = paste(street, postcode_city, sep = ", ")) 

write_csv(df_rewe, file.path("data", "rewe-markets-cgn.csv"))
