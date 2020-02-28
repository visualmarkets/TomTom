library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(glue)

rawHtml <- read_html("dropdownHtml.html")

hrefCities <- 
  rawHtml %>% 
    html_nodes(css = "a") %>% 
    html_attr("href") %>% 
    str_sub(1, nchar(.) - 8) %>% 
    str_sub(22, nchar(.))

dropDownData <-
  rawHtml %>% 
  html_nodes(css = "a") %>% 
  html_nodes("span :not(.NavbarSearch__item-country)") %>% 
  html_text()
 

rawData <-
  data.frame(
    b = rep(c("City", "Country")),
    a = dropDownData,
    stringsAsFactors = FALSE
  )


combData <-
  bind_cols(
    rawData %>% filter(b == "City") %>% select(city = a),
    rawData %>% filter(b == "Country") %>% select(country = a)
  ) %>% 
  select(
    city,
    country
  ) %>% 
  as_tibble() %>% 
  mutate(
    country = 
      case_when(country == "United States of America" ~ "USA",
                country == "Spain" ~ "ESP",
                country == "Australia" ~ "AUS",
                country == "Turkey" ~ "TUR",
                country == "United Arab Emirates" ~ "ARE",
                country == "Denmark" ~ "DNK",
                country == "Netherlands" ~ "NLD",
                country == "Belgium" ~ "BEL",
                country == "Greece" ~ "GRC",
                country == "New Zealand" ~ "NZL",
                country == "Germany" ~ "DEU",
                country == "France" ~ "FRA",
                country == "Thailand" ~ "THA",
                country == "Italy" ~ "ITA",
                country == "Switzerland" ~ "CHE",
                country == "China" ~ "CHN",
                country == "United Kingdom" ~ "GBR",
                country == "Brazil" ~ "BRA",
                country == "India" ~ "IND",
                country == "Norway" ~ "NOR",
                country == "Poland" ~ "POL",
                country == "South Africa" ~ "ZAF",
                country == "Colombia" ~ "COL",
                country == "Portugal" ~ "PRT",
                country == "Slovakia" ~ "SVK",
                country == "Czechia" ~ "CZE",
                country == "Romania" ~ "ROU",
                country == "Hungary" ~ "HUN",
                country == "Argentina" ~ "ARG",
                country == "Egypt" ~ "EGY",
                country == "Canada" ~ "CAN",
                country == "Russia" ~ "RUS",
                country == "Ireland" ~ "IRE",
                country == "Ukraine" ~ "UKR",
                country == "Sweden" ~ "SWE",
                country == "Austria" ~ "AUS",
                country == "Finland" ~ "FIN",
                country == "Hong Kong" ~ "HKG",
                country == "Indonesia" ~ "IDN",
                country == "Saudi Arabia" ~ "SAU",
                country == "Taiwan" ~ "TWN",
                country == "Japan" ~ "JPN",
                country == "Malaysia" ~ "MYS",
                country == "Kuwait" ~ "KWT",
                country == "Peru" ~ "PER",
                country == "Slovenia" ~ "SVN",
                country == "Luxembourg" ~ "LUX",
                country == "Philippines" ~ "PHL",
                country == "Mexico" ~ "MEX",
                country == "Iceland" ~ "ISL",
                country == "Latvia" ~ "LVA",
                country == "Chile" ~ "CHL",
                country == "Singapore" ~ "SGP",
                country == "Bulgaria" ~ "BGR",
                country == "Estonia" ~ "EST",
                country == "Israel" ~ "ISR",
                country == "Lithuania" ~ "LTU",
                TRUE ~ country),
    city = tolower(city) %>% str_replace_all(" ", "-") %>% str_replace_all(",", ""),
    href_cities = hrefCities,
    end_point = glue("{country}%2FCircle%2F{href_cities}")
  ) 

saveRDS(combData, "cityData.Rds")
