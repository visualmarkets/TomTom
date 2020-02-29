CityList <-
  modules::module({

    # Specify packages
    suppressMessages({
      modules::import(rvest)
      modules::import(stringr)
      modules::import(tidyr)
      modules::import(dplyr)
      modules::import(glue)
      modules::import(httr)
      modules::import(purrr)
    })

    # Api Globals
    tzApiKey <- "W899LW7HELDB"
    tzApiBase <- "http://api.timezonedb.com/v2.1/get-time-zone"

    #-----------------#
    # Start City Pull #
    #-----------------#

    # Raw HTML manually pulled from site
    rawHtml <- xml2::read_html("cache/dropdownHtml.html")

    # Get anchor links to city names
    hrefCities <-
      rawHtml %>%
        html_nodes(css = "a") %>%
        html_attr("href") %>%
        str_sub(1, nchar(.) - 8) %>%
        str_sub(22, nchar(.))

    # Dropdown data, cities and countries
    dropDownHtml <-
      rawHtml %>%
      html_nodes(css = "a") %>%
      html_nodes("span :not(.NavbarSearch__item-country)") %>%
      html_text()

    dropDownData <-
      data.frame(
        measure = rep(c("City", "Country")),
        value = dropDownHtml,
        stringsAsFactors = FALSE
      )

    cityData <-
      bind_cols(
        dropDownData %>% filter(measure == "City") %>% select(city = value),
        dropDownData %>% filter(measure == "Country") %>% select(country = value)
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

    #------------#
    # Time Zones #
    #------------#

    GetTimeZone <-
      function(geoData){
        names(geoData) %>%
        map_df(
          function(x){

            print(glue("Getting timezone for {x}"))

            Sys.sleep(5)

            response <- GET(tzApiBase,
                            query = list(key = tzApiKey,
                                         format = "json",
                                         by = "position",
                                         lat = geoData[[x]]['lat'],
                                         lng = geoData[[x]]['lon']
                            )
            )

            tibble(
              city = x,
              time_zone = content(response)$zoneName
            )

          }
        )
      }



    # Extract TimeZones
    timeZones <- CityList$GetTimeZone(geoData)

    #--------------#
    # Upload to DB #
    #--------------#

    UpdateSqLite <- function(tableName, tableData, sqlDbRead = ":memory:", sqlDbWrite = sqlDbRead, ...){

      suppressMessages({
        modules::import(RSQLite)
        modules::import(tibble)
      })

      channel <- dbConnect(SQLite(), sqlDbRead)

      uploadTable <- as.data.frame(tableData)

      dbWriteTable(channel, tableName, uploadTable, ...)

      sqliteCopyDatabase(channel, sqlDbWrite)

      dbDisconnect(channel)

    }

    latLon <-
      map_df(names(geoData),
        function(x){
          tibble(
            city = x,
            lat = geoData[[x]]['lat'],
            lon = geoData[[x]]['lon']
          )

        }
      )

    #---------#
    # Exports #
    #---------#

    modules::export("cityData")
    modules::export("GetTimeZone")

  })

