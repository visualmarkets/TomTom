#-----------#
# Api Setup #
#-----------#

# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(glue)
library(magrittr)
library(assertthat)

# Load modules
modules::expose("classes/GenCityList.R")

# Gather city href urls
refTable <- CityList$cityData

# Global settings
apiKey <- "3wmhSmm0ZzlhnOSnPKRYYXqivuljEp0y"

# API Endpoints
baseApi <- glue("https://api.midway.tomtom.com/ranking/live/{refTable[['country']]}%2FCircle%2F{refTable[['city']]}")

#-----------------#
# Gather Api Data #
#-----------------#

# Get raw traffic and city geo data
rawTrafficData <-
  pmap(
    list(
      url = baseApi,
      city = refTable[['city']]
    ),
    safely({
      function(url, city){

        print(glue("Collecting data for {city}"))

        # Raw data from API
        response <- GET(url)

        # Assertions
        assert_that(
          status_code(response) == 200,
          msg = "Issue with GET request"
        )

        # Response header
        tradfficHeader <- content(response)[[1]]
        # Response body
        trafficList <- content(response)[[2]]

        geoData <- c(lat = tradfficHeader[['shape']][['centerLat']],
                     lon = tradfficHeader[['shape']][['centerLon']])

        # Manipulate Data
        trafficData <-
          trafficList %>%
            map(
              safely({
                function(x){
                  unlist(x)
                }
              })
            ) %>%
            map(~{.x$result}) %>%
            reduce(function(x, y){bind_rows(x, y)}) %>%
            mutate(
              UpdateTime = as.POSIXct(UpdateTime / 1000, origin = "1970-01-01", timezone = "America/Boston"),
              city = city
            ) %>%
            select(
              city                   = city,
              date_time              = UpdateTime,
              traffic_index_live     = TrafficIndexLive,
              traffic_index_historic = TrafficIndexHistoric,
              jams_count             = JamsCount,
              jams_length            = JamsLength,
              jams_delay             = JamsDelay
            )

        list(
          geoData = geoData,
          trafficData = trafficData
        )

      }
    })
  ) %>%
  set_names(refTable[["city"]])

# Save list of api errors
trafficErrors <-
  rawTrafficData %>%
  map(~{.x$error}) %>%
  compact()

# Extract geographic data
geoData <-
  rawTrafficData %>%
  map(function(x){x$result$geoData}) %>%
  compact()

# Get traffic congestion data
trafficData <-
  rawTrafficData %>%
  map(function(x){x$result$trafficData}) %>%
  compact() %>%
  reduce(function(x, y){bind_rows(x, y)})

