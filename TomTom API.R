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

# Global settings
apiKey <- "3wmhSmm0ZzlhnOSnPKRYYXqivuljEp0y"

# Reference table
refTable <-
  tibble(
    country = c("CHN", "CHN", "HKG", "ITA", "CHN", "CHN", "SGP"),
    city = c("beijing", "shanghai", "hong-kong", "milan", "wuhan", "shenzhen", "singapore")
  )

refTable <- 
  readRDS("cityData.Rds") %>% 
  select(
    country = country, 
    city = href_cities
  )

# API Endpoints
baseApi <- glue("https://api.midway.tomtom.com/ranking/live/{refTable[['country']]}%2FCircle%2F{refTable[['city']]}")

#-----------------#
# Gather Api Data #
#-----------------#

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
        
        # Manipulate Data
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
      }      
    })
  ) %>% 
  set_names(refTable[["city"]])


trafficErrors <- 
  rawTrafficData %>% 
  map(~{.x$error}) %>% 
  compact()
  

trafficData <- 
  map(function(x){x$result}) %>% 
  reduce(function(x, y){bind_rows(x, y)})

#--------------#  
# Upload to DB #
#--------------#


