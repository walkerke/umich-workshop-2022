library(sf)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

from <- get_acs(geography = "tract", variables = "B01002_001", year = 2015,
                  state = "AZ", county = "Maricopa", geometry = TRUE) 

to <- get_acs(geography = "tract", variables = "B01002_001", year = 2020,
                  state = "AZ", county = "Maricopa", geometry = TRUE) 

weights <- blocks("AZ", "Maricopa", year = 2020)

weight_column <- "POP20"

from_id <- "GEOID"
to_id <- "GEOID"

crs <- 26950

extensive <- TRUE