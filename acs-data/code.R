## ----install-packages---------------------------------------------------
## install.packages(c("tidycensus", "tidyverse", "mapview", "leafsync", "ggspatial"))
## # For brand-new features:
## install.packages("remotes")
## remotes::install_github("walkerke/tidycensus")


## ----api-key------------------------------------------------------------
library(tidycensus)

census_api_key("YOUR KEY GOES HERE")


## ----born-in-mexico-------------------------------------------------------------------
library(tidycensus)

born_in_mexico <- get_acs(
  geography = "state", 
  variables = "B05006_150",
  year = 2020 #<<
)


## ----born-in-mexico-show--------------------------------------------------------------
born_in_mexico


## ----median-age-----------------------------------------------------------------------
median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2020 #<<
)


## ----median-age-show------------------------------------------------------------------
median_age


## ----cbsa-data------------------------------------------------------------------------
median_age_cbsa <- get_acs(
  geography = "cbsa", #<<
  variables = "B01002_001",
  year = 2020
)


## ----cbsa-data-show-------------------------------------------------------------------
median_age_cbsa


## ----geographic-subsets---------------------------------------------------------------
washtenaw_median_age <- get_acs(
  geography = "block group",
  variables = "B01002_001",
  state = "MI", #<<
  county = "Washtenaw", #<<
  year = 2020
)


## ----geographic-subsets-show----------------------------------------------------------
washtenaw_median_age


## ----median-home-value----------------------------------------------------------------
median_home_value <- get_acs(
  geography = "county",
  variables = "B25077_001",
  year = 2020
)


## ----get-top-10-percent---------------------------------------------------------------
library(tidyverse)

top10percent <- median_home_value %>%
  mutate(percentile = percent_rank(estimate)) %>% #<<
  filter(percentile >= 0.9) #<<


## ----show-top-10-percent--------------------------------------------------------------
top10percent


## ----separate-------------------------------------------------------------------------
top10percent <- top10percent %>%
  separate( #<<
    NAME, #<<
    into = c("county", "state"), #<<
    sep = ", " #<<
  ) #<<


## ----show-separated-------------------------------------------------------------------
top10percent


## ----state-summary--------------------------------------------------------------------
state_summary <- top10percent %>%
  group_by(state) %>% #<<
  summarize(n = n()) %>% #<<
  arrange(desc(n)) #<<


## ----state-summary-show---------------------------------------------------------------
state_summary


## ----histogram------------------------------------------------------------------------
library(tidyverse)
library(scales)

ggplot(median_home_value, aes(x = estimate)) + 
  geom_histogram(bins = 30) + 
  scale_x_continuous(labels = label_dollar())


## ----nj-income------------------------------------------------------------------------
nj_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "NJ",
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, 
                           " County, New Jersey"))


## ----nj-income-bar--------------------------------------------------------------------
nj_income_bar <- ggplot(nj_income, 
       aes(x = estimate, 
           y = reorder(NAME, estimate))) + 
  geom_col() + 
  labs(title = "Median household income, 2016-2020 ACS", 
       subtitle = "Counties in New Jersey", 
       x = "ACS estimate", 
       y = "") + 
  theme_minimal(base_size = 18) + 
  scale_x_continuous(labels = dollar_format(scale = 0.001, 
                                            suffix = "K"))


## ----nj-income-bar-show---------------------------------------------------------------
nj_income_bar


## ----head-nj-income-------------------------------------------------------------------
nj_income %>%
  arrange(desc(estimate)) %>%
  head(3)


## ----nj-income-errorbar---------------------------------------------------------------
nj_income_errorbar <- ggplot(nj_income, 
       aes(x = estimate, 
           y = reorder(NAME, estimate))) + 
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), #<<
                width = 0.5, size = 0.5) + #<<
  geom_point(color = "navy", size = 3) + #<<
  labs(title = "Median household income, 2016-2020 ACS", 
       subtitle = "Counties in New Jersey", 
       x = "ACS estimate", 
       y = "",
       caption = "Error bars reflect the margin of error around the ACS estimate") + #<< 
  theme_minimal(base_size = 18) + 
  scale_x_continuous(labels = dollar_format(scale = 0.001, 
                                            suffix = "K"))


## ----nj-income-errorbar-show----------------------------------------------------------
nj_income_errorbar


## ----load-subject---------------------------------------------------------------------
subject <- load_variables(2020, "acs5/subject")


## ----prepare-data-request-------------------------------------------------------------
cohort_names <- c("0-4", "5-9", "10-14", "15-19",
                  "20-24", "25-29", "30-34", "35-39",
                  "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75-79",
                  "80-84", "85+")

male_vars <- 2:19 %>%
  str_pad(2, "left", "0") %>%
  paste0("S0101_C03_0", .) %>%
  set_names(cohort_names)

female_vars <- 2:19 %>%
  str_pad(2, "left", "0") %>%
  paste0("S0101_C05_0", .) %>%
  set_names(cohort_names)


## ----get-pyramid-data-----------------------------------------------------------------
male_data <- get_acs(
  geography = "county",
  variables = male_vars,
  state = "MI",
  county = "Washtenaw",
  year = 2020
) %>%
  mutate(sex = "Male",
         estimate = estimate * -1)

female_data <- get_acs(
  geography = "county",
  variables = female_vars,
  state = "MI",
  county = "Washtenaw",
  year = 2020
) %>%
  mutate(sex = "Female")

pyramid_data <- bind_rows(male_data, female_data) %>%
  mutate(variable = factor(variable, levels = cohort_names)) #<<



## ----washtenaw-pyramid----------------------------------------------------------------
washtenaw_pyramid <- ggplot(pyramid_data, 
                            aes(x = estimate, y = variable, 
                                fill = sex)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_minimal(base_size = 18) + 
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000), "k")) + 
  scale_fill_manual(values = c("#00274C", "#FFCB05")) + 
  labs(x = "", 
       y = "ACS estimate", 
       title = "Population structure in Washtenaw County, Michigan", 
       fill = "", 
       caption = "Data source: 2016-2020 ACS & tidycensus R package")


## ----washtenaw-pyramid-show-----------------------------------------------------------
washtenaw_pyramid


## ----get-orleans-income---------------------------------------------------------------
orleans_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "LA",
  county = "Orleans",
  geometry = TRUE
)


## ----view-orleans-income--------------------------------------------------------------
orleans_income


## ----orleans-income-first-------------------------------------------------------------
ggplot(orleans_income, aes(fill = estimate)) + 
  geom_sf() 


## ----orleans-income-show--------------------------------------------------------------
ggplot(orleans_income, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(option = "mako")


## ----orleans-erase--------------------------------------------------------------------
library(tigris)
library(sf)

orleans_erase <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "LA",
  county = "Orleans",
  geometry = TRUE,
  cb = FALSE #<<
) %>%
  st_transform(26982) %>% #<<
  erase_water(area_threshold = 0.99) #<<


## ----orleans-erase-show---------------------------------------------------------------
ggplot(orleans_erase, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(option = "mako")


## ----final-map------------------------------------------------------------------------
library(ggspatial)

final_map <- ggplot(orleans_erase, aes(fill = estimate)) + 
  annotation_map_tile(type = "cartolight", zoom = 11) + #<<
  theme_void(base_size = 20) + 
  geom_sf(alpha = 0.6, lwd = 0.1) + 
  scale_fill_viridis_c(option = "mako", labels = label_dollar()) + 
  labs(title = "Median household income, Orleans Parish LA",
       subtitle = "2016-2020 ACS estimates",
       caption = "Tile source: CARTO / OpenStreetMap contributors",
       fill = "ACS estimate  ")


## ----final-map-show-------------------------------------------------------------------
final_map


## ----moe-example----------------------------------------------------------------------
vars <- paste0("B01001_0", c(20:25, 44:49))
salt_lake <- get_acs(
  geography = "tract",
  variables = vars,
  state = "Utah",
  county = "Salt Lake",
  year = 2019
)
example_tract <- salt_lake %>%
  filter(GEOID == "49035100100")


## ----moe-prop-------------------------------------------------------------------------
moe_prop(25, 100, 5, 3)


## ----slc-grouped----------------------------------------------------------------------
salt_lake_grouped <- salt_lake %>%
  mutate(sex = case_when(
    str_sub(variable, start = -2) < "26" ~ "Male",
    TRUE ~ "Female"
  )) %>%
  group_by(GEOID, sex) %>%
  summarize(sum_est = sum(estimate), 
            sum_moe = moe_sum(moe, estimate))


## ----slc-grouped-show-----------------------------------------------------------------
salt_lake_grouped


## ----nebraska-series------------------------------------------------------------------
years <- c(2010, 2015, 2020)

nebraska_series <- map_dfr(years, function(year) {
  get_acs(
    geography = "county",
    state = "NE",
    variables = "B01002_001",
    year = year
  ) %>%
    mutate(year = year)
}) %>%
  arrange(NAME)


## ----nebraska-series-show-------------------------------------------------------------
nebraska_series


## ----cp-tables------------------------------------------------------------------------
ak_income_compare <- get_acs(
  geography = "county",
  variables = c(
    income15 = "CP03_2015_062",
    income20 = "CP03_2020_062"
  ),
  state = "AK",
  year = 2020
)


## ----cp-tables-show-------------------------------------------------------------------
ak_income_compare


## ----collin-compare, echo = FALSE-----------------------------------------------------
library(patchwork)

ts_maps <- purrr::map_dfr(2019:2020, ~{
  dat <- get_acs(
    geography = "tract",
    variables = "B01001_001",
    state = "TX",
    county = "Collin County",
    geometry = TRUE,
    year = .x
  ) %>%
    mutate(year = .x)
})

ggplot(ts_maps, aes(fill = estimate)) + 
  geom_sf(lwd = 0.1) + 
  theme_void(base_size = 18) + 
  scale_fill_viridis_c() + 
  facet_wrap(~year)



## ----get-wfh-data-------------------------------------------------------
library(sf)

wfh_15 <- get_acs(geography = "tract", variables = "B08006_017", year = 2015,
                  state = "AZ", county = "Maricopa", geometry = TRUE) %>%
  st_transform(26950)

wfh_20 <- get_acs(geography = "tract", variables = "B08006_017", year = 2020,
                  state = "AZ", county = "Maricopa", geometry = TRUE) %>%
  st_transform(26950)


## ----areal-interpolate--------------------------------------------------
library(sf)

wfh_20_to_15 <- wfh_20 %>%
  select(estimate) %>%
  st_interpolate_aw(to = wfh_15, extensive = TRUE)


## ----map-aw-------------------------------------------------------------
library(mapview)
library(leafsync)

m20a <- mapview(wfh_20, zcol = "estimate", layer.name = "2020 geographies")
m15a <- mapview(wfh_20_to_15, zcol = "estimate", layer.name = "2015 geographies")

sync(m20a, m15a)


## ----pop-interpolate----------------------------------------------------
# source("population_weighted_interpolation.R")
library(tigris)
options(tigris_use_cache = TRUE)

maricopa_blocks <- blocks(
  "AZ",
  "Maricopa",
  year = 2020
)

wfh_15_to_20 <- interpolate_pw(
  from = wfh_15,
  to = wfh_20,
  weights = maricopa_blocks,
  weight_column = "POP20",
  crs = 26950,
  extensive = TRUE
)


## ----map-pw-------------------------------------------------------------
m15b <- mapview(wfh_15, zcol = "estimate", layer.name = "2015 geographies")
m20b <- mapview(wfh_15_to_20, zcol = "estimate", layer.name = "2020 geographies")

sync(m15b, m20b)

