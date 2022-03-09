## ----install-packages----------------------------------------------------
## install.packages(c("tidycensus", "tidyverse", "geofacet", "ggridges"))


## ----api-key-------------------------------------------------------------
## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)


## ----decennial-------------------------------------------------------------------------
pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020
)


## ----view-decennial--------------------------------------------------------------------
pop20


## ----census-table----------------------------------------------------------------------
table_p2 <- get_decennial(
  geography = "state", 
  table = "P2", 
  year = 2020
)


## ----view-table------------------------------------------------------------------------
table_p2


## ----query-by-state--------------------------------------------------------------------
mi_hispanic <- get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "MI", 
  year = 2020
)


## ----show-query-by-state---------------------------------------------------------------
mi_hispanic


## ----query-by-county-------------------------------------------------------------------
washtenaw_hispanic <- get_decennial(
  geography = "tract", 
  variables = "P2_002N", 
  state = "MI", 
  county = "Washtenaw", 
  year = 2020
)


## ----show-query-by-county--------------------------------------------------------------
washtenaw_hispanic


## ----search-variables----------------------------------------------------
vars <- load_variables(2020, "pl")

View(vars)



## ----tidy-data-------------------------------------------------------------------------
group_quarters <- get_decennial(
  geography = "state", 
  table = "P5", 
  year = 2020
)



## ----show-tidy-data--------------------------------------------------------------------
group_quarters


## ----wide-data-------------------------------------------------------------------------
group_quarters_wide <- get_decennial(
  geography = "state", 
  table = "P5",
  year = 2020,
  output = "wide" 
)


## ----show-wide-data--------------------------------------------------------------------
group_quarters_wide


## ----named-variables-------------------------------------------------------------------
vacancies_wide <- get_decennial(
  geography = "county",
  state = "MI",
  variables = c(vacant_households = "H1_003N", 
                total_households = "H1_001N"), 
  output = "wide",
  year = 2020
)


## ----show-named-variables--------------------------------------------------------------
vacancies_wide


## ----tidyverse-------------------------------------------------------------------------
library(tidyverse)

tidyverse_logo()


## ----texas-population------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

tx_population <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  state = "TX"
)



## ----sort-ascending--------------------------------------------------------------------
arrange(tx_population, value)



## ----sort-descending-------------------------------------------------------------------
arrange(tx_population, desc(value))


## ----filter-below-1000-----------------------------------------------------------------
below1000 <- filter(tx_population, value < 1000)

below1000


## ----summary-variable------------------------------------------------------------------
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Native = "P2_007N",
  Asian = "P2_008N",
  HIPI = "P2_009N"
)

az_race <- get_decennial(
  geography = "county",
  state = "AZ",
  variables = race_vars,
  summary_var = "P2_001N",
  year = 2020
)


## ----view-summary-variable-------------------------------------------------------------
az_race


## ----mutate-and-select-----------------------------------------------------------------
az_race_percent <- az_race %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  select(NAME, variable, percent)


## ----view-percent----------------------------------------------------------------------
az_race_percent


## ----largest-group---------------------------------------------------------------------
largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))


## ----view-largest-group----------------------------------------------------------------
largest_group


## ----median-by-group-------------------------------------------------------------------
az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))


## ----get-2010-data---------------------------------------------------------------------
county_pop_10 <- get_decennial(
  geography = "county",
  variables = "P001001", 
  year = 2010 
)

county_pop_10


## ----clean-2010-data-------------------------------------------------------------------
county_pop_10_clean <- county_pop_10 %>%
  select(GEOID, value10 = value) 

county_pop_10_clean


## ----join-data-------------------------------------------------------------------------
county_pop_20 <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020
) %>%
  select(GEOID, NAME, value20 = value)

county_joined <- county_pop_20 %>%
  left_join(county_pop_10_clean, by = "GEOID") 


## ----show-joined-data------------------------------------------------------------------
county_joined


## ----calculate-change------------------------------------------------------------------
county_change <- county_joined %>%
  mutate( 
    total_change = value20 - value10, 
    percent_change = 100 * (total_change / value10) 
  ) 



## ----show-change-----------------------------------------------------------------------
county_change


## ----show-unmatched-areas--------------------------------------------------------------
filter(county_change, is.na(value10))


## ----get-georgia-data------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

ga_hispanic <- get_decennial(
  geography = "county", 
  variables = c(total = "P2_001N",
                hispanic = "P2_002N"), 
  state = "GA",
  year = 2020,
  output = "wide"
) %>%
  mutate(percent = 100 * (hispanic / total))


## ----show-georgia-data-----------------------------------------------------------------
ga_hispanic


## ----histogram-------------------------------------------------------------------------
ggplot(ga_hispanic, aes(x = percent)) + 
  geom_histogram(bins = 10)


## ----boxplot---------------------------------------------------------------------------
ggplot(ga_hispanic, aes(x = percent)) + 
  geom_boxplot() 


## ----scatterplot-----------------------------------------------------------------------
options(scipen = 999) # Disable scientific notation

ggplot(ga_hispanic, aes(x = total, y = percent)) + 
  geom_point()


## ----scatterplot-with-lm---------------------------------------------------------------
ggplot(ga_hispanic, aes(x = total, y = percent)) + 
  geom_point() + 
  geom_smooth(method = "lm") 


## ----scatterplot-with-log-axis---------------------------------------------------------
ggplot(ga_hispanic, aes(x = total, y = percent)) + 
  geom_point() + 
  scale_x_log10() + 
  geom_smooth()


## ----nj-vacancies----------------------------------------------------------------------
nj_vacancies <- get_decennial(
  geography = "county",
  variables = c(total_households = "H1_001N",
                vacant_households = "H1_003N"),
  state = "NJ",
  year = 2020,
  output = "wide"
) %>%
  mutate(percent_vacant = 100 * (vacant_households / total_households))


## ----first-bar-chart-----------------------------------------------------
ggplot(nj_vacancies, aes(x = percent_vacant, y = NAME)) +
  geom_col()


## ----second-bar-chart----------------------------------------------------
library(scales)

ggplot(nj_vacancies, aes(x = percent_vacant, y = reorder(NAME, percent_vacant))) +
  geom_col() +
  scale_x_continuous(labels = label_percent(scale = 1)) + 
  scale_y_discrete(labels = function(y) str_remove(y, " County, New Jersey")) + 
  labs(x = "Percent vacant households", 
       y = "", 
       title = "Household vacancies by county in New Jersey", 
       subtitle = "2020 decennial US Census") 



## ----third-bar-chart-----------------------------------------------------
ggplot(nj_vacancies, aes(x = percent_vacant, y = reorder(NAME, percent_vacant))) +
  geom_col(fill = "navy", color = "navy", alpha = 0.5) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_discrete(labels = function(y) str_remove(y, " County, New Jersey")) +
  labs(x = "Percent vacant households",
       y = "",
       title = "Household vacancies by county in New Jersey",
       subtitle = "2020 decennial US Census")


## ----get-nyc-data----------------------------------------------------------------------
nyc_percent_black <- get_decennial(
  geography = "tract",
  variables = "P2_006N",
  summary_var = "P2_001N",
  state = "NY",
  county = c("New York", "Kings",
             "Queens", "Bronx",
             "Richmond"),
  year = 2020
) %>%
  mutate(percent = 100 * (value / summary_value))


## ----show-nyc-data---------------------------------------------------------------------
nyc_percent_black


## ----separate-nyc-data-----------------------------------------------------------------
nyc_percent_black2 <- nyc_percent_black %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep = ", ")


## ----show-separated-nyc-data-----------------------------------------------------------
nyc_percent_black2


## ----overlapping-density-plots---------------------------------------------------------
ggplot(nyc_percent_black2, 
       aes(x = percent, fill = county)) + 
  geom_density(alpha = 0.3)


## ----faceted-plots-------------------------------------------------------
ggplot(nyc_percent_black2, aes(x = percent)) +
  geom_density(fill = "darkgreen", color = "darkgreen", alpha = 0.5) +
  facet_wrap(~county) + 
  scale_x_continuous(labels = label_percent(scale = 1)) + 
  theme_minimal(base_size = 14) + 
  theme(axis.text.y = element_blank()) + 
  labs(x = "Percent Black", 
       y = "", 
       title = "Black population shares by Census tract, 2020") 


## ----ridgeline-plots-----------------------------------------------------
library(ggridges)

ggplot(nyc_percent_black2, aes(x = percent, y = county)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(x = "Percent Black, 2020 (by Census tract)",
       y = "") +
  scale_x_continuous(labels = label_percent(scale = 1))


## ----get-us-data---------------------------------------------------------

us_percent_white <- map_dfr(c(state.abb, "DC"), function(state) {
  get_decennial(
    geography = "tract",
    variables = "P2_005N",
    summary_var = "P2_001N",
    state = state,
    year = 2020
  ) %>%
    mutate(percent = 100 * (value / summary_value)) %>%
    separate(NAME, into = c("tract", "county", "state"),
             sep = ", ")
})



## ----geofaceted-plot-----------------------------------------------------
library(geofacet)

ggplot(us_percent_white, aes(x = percent)) +
  geom_histogram(fill = "navy", alpha = 0.8, bins = 30) +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "navy")) +
  facet_geo(~state, grid = "us_state_grid2",
            label = "code", scales = "free_y") +
  theme(axis.text = element_blank(),
        strip.text.x = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "Non-Hispanic white population shares among Census tracts",
       fill = "",
       caption = "Data source: 2020 decennial US Census & tidycensus R package\nX-axes range from 0% white (on the left) to 100% white (on the right).  Y-axes are unique to each state.")

