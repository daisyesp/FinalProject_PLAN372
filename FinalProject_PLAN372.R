## 
## Daisy Espinoza
## Final Project 
## Plan372
##


library(tidycensus)
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)

# Loading Data
census <- read.csv("ACS/census_5yr_2022.csv")
voter <- read.delim("voter_stats.txt", sep = "\t")
census_variables <- c(total_pop = "B01003_001",
                      median_age = "B01002_001",
                      median_income = "B19013_001",
                      poverty_total = "B17001_001",
                      poverty_below = "B17001_002",
                      edu_total = "B15003_001",
                      edu_bachelors_plus = c("B15003_022", "B15003_023", "B15003_024", "B15003_025"),
                      female = "B01001_026",
                      male = "B01001_002",
                      white = "B02001_002",
                      black = "B02001_003",
                      hispanic = "B03003_003")


#API Key
readRenviron("~/.Renviron")

# accessing ACS 5-Year Estimate, for NC, county-level, 2022
data <- get_acs(
  geography = "county",
  state = "NC",
  variables = census_variables,
  year = 2022,
  survey = "acs5"
)

###### ACS Census Data Clean Up ######

# pivoting data
data_wide <- data %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  pivot_wider(names_from = variable, values_from = estimate)

head(data_wide)

# creating variables to use in my regression model by taking percentages for each demographic variable, and voter turnout 
variables <- census_voter_join %>% 
  mutate(
    turnout_rate = total_voters / total_pop,
    pct_poverty = poverty_below / poverty_total,
    pct_bachelors_plus = (edu_bachelors_plus1 + edu_bachelors_plus2 + edu_bachelors_plus3 + edu_bachelors_plus4) / edu_total,
    pct_white = white / total_pop,
    pct_black = black / total_pop,
    pct_hispanic = hispanic / total_pop,
    pct_female = female / (female + male) 
  )

###### Voter Data Clean Up ######
# cleaning up voter data
head(voter)

# Making one row per county
county_turnout <- voter %>%
  group_by(county_desc) %>%
  summarise(total_voters = sum(total_voters, na.rm = TRUE))

# Joining by county name COUNTY_TURNOUT and DATA_WIDE
data_wide <- data_wide %>%
  mutate(county_clean = str_to_upper(str_remove(NAME, " County, North Carolina")))

county_turnout <- county_turnout %>%
  mutate(county_clean = str_to_upper(county_desc))

census_voter_join <- data_wide %>% 
  left_join(county_turnout, by = "county_clean")


## Using variables object
head(variables)


## Loading in urban/rural data for each county
urban_rural <- read.csv("urban_rural.csv")
urban_rural$COUNTYNAME <- toupper(urban_rural$COUNTYNAME)

# Joining urban_rural to the vairable dataset
variables <- left_join(variables, urban_rural, by = c("county_clean" = "COUNTYNAME"))

###### Regression Model ######

model <- lm(turnout_rate ~ pct_poverty + pct_bachelors_plus + pct_white + pct_black + pct_hispanic + pct_female + URBAN_RURAL, data = variables)

summary(model)



###### Graph of NC County Voter Turnout ######
nc_counties <- counties(state = "NC", cb = TRUE, class = "sf")

# Joining spatial NC counties to the variables to map
nc_counties <- nc_counties %>%
  mutate(NAME_UPPER = toupper(NAME))

nc_counties <- nc_counties %>%
  mutate(county_clean = toupper(NAME))
         
nc_map_data <- left_join(nc_counties, variables, by = "county_clean")
         
         
# Mapping nc counties
ggplot(nc_map_data)+
  geom_sf(aes(fill= turnout_rate)) +
  scale_fill_viridis_c(option = "plasma", name = "Turnout Rate")+
  labs(title = "Voter Turnout by County in North Carolina 2022")+
  theme_minimal()

# Urban Rural Map of NC
ggplot(nc_map_data) +
  geom_sf(aes(fill = URBAN_RURAL), color = "black") + 
  scale_fill_manual(values = c("Urban" = "magenta4", "Rural" = "gold")) + 
  theme_minimal() +
  labs(title = "Map of Urban vs. Rural Counties in North Carolina", fill = "Urban/Rural") +
  theme(axis.text = element_blank(), axis.title = element_blank())
