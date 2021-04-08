library(tidyverse)
library(janitor)

# Load data
geo_mapping <- read_csv("data-raw/country_mapping.csv")
regions <- geo_mapping %>% 
  select(c(name, four_regions))

gdp <- read_csv("data-raw/total_gdp_ppp_inflation_adjusted.csv")
pop <- read_csv("data-raw/population_total.csv")
lifex <- read_csv("data-raw/life_expectancy_years.csv")
lifex_M <- read_csv("data-raw/life_expectancy_male.csv")
lifex_F <- read_csv("data-raw/life_expectancy_female.csv")

# Reshape data
gdp <- pivot_longer(gdp, cols=2:ncol(gdp), names_to="year", values_to="gdp")
gdp$year <- as.integer(gdp$year)

pop <- pivot_longer(pop, cols=2:ncol(pop), names_to="year", values_to="population")
pop$year <- as.integer(pop$year)

lifex <- pivot_longer(lifex, cols=2:ncol(lifex), names_to="year", values_to="life_expectancy")
lifex$year <- as.integer(lifex$year)

lifex_M <- pivot_longer(lifex_M, cols=2:ncol(lifex_M), names_to="year", values_to="life_exp_male")
lifex_M$year <- as.integer(lifex_M$year)

lifex_F <- pivot_longer(lifex_F, cols=2:ncol(lifex_F), names_to="year", values_to="life_exp_female")
lifex_F$year <- as.integer(lifex_F$year)

# Join data
country_data <- left_join(x=gdp, y=regions, by=c("country"="name"))
country_data <- left_join(x=country_data, y=pop, by=c("country"="country", "year"="year"))
country_data <- left_join(x=country_data, y=lifex, by=c("country"="country", "year"="year"))
country_data <- left_join(x=country_data, y=lifex_M, by=c("country"="country", "year"="year"))
country_data <- left_join(x=country_data, y=lifex_F, by=c("country"="country", "year"="year"))

# Fill na regions for new countries
country_data <- country_data %>% 
  mutate(four_regions = replace(four_regions, is.na(four_regions) & country=="Eswatini", "africa")) %>% 
  mutate(four_regions = replace(four_regions, is.na(four_regions) & country=="North Macedonia", "europe"))

# Rearrange columns and capitalize first letter in region name
country_data <- country_data %>%
  mutate(four_regions=str_to_title(four_regions),
         gdp_percap=gdp/population) %>%
  rename(region=four_regions) %>% 
  select(country, region, year, gdp, gdp_percap, population, life_expectancy, life_exp_male, life_exp_female)

# Subset observations every 5th year
country_5year <- country_data %>% 
  filter(year %in% seq(from=1800, to=2010, by=5))

# Write data
write_csv(country_data, file="data-processed/country_full.csv")
write_csv(country_5year, file="data-processed/country_5yr.csv")