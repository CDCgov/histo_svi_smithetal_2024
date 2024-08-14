# Clean histo data and merge with svi, pop, and rurality data for all counties
# in selected states ----

# load libraries
library(tidycensus)
library(tidyverse)
library(readxl)
library(here)

# load in data
histo_data <- read_excel(here("data/raw/HistoSVI.xlsx"), sheet = "SVI")
svi <- read_csv(here("data/regional_mh_svi.csv"))

# add some metadata from tidycensus for other counties
co_metadata <-
  tidycensus::fips_codes %>%
  mutate(GEOID = paste0(state_code, county_code)) 

# clean up histo data ----
histo_long <-
  histo_data %>% 
  select(
    state_name = `State Name`,
    fips_code, 
    cases_2019 = `Histo cases in 2019`, 
    cases_2020 = `Histo cases in 2020`, 
    cases_2011 = `Histo cases in 2011`,
    cases_2012 = `Histo cases in 2012`,
    cases_2013 = `Histo cases in 2013`,
    cases_2014 = `Histo cases in 2014`,
  ) %>%
  mutate(across(starts_with("cases"), as.numeric)) %>%
  pivot_longer(starts_with("cases"), names_to = "year", names_prefix = "cases_", 
               values_to = "cases") %>%
  mutate(year = as.numeric(year))

# fill in missing data ----
all_st_cos <-
  co_metadata %>%
  filter(state_name %in% histo_long$state_name) %>%
  select(state, state_name, fips_code = GEOID, county_name = county)

# get state name and county pop data for 2011:2014, 2019 ----
pops <- 
  get_estimates(geography = "county", product = "population", year = 2019, 
                time_series = TRUE) %>%
  filter(variable == "POP", 
         DATE %in% c(4:7, 12)) %>%
  mutate(year = case_when(DATE == 4 ~ 2011,  
                          DATE == 5 ~ 2012,
                          DATE == 6 ~ 2013, 
                          DATE == 7 ~ 2014, 
                          DATE == 12 ~ 2019)) %>%
  select(fips_code = GEOID, pop_est = value, year)

# get decennial census ests from 2020
# use `load_variables(year = 2020, dataset = "pl")` to get info on variable names
pops <- 
  get_decennial(geography = "county", variables = "P1_001N", year = 2020) %>%
  select(fips_code = GEOID, pop_est = value) %>%
  mutate(year = 2020) %>%
  bind_rows(pops)


# merge all data sets together
histo_svi_cleaned <- 
  svi %>%
  select(-state_name) %>%
  left_join(pops) %>%
  left_join(histo_long) %>%
  mutate(cases = replace_na(cases, 0)) 

# should be false if everything joined up correctly!
any(is.na(histo_svi_cleaned))

# should be true if everything joined up correctly!
merged <- histo_svi_cleaned %>% group_by(state) %>% summarise(nonzero = sum(cases > 0))
orig <- histo_long %>% group_by(state_name) %>% summarize(nonzero = sum(cases > 0))
all(merged$nonzero == orig$nonzero)

write_csv(histo_svi_cleaned, "data/histo_cleaned.csv")
