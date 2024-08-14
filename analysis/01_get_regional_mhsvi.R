# Calculate regional SVI for an arbitrary region ----
# data and code from Sarah Rockhill: https://github.com/s5rockhill/mh-svi

library(tidyverse)
library(here)

mh <- read_csv(here("data/raw/mh_svi_20230329.csv"))
nchs <- readxl::read_excel(here("data/raw/NCHS Urban_Rural Codes.xlsx"))

# for our subset of states
states <- c('AR', 'IL', 'IN', 'KY', 'MI', 'MN', 'PA', 'WI')  

# Create ranking function that is analogous to SQL PERCENT_RANK() ----
# The default dplyr percent_rank() function ranks NA values; 
# however, this function excludes NAs. 
# The function also sets the ties method to "max" when ranking descending values. 

sql_rank <- function(x, direction='asc'){
  if(direction=='desc') {
    output<-(
      ifelse(is.na(x), NA, 
             round(
               (rank(desc(round(x, 4)), ties.method = "max") - 1) / (sum(!is.na(x))-1), 4)))
  }
  
  if(direction=='asc'){
    output<-(
      ifelse(is.na(x), NA, 
             round(
               (rank(round(x, 4), ties.method = "min") - 1) / (sum(!is.na(x))-1), 4)))
  }
  
  return(output)
}

# clean up data, filter to states in analysis, join with rurality ----
nchs <- 
  janitor::clean_names(nchs) %>%
  # one manual fix for a county name mismatch
  mutate(county_name = case_when(state_abr == "IL" & county_name == "La Salle County" ~ "LaSalle County", 
                                 TRUE ~ county_name))
mh <- 
  mh %>%
  mutate(across(everything(), ~ifelse(.x == -999, NA, .x)), 
         FIPS = str_pad(FIPS, 5, side = "left", pad = "0")) %>%
  filter(ST_ABBR %in% states) %>% 
  mutate(county_name = glue::glue("{COUNTY} {ifelse(ST_ABBR == 'LA', 'Parish', 'County')}")) %>%
  left_join(nchs, by = c("ST_ABBR" = "state_abr", "county_name")) %>%
  rename(rural_score = x2013_code) %>%
  # also collapse rural scores per Fletcher et al 2021
  mutate(rural_score_summ = 
           case_when(rural_score %in% c(1, 2) ~ "Large Metropolitan", 
                     rural_score %in% c(3, 4) ~ "Medium/Small Metropolitan", 
                     rural_score %in% c(5, 6) ~ "Micropolitan/Non-core"))
  
# Calculate theme-specific and overall sum scores and percentile rank ----
mh <- mh %>%
  mutate(
    new_theme1_sum = round(
      sql_rank(EP_POV) +
        sql_rank(EP_UNEMP) +
        sql_rank(EP_PCI, 'desc') +
        sql_rank(EP_NOHSDP), 4), 
    new_theme1_tile = round(sql_rank(new_theme1_sum), 4),  
    new_theme2_sum = round(
      sql_rank(EP_AGE65) +
        sql_rank(EP_AGE17) +
        sql_rank(EP_DISABL) +
        sql_rank(EP_SNGPNT), 4),
    new_theme2_tile = round(sql_rank(new_theme2_sum), 4),  
    new_theme3_sum = round(
      sql_rank(EP_AIAN) +
        sql_rank(EP_ASIAN) +
        sql_rank(EP_AFAM) +
        sql_rank(EP_NHPI) +
        sql_rank(EP_HISP) +
        sql_rank(EP_OTHER) +
        sql_rank(EP_SPAN) +
        sql_rank(EP_CHIN) +
        sql_rank(EP_VIET) +
        sql_rank(EP_KOR) +
        sql_rank(EP_RUS), 4),
    new_theme3_tile = round(sql_rank(new_theme3_sum), 4), 
    new_theme4_sum = round(
      sql_rank(EP_MUNIT) +
        sql_rank(EP_MOBILE) +
        sql_rank(EP_CROWD) +
        sql_rank(EP_NOVEH) +
        sql_rank(EP_GROUPQ), 4),
    new_theme4_tile = round(sql_rank(new_theme4_sum), 4), 
    new_theme5_sum =  
      round(
        sql_rank(R_HOSP, 'desc') +
          sql_rank(R_URG, 'desc') +
          sql_rank(R_PHARM, 'desc') +
          sql_rank(R_PCP, 'desc') +
          sql_rank(EP_UNINSUR), 4),
    new_theme5_tile = round(sql_rank(new_theme5_sum), 4),
    new_theme6_sum =  round(
      sql_rank(ER_CARDIO) +
        sql_rank(ER_DIAB) +
        sql_rank(ER_OBES) +
        sql_rank(ER_RESPD) +
        sql_rank(EP_NOINT), 4),
    new_theme6_tile = round(sql_rank(new_theme6_sum), 4),
    new_themes_sum = new_theme1_sum +
      new_theme2_sum +
      new_theme3_sum +
      new_theme4_sum +
      new_theme5_sum +
      new_theme6_sum,
    new_themes_tile = round(sql_rank(new_themes_sum), 4)
  )

# stratified by rurality ----
mh <- mh %>%
  mutate(
    strat_theme1_sum = round(
      sql_rank(EP_POV) +
        sql_rank(EP_UNEMP) +
        sql_rank(EP_PCI, 'desc') +
        sql_rank(EP_NOHSDP), 4), 
    strat_theme1_tile = round(sql_rank(strat_theme1_sum), 4),  
    strat_theme2_sum = round(
      sql_rank(EP_AGE65) +
        sql_rank(EP_AGE17) +
        sql_rank(EP_DISABL) +
        sql_rank(EP_SNGPNT), 4),
    strat_theme2_tile = round(sql_rank(strat_theme2_sum), 4),  
    strat_theme3_sum = round(
      sql_rank(EP_AIAN) +
        sql_rank(EP_ASIAN) +
        sql_rank(EP_AFAM) +
        sql_rank(EP_NHPI) +
        sql_rank(EP_HISP) +
        sql_rank(EP_OTHER) +
        sql_rank(EP_SPAN) +
        sql_rank(EP_CHIN) +
        sql_rank(EP_VIET) +
        sql_rank(EP_KOR) +
        sql_rank(EP_RUS), 4),
    strat_theme3_tile = round(sql_rank(strat_theme3_sum), 4), 
    strat_theme4_sum = round(
      sql_rank(EP_MUNIT) +
        sql_rank(EP_MOBILE) +
        sql_rank(EP_CROWD) +
        sql_rank(EP_NOVEH) +
        sql_rank(EP_GROUPQ), 4),
    strat_theme4_tile = round(sql_rank(strat_theme4_sum), 4), 
    strat_theme5_sum =  
      round(
        sql_rank(R_HOSP, 'desc') +
          sql_rank(R_URG, 'desc') +
          sql_rank(R_PHARM, 'desc') +
          sql_rank(R_PCP, 'desc') +
          sql_rank(EP_UNINSUR), 4),
    strat_theme5_tile = round(sql_rank(strat_theme5_sum), 4),
    strat_theme6_sum =  round(
      sql_rank(ER_CARDIO) +
        sql_rank(ER_DIAB) +
        sql_rank(ER_OBES) +
        sql_rank(ER_RESPD) +
        sql_rank(EP_NOINT), 4),
    strat_theme6_tile = round(sql_rank(strat_theme6_sum), 4),
    strat_themes_sum = strat_theme1_sum +
      strat_theme2_sum +
      strat_theme3_sum +
      strat_theme4_sum +
      strat_theme5_sum +
      strat_theme6_sum,
    strat_themes_tile = round(sql_rank(strat_themes_sum), 4), 
    .by = rural_score_summ
  )

# rename by svi metrics
slug <- paste0("theme", c(1:6, "s"))
svi_names <- c("socioeconomic_status",
               "household_disability",
               "minority_language",
               "housing_transportation",
               "healthcare_infrastructure",
               "medical_vulnerability",
               "overall_svi")
reg_svis <- paste0("new_", slug, "_tile")
strat_svis <- paste0("strat_", slug, "_tile")
names(reg_svis) <- svi_names
names(strat_svis) <- paste0("strat_", svi_names)

# selects and renames to svi names (baseline & stratified)
mh <-
  mh %>%
  select(fips_code = FIPS, state = ST_ABBR, state_name = STATE, 
         county_name, 
         any_of(reg_svis), any_of(strat_svis), 
         rural_score, rural_score_summ)

# Should be FALSE (none should be NA)
any(is.na(mh))

write_csv(mh,
          here("data/regional_mh_svi.csv"))

