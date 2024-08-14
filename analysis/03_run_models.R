# Regression models for histo vs. svi metrics ----

# load pkgs
library(tidyverse)
library(glmmTMB)
library(here)
library(broom.mixed)
library(DHARMa)
library(glue)

# load cleaned data (long format)
histo_cleaned <- read_csv("data/histo_cleaned.csv")

# labels for minority health index
covars <- c(
  "socioeconomic_status", 
  "household_disability", 
  "minority_language", 
  "housing_transportation", 
  "healthcare_infrastructure", 
  "medical_vulnerability", 
  "overall_svi"
)

# make wide format summing # of cases and population and adding tertiles ---- 
histo_wide <- 
  histo_cleaned %>%
  mutate(pop_mean = mean(pop_est),
         pop_est = sum(pop_est), 
         cases = sum(cases), 
         .by = c("county_name", "state")) %>%
  select(-year) %>%
  distinct() %>%
  mutate(across(all_of(c(covars, 
                         paste0("strat_", covars))),
                ~factor(case_when(.x <= 0.33 ~ 1,
                                  .x <= 0.66 & .x > 0.33 ~ 2, 
                                  .x > 0.66 ~ 3), 
                        labels = c("Low", "Mid", "High")), 
                .names = "{.col}_tertile"), 
         rural_score_summ = as.factor(rural_score_summ))

write_csv(histo_wide, here("data/histo_cleaned_wide.csv"))

histo_wide <- 
  histo_wide %>%
  mutate(pop_cscaled = scale(pop_est, center = TRUE, scale = TRUE)) %>%
  mutate(strat_pop_cscaled = scale(pop_est, center = TRUE, scale = TRUE), 
         .by = rural_score_summ)

# hurdle models ----
set.seed(3570)

## all counties ----
# continuous 
mod_hurdle <- 
  glmmTMB(cases ~ 
          household_disability +
          socioeconomic_status + 
          minority_language +
          housing_transportation +
          healthcare_infrastructure +
          medical_vulnerability +
          rural_score_summ +
          (1 | state),
        family = truncated_poisson, 
        offset = log(pop_est),
        zi = ~
          household_disability +
          socioeconomic_status + 
          minority_language +
          housing_transportation +
          healthcare_infrastructure +
          medical_vulnerability +
          rural_score_summ +
          pop_cscaled +
          (1 | state),
        data = histo_wide)

# tertile
mod_hurdle_tert <-
  glmmTMB(cases ~ 
          household_disability_tertile + 
          socioeconomic_status_tertile + 
          minority_language_tertile + 
          housing_transportation_tertile + 
          healthcare_infrastructure_tertile + 
          medical_vulnerability_tertile + 
          rural_score_summ + 
          (1 | state),
        family = truncated_poisson, 
        offset = log(pop_est),
        zi = ~ 
          household_disability_tertile + 
          socioeconomic_status_tertile + 
          minority_language_tertile + 
          housing_transportation_tertile + 
          healthcare_infrastructure_tertile + 
          medical_vulnerability_tertile + 
          rural_score_summ + 
          + pop_cscaled +
          (1 | state),
        data = histo_wide)

## stratified by rurality ----
mod_strat <-
  histo_wide %>%
  split(., .$rural_score_summ) %>%
  map(~glmmTMB(cases ~ 
                 strat_household_disability + 
                 strat_socioeconomic_status + 
                 strat_minority_language + 
                 strat_housing_transportation + 
                 strat_healthcare_infrastructure + 
                 strat_medical_vulnerability + 
                 (1 | state),
               family = truncated_poisson, 
               offset = log(pop_est),
               zi = ~ 
                 strat_household_disability + 
                 strat_socioeconomic_status + 
                 strat_minority_language + 
                 strat_housing_transportation + 
                 strat_healthcare_infrastructure + 
                 strat_medical_vulnerability + 
                 strat_pop_cscaled +
                 (1 | state),
               data = .x))

mod_strat_tert <-
  histo_wide %>%
  split(., .$rural_score_summ) %>%
  map(~glmmTMB(cases ~ 
                  strat_household_disability_tertile + 
                  strat_socioeconomic_status_tertile + 
                  strat_minority_language_tertile + 
                  strat_housing_transportation_tertile + 
                  strat_healthcare_infrastructure_tertile + 
                  strat_medical_vulnerability_tertile + 
                  (1 | state),
                family = truncated_poisson, 
                offset = log(pop_est),
                zi = ~  
                  strat_household_disability_tertile + 
                  strat_socioeconomic_status_tertile + 
                  strat_minority_language_tertile + 
                  strat_housing_transportation_tertile + 
                  strat_healthcare_infrastructure_tertile + 
                  strat_medical_vulnerability_tertile + 
                  strat_pop_cscaled +
                  (1 | state),
                data = .x))

# tidy and summarize models ----

## coefficient estimates ----
all_tertile_mods <- c(list("All counties" = mod_hurdle_tert), mod_strat_tert)
  
tertile_ests <-  
  all_tertile_mods %>% 
  map(broom.mixed::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "model")

all_cont_mods <- c(list("All counties" = mod_hurdle), mod_strat)

continuous_ests <-
  all_cont_mods %>% 
  map(broom.mixed::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "model") 

all_data <- c(list("All counties" = histo_wide), 
              histo_wide %>% split(., .$rural_score_summ))

## state effects ----
tertile_ranefs <- 
  map(all_tertile_mods, 
      ~as.data.frame(ranef(.x))) %>%
  bind_rows(.id = "model")

continuous_ranefs <- 
  map(all_cont_mods, 
      ~as.data.frame(ranef(.x))) %>%
  bind_rows(.id = "model")

## predictions ----
tertile_preds <-
  map2(all_tertile_mods, all_data, 
     ~data.frame(predicted_cases = predict(.x, type = "response"), 
                 observed_cases = .y$cases, 
                 pop_est = .y$pop_est, 
                 fips_code = .y$fips_code)) %>%
  bind_rows(.id = "model") 

continuous_preds <-
  map2(all_cont_mods, all_data, 
       ~data.frame(predicted_cases = predict(.x, type = "response"), 
                   observed_cases = .y$cases, 
                   pop_est = .y$pop_est, 
                   fips_code = .y$fips_code)) %>%
  bind_rows(.id = "model")

continuous_preds %>% count(model)
histo_wide %>% count(rural_score_summ)

## residuals/diagnostics with DHARMA ----

summarize_diagnostics <- function(model) {
  
  diag <- simulateResiduals(model)
  tests <- c(testResiduals(diag, plot = FALSE))
  data.frame(test_type = unlist(map(tests, ~.x$method)),
             statistic = unlist(map(tests, ~.x$statistic)), 
             p_val = unlist(map(tests, ~.x$p.value)), 
             row.names = NULL)
  
}

tertile_diags <-
  map(all_tertile_mods, 
    summarize_diagnostics) %>%
  bind_rows(.id = "model")

continuous_diags <-
  map(all_cont_mods, 
      summarize_diagnostics) %>%
  bind_rows(.id = "model")

# write out all combined outputs ----

# coefficient estimates
write_csv(bind_rows(tertile_ests %>% mutate(type = "tertile"), 
                    continuous_ests %>% mutate(type = "continuous")), 
          "output/model_effect_estimates.csv")

write_csv(bind_rows(tertile_diags %>% mutate(type = "tertile"), 
                    continuous_diags %>% mutate(type = "continuous")), 
          "output/model_diagnostics.csv")

write_csv(bind_rows(tertile_preds %>% mutate(type = "tertile"), 
                    continuous_preds %>% mutate(type = "continuous")), 
          "output/model_preds.csv")

write_csv(bind_rows(tertile_ranefs %>% mutate(type = "tertile"), 
                    continuous_ranefs %>% mutate(type = "continuous")), 
          "output/model_state_intercepts.csv")

saveRDS(all_cont_mods, "output/all_continuous_mods.rds")
saveRDS(all_tertile_mods, "output/all_tertile_mods.rds")
