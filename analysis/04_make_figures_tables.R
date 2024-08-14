# Make figures and tables for manuscript ----

library(tidyverse)
library(patchwork)
library(cowplot)
library(here)
library(tidycensus)
library(biscale)
library(sf)
library(corrplot)
library(gt)
library(ggtext)

# data
histo_long <- read_csv(here("data/histo_cleaned.csv"))
histo_cleaned <- read_csv(here("data/histo_cleaned_wide.csv"))
county_metadata <- tidycensus::fips_codes
county_shape <- tidycensus::county_laea
state_shape <- tidycensus::state_laea

# results
mod_effs <- read_csv("output/model_effect_estimates.csv")
mod_diag <- read_csv("output/model_diagnostics.csv")
mod_preds <- read_csv("output/model_preds.csv")
mod_state_ints <- read_csv("output/model_state_intercepts.csv")

# labels for minority health index & other cols
covars <- c(
  "socioeconomic_status", 
  "household_disability", 
  "minority_language", 
  "housing_transportation", 
  "healthcare_infrastructure", 
  "medical_vulnerability", 
  "overall_svi"
)

covar_labs <- c(
  "Socioeconomic Status", 
  "Household Composition & Disability", 
  "Minority Status & Language", 
  "Housing Type & Transportation", 
  "Healthcare Infrastructure & Access", 
  "Medical Vulnerability", 
  "Overall SVI"
)

names(covar_labs) <- covars
covar_labs_tert <- covar_labs
names(covar_labs_tert) <- paste0(covars, "_tertile")

rural_cols <-
  c("All counties" = "grey", 
    "Large Metropolitan" = "#1b9e77", 
    "Medium/Small Metropolitan" = "#d95f02", 
    "Micropolitan/Non-core" = "#7570b3")

# Main figures ----

# all us states
histo_map_all <-
  histo_cleaned %>%
  mutate(inc = cases / pop_est * 1e5, 
         inc_bin = cut(inc, breaks = c(-1, 0, 5, 10, 20, 40, Inf), 
                       labels = c("None", "1 - 5", "6 - 10", 
                                  "11 - 20", "21 - 40", "40+"), 
                       include.lowest = TRUE)) %>%
  left_join(county_metadata, by = c("state", "county_name" = "county")) %>%
  mutate(GEOID = paste0(state_code, county_code)) %>%
  right_join(county_shape) %>%
  st_as_sf()

# county level ones with data (using subsetted bounding box)
histo_map <- filter(histo_map_all, !is.na(cases))

# state level ones with data (inset)
h_xlim <- st_bbox(histo_map)[c(1, 3)]
h_ylim <- st_bbox(histo_map)[c(2, 4)]

## Figure 1: map incidence ----

# map of incidence
incidence_map <-
  ggplot() +
  geom_sf(data = state_shape, fill = "grey", color = "white") +
  geom_sf(data = histo_map,
          aes(fill = inc_bin)) +
  geom_sf(data = state_shape %>% filter(GEOID %in% histo_map$state_code), 
          color = "black", linewidth = 0.5, fill = "NA") +
  scale_fill_manual(values = c("lightgrey", '#f1eef6','#d7b5d8','#df65b0','#dd1c77','#980043'), 
                    name = "Cases per 100k") +
  coord_sf(
    crs=st_crs(histo_map),
    xlim = h_xlim,
    ylim = h_ylim) +
  theme_map()

ggsave("figs/pres_incidence_map.jpeg", incidence_map, bg = "white",
       dpi = 600,
       height = 6, width = 8)

binomial_case_map <-
  ggplot() +
  geom_sf(data = state_shape, fill = "grey", color = "white") +
  geom_sf(data = histo_map,
          aes(fill = factor(ifelse(inc > 0, 1, 0)))) +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = '#980043'),
                    labels = c("No cases", "at least one case"),
                    name = "") +
  coord_sf(
    crs=st_crs(histo_map),
    xlim = h_xlim,
    ylim = h_ylim) +
  theme_map()

ggsave("figs/pres_binomial_map.jpeg", binomial_case_map, bg = "white",
       dpi = 600,
       height = 6, width = 8)

# state inset
state_selected <-
  state_shape %>% 
  sf::st_centroid() %>%
  left_join(select(histo_map_all, GEOID = state_code, state) %>%
              st_drop_geometry()) %>%
  distinct()

states_data <- 
  ggplot() +
  geom_sf(data = state_shape, fill = "grey", color = "white") +
  geom_sf(data = state_shape %>% filter(GEOID %in% histo_map$state_code), 
          color = "black", linewidth = 0.25, fill = "white") +
  ggrepel::geom_label_repel(data = state_selected, aes(label = state, geometry = geometry),
                            stat = "sf_coordinates",
                            fill = alpha("white", 0.5),
                            min.segment.length = 0, size = 3) +
  coord_sf(crs=st_crs(histo_map)) +
  theme_map() +
  theme(panel.border = element_rect(color = "darkgrey", 
                                    linewidth = 2,
                                    fill = NA))

# county incidence by state
cnty_inc <-
  histo_cleaned %>% 
  mutate(avg_inc = mean(cases / pop_est), .by = state) %>%
  ggplot(aes(y = reorder(state, avg_inc), x = cases / pop_est * 1e5)) + 
  geom_boxplot(width = 0.25, outlier.color = NA) +
  geom_point(aes(y = as.numeric(reorder(state, avg_inc)) - 0.3), 
             alpha = 0.25, position = position_jitter(height = 0.1)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Incidence per 100,000 persons", 
       y = "State") +
  theme_minimal_vgrid() 

ggsave("figs/pres_county_inc.jpeg", cnty_inc, height = 6, width = 8, bg = "white", 
       dpi = 600)

# prop of counties with a case at the state level
cnty_prop <-
  histo_cleaned %>% 
  group_by(state) %>%
  summarize(prop_nonzero = sum(cases > 0)/n() * 100,
            avg_inc = mean(cases / pop_est)) %>%
  ggplot(aes(x = reorder(state, avg_inc), y = prop_nonzero)) +
  geom_col(width = 0.025) +
  geom_point(size = 2) +
  labs(x = "State", y = "Percent of counties with\n at least one case") +
  coord_flip(expand = FALSE, clip = "off") +
  ylim(c(0, 100)) +
  theme_minimal_vgrid()

ggsave("figs/pres_county_prop.jpeg", cnty_prop, height = 6, width = 8, bg = "white", 
       dpi = 600)

fig1A <-
  incidence_map + 
  inset_element(states_data, left = 0.9, right = 1.35, top = 1.1, bottom = -0.7, 
                clip = FALSE, ignore_tag = TRUE, align_to = "plot")

design <-
  "111122
   111122
   111133
   111133
   111133"
fig1 <- 
  (fig1A + cnty_prop + cnty_inc) + 
  plot_layout(design = design) +
  plot_annotation(tag_levels = "A")

ggsave("figs/inc_map_fig.jpeg", height = 6, width = 10, bg = "white", 
       dpi = 600)

## save separately for EID
fig1A_sep <-
  incidence_map + 
  labs(tag = "A") +
  inset_element(states_data, left = 0.85, right = 1.55, top = 1.2, bottom = -1.2, 
                clip = FALSE, ignore_tag = TRUE, align_to = "plot")
fig1B_sep <-
  cnty_prop +
  labs(tag = "B") +
  coord_flip(expand = TRUE, clip = "on") 

ggsave("figs/fig1A.jpeg", fig1A_sep, height = 5, width = 5, bg = "white", 
       dpi = 300)
ggsave("figs/fig1B.jpeg", fig1B_sep, height = 5, width = 5, bg = "white", 
       dpi = 300)
ggsave("figs/fig1C.jpeg", cnty_inc + labs(tag = "C"), height = 5, width = 5, bg = "white", 
       dpi = 300)

## Figure 2: rurality vs. incidence ----

effects_rur <-
  mod_effs %>%
  filter(model == "All counties", 
         grepl("rural", term), 
         type == "tertile") %>%
  mutate(across(c(estimate, conf.high, conf.low), 
                ~ifelse(component == "zi", 
                        1/exp(.x), 
                        exp(.x)))) %>%
  ggplot(aes(y = term, x = estimate, color = component)) +
  geom_pointrange(aes(xmax = conf.high, xmin = conf.low), 
                  position = position_dodge(width = 0.5)) + 
  scale_y_discrete(labels = c("Medium/Small Metro",
                              "Micropolitan/Non-core")) +
  scale_color_brewer(labels = c("cond" = "Incidence Rate Ratio (95% CI), conditional",
                                "zi" = "Odds Ratio (95% CI) of observing\nat least one case (95% CI)"), 
                     palette = "Dark2", 
                     name = "Model component") +
  scale_x_log10() +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(y = "", x = "Estimate", tag = "A") +
  cowplot::theme_minimal_grid() +
  theme(legend.position = "bottom", legend.direction = "vertical")

rural_vs_inc <-
  histo_map %>% 
  mutate(rural_class = case_when(rural_score >= 5 ~ 3, 
                                 rural_score == 4 | rural_score == 3 ~ 2, 
                                 rural_score < 3 ~ 1), 
         # focusing on counties with at least one case
         incidence = ifelse(cases == 0, NA, cases / pop_est * 1e5),
         incidence_tertile = case_when(
           incidence <= quantile(incidence, probs = 0.33, na.rm = TRUE) ~ 1, 
           incidence > quantile(incidence, probs = 0.33, na.rm = TRUE) & 
             incidence <= quantile(incidence, probs = 0.66, na.rm = TRUE) ~ 2, 
           incidence > quantile(incidence, probs = 0.66, na.rm = TRUE) ~ 3), 
         biscale = glue::glue("{rural_class}-{incidence_tertile}"))


rural_vs_inc_map <-
  ggplot() +
  geom_sf(data = state_shape, fill = "lightgrey", color = "white") +
  geom_sf(data = rural_vs_inc %>% filter(is.na(incidence)), 
          fill = "white", color = "black") +
  geom_sf(data = rural_vs_inc %>% filter(!is.na(incidence)), 
          aes(fill = biscale)) +
  bi_scale_fill(pal = "DkViolet", dim = 3, guide = "none", 
                na.value = "lightgrey") +
  scale_color_identity() +
  coord_sf(
    crs=st_crs(histo_map),
    xlim = h_xlim,
    ylim = h_ylim) +
  theme_map() +
  labs(tag = "B")


rural_vs_inc_dist <-
  ggplot(rural_vs_inc,
         aes(y = rural_score_summ, x  = incidence)) +
  ggdist::stat_slab(color = "grey50", normalize = "none") +
  geom_boxplot(color = "black", width = 0.15, outlier.color = NA) +
  geom_point(aes(y = as.numeric(factor(rural_score_summ)) - 0.25, 
                 color = biscale),
             alpha = 0.5, position = position_jitter(height = 0.1)) +
  bi_scale_color(pal = "DkViolet", dim = 3, guide = "none", 
                 na.value = "lightgrey") +
  scale_y_discrete(labels = function(x) gsub("politan", "", x)) +
  labs(x = "Incidence per 100,000 persons", 
       y = "") +
  cowplot::theme_minimal_grid() +
  labs(tag = "C")

rural_vs_inc_leg <-
  bi_legend("DkViolet", dim = 3, xlab = "More rural", ylab = "Higher Incidence")


design <-
  "111#
 111#
 2223
 2223
 2224
 2224"

fig2 <- 
  effects_rur + rural_vs_inc_map + states_data + rural_vs_inc_leg +
  plot_layout(design = design)

design_fig2B <- 
  "1112
   1113"

fig2B_sep <- 
  rural_vs_inc_map + states_data + rural_vs_inc_leg +
  plot_layout(design = design_fig2B)

fig2A_sep <-
  effects_rur +
  theme(text = element_text(size = 11))

ggsave("figs/rural_vs_inc.jpeg", fig2, bg = "white", dpi = 600, 
       height = 8, width = 8)

## save separately for EID
ggsave("figs/fig2A.jpeg", fig2A_sep, height = 4, width = 5, bg = "white", 
       dpi = 300)
ggsave("figs/fig2B.jpeg", fig2B_sep, height = 6, width = 5, bg = "white", 
       dpi = 300)


## Figure 3: models & stratified analyses ----

main_effs <-
  mod_effs %>%
  filter(!grepl("rural|Intercept|cscaled", term), 
         type == "tertile") %>%
  mutate(across(c(estimate, conf.high, conf.low), 
                ~ifelse(component == "zi", 
                        round(1/exp(.x), 2), 
                        round(exp(.x), 2)))) %>%
  mutate(term = gsub("strat_", "", term), 
         level = factor(
           case_when(grepl("Mid", term) ~ "Mid", 
                     grepl("High", term) ~ "High"),
           levels = c("Mid", "High")), 
         svi = gsub("Mid|High", "", term)) 

main_eff_fig <-
  main_effs %>%
  ggplot(aes(y = svi, x = estimate)) +
  scale_x_log10(breaks = scales::breaks_log()) +
  scale_y_discrete(labels = covar_labs_tert) +
  geom_pointrange(aes(xmin = conf.low, 
                      xmax = conf.high, 
                      fill = model, 
                      shape = level, 
                      group = level, 
                      alpha = ifelse(p.value <= 0.05, 0.75, 0.5), 
                      size= ifelse(model == "All counties", 1.2, 1), 
                      color = ifelse(p.value <= 0.05, "black", "grey")), 
                  position = position_dodge(width = 0.25)) +
  scale_size_identity() +
  scale_color_identity(name = "Model") +
  scale_alpha_identity() +
  scale_shape_manual(values = c("High" = 23, "Mid" = 22), breaks = c("High", "Mid")) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = "", 
       y = "SVI Theme", 
       shape = "Tertile", 
       fill = "Model") +
  facet_grid(~component, scales = "free_x", 
             labeller = as_labeller(c("cond" = "Incidence Rate Ratio (conditional)", 
                                      "zi" = "Odds Ratio (Probability of\n observing at least one case)")), 
             switch = "x") +
  scale_fill_manual(values = rural_cols) +  
  coord_cartesian(expand = TRUE) +
  theme_minimal_grid(font_size = 12) +
  panel_border() +
  guides(fill = guide_legend(override.aes = list(size = 0.5, shape = 21)),
         shape = guide_legend(override.aes = list(size = 0.5))) 

ggsave("figs/all_mod_effs.jpeg", main_eff_fig, bg = "white", dpi = 600, 
       height = 8, width = 10)


main_eff_fig$data <- main_effs %>% filter(component %in% "cond")
ggsave("figs/pres_cond_effs.jpeg", main_eff_fig, bg = "white", dpi = 600, 
       height = 8, width = 8)


main_eff_fig$data <- main_effs %>% filter(component %in% "zi")
ggsave("figs/pres_zi_effs.jpeg", main_eff_fig, bg = "white", dpi = 600, 
       height = 8, width = 8)

## Table 1: summaries ----

main_effs %>%
  filter(model == "All counties") %>%
  bind_rows(
    mod_effs %>%
      filter(grepl("rural", term), type == "tertile") %>%
      mutate(across(c(estimate, conf.high, conf.low), 
                    ~ifelse(component == "zi", 
                            1/exp(.x), 
                            exp(.x))))) %>% 
  write_csv("output/table1_values.csv")

inc_table <-
  histo_cleaned %>%
  pivot_longer(all_of(paste0(covars, "_tertile"))) %>%
  group_by(value, name) %>%
  summarize(tot_cases = sum(cases),
            inc = tot_cases / sum(pop_est) * 1e5, 
            se = 1 / sqrt(tot_cases), 
            conf_lower = exp(log(inc) - 1.96 * se), 
            conf_upper = exp(log(inc) + 1.96 * se))
write_csv(inc_table, "output/table1_incidence.csv")

rural_table <-
  histo_cleaned %>%
  group_by(rural_score_summ) %>%
  summarize(tot_cases = sum(cases),
            inc = tot_cases / sum(pop_est) * 1e5, 
            se = 1 / sqrt(tot_cases), 
            conf_lower = exp(log(inc) - 1.96 * se), 
            conf_upper = exp(log(inc) + 1.96 * se))

write_csv(rural_table, "output/table1_rural_incidence.csv")

# Supplementary figures + tables ----

## map all SVI metrics ----
histo_svi_map <- 
  histo_map %>%
  pivot_longer(all_of(covars))

all_svi <-
  ggplot() +
  geom_sf(data = state_shape, fill = "lightgrey", color = "white") +
  geom_sf(data = histo_svi_map %>% filter(!name %in% "overall_svi"),
          aes(fill = value)) +
  scale_fill_distiller(direction = 1, name = "SVI") +
  coord_sf(
    crs=st_crs(histo_map),
    xlim = h_xlim,
    ylim = h_ylim) +
  theme_map() +
  facet_wrap(~name, labeller = as_labeller(covar_labs), nrow = 3) +
  labs(tag = "B") +
  theme(strip.text = element_textbox_simple(size = 10, halign = 0.5), 
        strip.clip = "off")

overall_svi <-
  ggplot() +
  geom_sf(data = state_shape, fill = "lightgrey", color = "white") +
  geom_sf(data = histo_svi_map %>% filter(name %in% "overall_svi"),
          aes(fill = value)) +
  scale_fill_distiller(direction = 1, name = "SVI") +
  coord_sf(
    crs=st_crs(histo_map),
    xlim = h_xlim,
    ylim = h_ylim) +
  theme_map() +
  facet_wrap(~name, labeller = as_labeller(c("overall_svi" = "Overall SVI"))) +
  labs(tag = "A") 

sfig_svi_metrics <-
  (overall_svi | all_svi) + plot_layout(widths = c(1, 2), guides = "collect")

ggsave("figs/sfig_svi_metrics.jpeg", sfig_svi_metrics, height = 10, width = 8, bg = "white",
       dpi = 600)

## cases over time by county ----
histo_inc <-
  histo_long %>%
  mutate(inc = cases / pop_est * 1e5, 
         inc_bin = cut(inc, breaks = c(-1, 0, 5, 10, 20, 40, Inf), 
                       labels = c("None", "1 - 5", "6 - 10", 
                                  "11 - 20", "21 - 40", "40+"), 
                       include.lowest = TRUE)) 

inc_heatmap <-
  histo_inc %>%
  ggplot(aes(x = factor(year), y = fips_code, 
             fill = inc_bin)) +
  geom_tile() +
  scale_fill_manual(values = c("grey", '#f1eef6','#d7b5d8','#df65b0',
                               '#dd1c77','#980043'), 
                    name = "Cases per 100k") +
  facet_grid(state ~ ., scales = "free", drop = TRUE) +
  labs(y = "County", x = "Year") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

inc_dist <-
  histo_inc %>%
  ggplot(aes(x = factor(year), y = inc)) +
  geom_boxplot(width = 0.25, outlier.color = NA) +
  geom_point(aes(x = as.numeric(factor(year)) + 0.35), 
             alpha = 0.1, 
             position = position_jitter(width = 0.15)) +
  labs(x = "Year", y = "Cases per 100k")


inc_years <-
  histo_inc %>%
  filter(cases != 0) %>%
  summarize(years_with_case = sum(cases > 0), .by = c("fips_code", "state")) %>%
  ggplot(aes(x = years_with_case, y = after_stat(prop))) +
  geom_bar() +
  labs(x = "Number of years with at least one case", 
       y = "Proportion of counties")

sfig_cases_by_year <- 
  (inc_heatmap | (inc_dist / inc_years)) + 
  plot_annotation(tag_levels = "A")

ggsave("figs/sfig_cases_by_year.jpeg",
       sfig_cases_by_year, height = 6, width = 9,
       bg = "white", dpi = 600)

## svi vs. svi correlations ----
names(covars) <- gsub("&", "&\n", covar_labs)

png("figs/sfig_svi_corrs.jpeg", width = 800, height = 800)

histo_corrs <-
  histo_cleaned %>%
  select(all_of(covars)) %>%
  cor() 

write_csv(histo_corrs %>% 
            as.data.frame(row.names = TRUE) %>% 
            mutate(var = colnames(histo_corrs)), 
          "output/histo_corrs.csv")

sfig_svi_corrs <-
  histo_corrs %>%
  corrplot(tl.col = "black", method = "ellipse",
           type = "lower", diag = FALSE)

dev.off()

## svi vs. rurality (overall and stratified) ----
names(covars) <- ""
covars_strat <- paste0("strat_", covars)
names(covars_strat) <- covars

rural_labs <- c("1" = "Large central metro", 
                "2" = "Large fringe metro", 
                "3" = "Medium metro", 
                "4" = "Small metro", 
                "5" = "Micropolitan", 
                "6" = "Non-core")

unstrat <-
  histo_cleaned %>%
  select(all_of(covars), rural_score_summ, rural_score, fips_code) %>%
  pivot_longer(all_of(covars)) %>%
  mutate(type = "Regional")

svi_comb <- 
  histo_cleaned %>%
  select(all_of(covars_strat), rural_score_summ, rural_score, fips_code) %>%
  pivot_longer(all_of(covars)) %>%
  mutate(type = "Stratified by rurality") %>%
  bind_rows(unstrat)

svi_vs_rurality <-
  svi_comb %>%
  filter(!name %in% c("socioeconomic_status", "overall_svi")) %>%
  ggplot(aes(x = rural_score_summ, y = value)) +
  geom_boxplot(width = 0.15, aes(color = factor(rural_score)), 
               outlier.color = NA) +
  geom_point(aes(x = as.numeric(factor(rural_score_summ)) + 0.3, 
                 y = value, color = factor(rural_score)), 
             alpha = 0.15, 
             position = position_jitter(width = 0.1)) +  
  scale_color_brewer(palette = "Dark2", 
                     labels = rural_labs, 
                     name = "NCHS Urban-Rural\nClassification (non-collapsed)") +
  labs(y = "SVI", x = "") +
  facet_grid(type ~ name, labeller = labeller(name = covar_labs)) +
  coord_flip() +
  theme_minimal_vgrid(font_size = 10) +
  panel_border() +
  theme(strip.text = element_textbox_simple(halign = 0.5), 
        strip.clip = "off", 
        axis.text.x = element_text(angle = 45), 
        plot.margin = margin(20, 2, 2, 2)) +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 0)))

ggsave("figs/sfig_svi_vs_rurality.jpeg", svi_vs_rurality, height = 8, width = 12, bg = "white", 
       dpi = 600)

svi_vs_rurality$data <- 
  svi_comb %>%
  filter(name %in% c("minority_language", "medical_vulnerability"))
ggsave("figs/pres_example_svi_vs_rurality.jpeg", svi_vs_rurality, height = 8, width = 8, bg = "white", 
       dpi = 600)

## rurality vs. pop, svi vs. pop ----
rur_vs_pop <-
  ggplot(histo_cleaned, 
         aes(x = rural_score_summ, y = pop_mean)) + 
  geom_boxplot(width = 0.15, aes(color = factor(rural_score)), 
               outlier.colour = NA) +
  geom_point(aes(x = as.numeric(factor(rural_score_summ)) + 0.3, 
                 y = pop_mean, color = factor(rural_score)), 
             alpha = 0.25, 
             position = position_jitter(width = 0.1)) +  
  scale_y_log10(breaks = scales::breaks_log()) +
  scale_color_brewer(palette = "Dark2", 
                     labels = rural_labs, 
                     name = "Urban-rural classification\n(non-collapsed)") +
  theme_minimal_hgrid(font_size = 12) +
  labs(x = "", y = "Mean county population")


baseline <-
  histo_cleaned %>%
  select(all_of(covars), pop_mean, fips_code, rural_score) %>%
  pivot_longer(-c(pop_mean, fips_code, rural_score)) %>%
  filter(!name %in% c("socioeconomic_status", 
                      "overall_svi")) %>%
  mutate(type = "All counties")

strat <-
  histo_cleaned %>%
  select(all_of(paste0("strat_", covars)), pop_mean, type = rural_score_summ, 
         fips_code, rural_score) %>%
  pivot_longer(-c(pop_mean, fips_code, type, rural_score)) %>%
  mutate(name = gsub("strat_", "", name)) %>%
  filter(!name %in% c("socioeconomic_status", 
                      "overall_svi")) %>%
  bind_rows(baseline)

svi_pop_corrs <-
  strat %>%
  ggplot(aes(x = value, y = pop_mean, color = factor(rural_score))) +
  geom_point(alpha = 0.5) +
  scale_y_log10() +
  labs(x = "SVI", y = "Mean county population") +
  scale_color_brewer(palette = "Dark2", 
                     labels = rural_labs, 
                     name = "Urban-rural classification\n(non-collapsed)", 
                     guide = "none") +
  facet_grid(type~name, labeller = labeller(name = covar_labs)) +
  theme_minimal_grid(font_size = 12) +
  panel_border() +
  theme(strip.text = element_textbox_simple(size = 10, halign = 0.5), 
        strip.clip = "off")

rur_corrs <- (rur_vs_pop / svi_pop_corrs) + plot_layout(heights = c(1, 2), guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("figs/sfig_rur_corrs.jpeg", rur_corrs, height = 12, width = 12, bg = "white", 
       dpi = 600)

## continuous vs. tertile mods -----

terms_all <- c(
  "socioeconomic_status", 
  "household_disability", 
  "minority_language", 
  "housing_transportation", 
  "healthcare_infrastructure", 
  "medical_vulnerability", 
  "rural_score_summMicropolitan/Non-core",
  "rural_score_summMedium/Small Metropolitan",
  "pop_cscaled"
)

term_labs <- c(
  "Socioeconomic Status", 
  "Household Composition & Disability", 
  "Minority Status & Language", 
  "Housing Type & Transportation", 
  "Healthcare Infrastructure & Access", 
  "Medical Vulnerability", 
  "Micropolitan/Non-core",
  "Medium/Small Metropolitan",
  "County population"
)
names(term_labs) <- terms_all

all_effs <-
  mod_effs %>%
  filter(!term %in% c("(Intercept)", "sd__(Intercept)")) %>%
  mutate(term = gsub("strat_", "", term), 
         term = gsub("_tertile", "", term), 
         level = factor(case_when(grepl("Mid", term) ~ "Mid", 
                                  grepl("High", term) ~ "High",
                                  TRUE ~ "Not applicable"), 
                        levels = c("Not applicable", "Mid", "High")), 
         svi = gsub("Mid|High", "", term)) %>% 
  mutate(across(c(estimate, conf.high, conf.low), 
                ~ifelse(component == "zi", 
                        round(1/exp(.x), 2), 
                        round(exp(.x), 2))))
all_mod_effs <-
  all_effs %>% 
  mutate(term = gsub("_tertile", "", term)) %>%
  ggplot(aes(y = svi, x = estimate)) +
  scale_x_log10(breaks = scales::breaks_log()) +
  scale_y_discrete(labels = term_labs) +
  geom_pointrange(aes(xmin = conf.low, 
                      xmax = conf.high, 
                      fill = model, 
                      shape = level, 
                      group = level, 
                      alpha = ifelse(p.value <= 0.05, 0.75, 0.5), 
                      size= ifelse(model == "All counties", 1.2, 1), 
                      color = ifelse(p.value <= 0.05, "black", "grey")), 
                  position = position_dodge(width = 0.5)) +
  scale_size_identity() +
  scale_color_identity(name = "Model") +
  scale_alpha_identity() +
  scale_shape_manual(values = c("High" = 23, "Mid" = 22, "Not applicable" = 21), 
                     breaks = c("High", "Mid", "Not applicable")) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(x = "Effect estimate (exponentiated)", 
       y = "", 
       shape = "Tertile", 
       fill = "Model") +
  facet_grid(type~component, scales = "free_x", 
             labeller = labeller(component = c("cond" = "Conditional", 
                                               "zi" = "Zero-inflated"), 
                                 type = c("tertile" = "Tertile SVI", "continuous" = "Continuous SVI"))) +
  scale_fill_manual(values = rural_cols) +  
  coord_cartesian(expand = TRUE) +
  theme_minimal_grid() +
  panel_border() +
  guides(fill = guide_legend(override.aes = list(size = 0.5, shape = 21)),
         shape = guide_legend(override.aes = list(size = 0.5))) 

ggsave("figs/sfig_all_mod_ests.jpeg", all_mod_effs, height = 12, width = 12, bg = "white", 
       dpi = 600)

## state random effects ----

state_intercepts <-
  mod_state_ints %>%
  mutate(condval = ifelse(component == "zi", -condval, condval)) %>%
  ggplot(aes(y = grp, x = condval, xmin = condval + condsd, 
             xmax = condval - condsd, 
             color = model)) +
  geom_pointrange() +
  scale_color_manual(values = rural_cols) +
  facet_grid(type~component, scales = "free_x", 
             labeller = labeller(component = c("cond" = "Conditional (poisson)", 
                                               "zi" = "Zero-inflated (logistic)"), 
                                 type = c("tertile" = "Tertile SVI", "continuous" = "Continuous SVI"))) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Intercept (SD)", y = "State", color = "Model") +
  theme_minimal_grid() +
  panel_border()

ggsave("figs/sfig_state_intercepts.jpeg", state_intercepts, bg = "white", dpi = 600,
       height = 8, width = 7)

## mod pred vs. observed ----
pred_vs_obs <-
  mod_preds %>%
  split(., .$model) %>%
  map(~.x %>% 
        ggplot(aes(x = observed_cases, y = predicted_cases)) +
        geom_point() +
        facet_grid(model~type, 
                   labeller = labeller(type = c("tertile" = "Tertile SVI", "continuous" = "Continuous SVI"))) +
        geom_abline(slope = 1, intercept = 0, linetype = 2) +
        xlim(c(0, max(.x$predicted_cases, .x$observed_cases))) +
        ylim(c(0, max(.x$predicted_cases, .x$observed_cases))) +
        theme_minimal_grid(font_size = 12) +
        labs(x = "Observed cases", y = "Predicted cases") +
        panel_border() + 
        theme(strip.text.x = element_blank()))

pred_vs_obs[[1]] <- pred_vs_obs[[1]] + theme(strip.text.x = element_text())

all_pred_vs_obs <-
  patchwork::wrap_plots(pred_vs_obs) + plot_layout(nrow = 4) + plot_annotation(tag_levels = "A")

ggsave("figs/sfig_pred_vs_obs.jpeg", all_pred_vs_obs, bg = "white", dpi = 600,
       height = 10, width = 8)

## mod diagnostics -----

all_mod_diags <-
  mod_diag %>%
  mutate(test_short = case_when(grepl("Kolmogorov", test_type) ~ "Uniformity of residuals", 
                                grepl("dispersion", test_type) ~ "Over/underdispersion", 
                                grepl("outlier", test_type) ~ "Outliers")) %>%
  ggplot(aes(x = test_short, y = model, fill = ifelse(p_val <= 0.05, "Failed", "Passed"))) +
  geom_tile(color = "black") +
  theme_minimal_grid(font_size = 12) + 
  labs(y = "Model", x = "DHARMa Diagnostic Test", fill = "Test status") +
  scale_fill_manual(values = c("#fc8d62", "#8da0cb")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(expand = FALSE) +
  panel_border() +
  facet_grid(~type,
             labeller = labeller(type = c("tertile" = "Tertile SVI", "continuous" = "Continuous SVI")))

ggsave("figs/sfig_mod_diags.jpeg", all_mod_diags, bg = "white", dpi = 600,
       height = 6, width = 8)
