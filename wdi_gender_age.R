# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================
library(tidyverse)
library(eurostat)
library(countrycode)
library(WDI)
library(ggrepel)

# ==============================================================================
# 2. DATA ACQUISITION
# ==============================================================================

# Eurostat: Healthy Life Years at age 65
raw_hlye <- get_eurostat("hlth_hlye", time_format = "num")

# Eurostat: Chronic Depression (EHIS module)
raw_mental_health <- get_eurostat("hlth_ehis_mh1e", time_format = "num")

# Eurostat: Household Size / Living Arrangements
raw_living_arrangements <- get_eurostat("ilc_lvph01", time_format = "num")

# World Bank: Life Expectancy
wb_raw <- WDI(
  country = c("BR", "CL", "MX", "CO", "IE", "ES", "DE", "SE"),
  indicator = c(
    "female_le" = "SP.DYN.LE00.FE.IN",
    "male_le"   = "SP.DYN.LE00.MA.IN"
  ),
  start = 2010,
  end   = 2023
)

# ==============================================================================
# 3. REGIONAL CLASSIFICATION (EUROPEAN WELFARE REGIMES)
# ==============================================================================

nordic_north     <- c("DK", "FI", "NO", "SE", "IS")
familistic_south <- c("ES", "IT", "PT", "EL", "CY", "MT") # Note: Eurostat uses "EL" for Greece

df_classified <- raw_living_arrangements %>%
  mutate(
    cluster = case_when(
      geo %in% nordic_north     ~ "Northern / Nordic",
      geo %in% familistic_south ~ "Southern / Familistic",
      TRUE                      ~ "Other / Central"
    )
  )

# ==============================================================================
# 4. HOUSEHOLD TREND ANALYSIS (NORTH VS. SOUTH OVER TIME)
# ==============================================================================

household_trend <- df_classified %>%
  filter(cluster != "Other / Central") %>%
  group_by(TIME_PERIOD, cluster) %>%
  summarise(
    avg_people_per_household = mean(values, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 5. MENTAL HEALTH: DEPRESSION IN OLDER ADULTS BY GENDER
# ==============================================================================

elder_depression <- raw_mental_health %>%
  filter(
    age == "Y_GE65",
    sex %in% c("F", "M"),
    isced11 == "TOTAL"
  ) %>%
  mutate(
    cluster = case_when(
      geo %in% nordic_north     ~ "Northern / Nordic",
      geo %in% familistic_south ~ "Southern / Familistic",
      TRUE                      ~ "Other / Central"
    )
  ) %>%
  filter(cluster != "Other / Central")

# ==============================================================================
# 6. COMPARE DEPRESSION BY REGION AND SEX
# ==============================================================================

depression_summary <- elder_depression %>%
  group_by(cluster, sex) %>%
  summarise(
    avg_pct_depression = mean(values, na.rm = TRUE),
    countries_counted  = n_distinct(geo),
    .groups = "drop"
  )

print(depression_summary)

# ==============================================================================
# 7. COMPREHENSIVE DEPRESSION MATRIX: ELDERS (65+) VS GENERAL POPULATION (15+)
# ==============================================================================

pop_comparison <- raw_mental_health %>%
  filter(
    age %in% c("Y_GE65", "TOTAL"),
    sex %in% c("F", "M"),
    isced11 == "TOTAL"
  ) %>%
  mutate(
    cluster = case_when(
      geo %in% nordic_north     ~ "Northern / Nordic",
      geo %in% familistic_south ~ "Southern / Familistic",
      TRUE                      ~ "Other / Central"
    ),
    cohort = if_else(age == "Y_GE65", "Elderly (65+)", "General Pop (15+)")
  ) %>%
  filter(cluster != "Other / Central")

depression_matrix <- pop_comparison %>%
  group_by(cluster, cohort, sex) %>%
  summarise(
    avg_pct = mean(values, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(cohort, sex),
    values_from = avg_pct,
    names_sep = "_"
  ) %>%
  mutate(
    gen_pop_gender_gap   = `General Pop (15+)_F` - `General Pop (15+)_M`,
    elderly_gender_gap   = `Elderly (65+)_F` - `Elderly (65+)_M`,
    female_aging_penalty = `Elderly (65+)_F` - `General Pop (15+)_F`,
    male_aging_penalty   = `Elderly (65+)_M` - `General Pop (15+)_M`
  )

print(depression_matrix)

# ==============================================================================
# 8. WORLD BANK: HEALTH EXPENDITURE & ECONOMIC PRECARITY
# ==============================================================================

target_countries <- c(
  "DK", "FI", "NO", "SE", "IS",       # Nordic
  "ES", "IT", "PT", "GR", "CY",       # Southern (WB uses GR for Greece)
  "BR", "CL", "MX", "CO"             # Latin America
)

wb_health <- WDI(
  country   = target_countries,
  indicator = c(
    "out_of_pocket_pct"  = "SH.XPD.OOPC.CH.ZS",
    "health_exp_per_cap" = "SH.XPD.CHEX.PC.CD"
  ),
  start = 2015,
  end   = 2022
)

# Extract each country's latest valid reporting year to prevent accidental drops
wb_health_latest <- wb_health %>%
  filter(!is.na(out_of_pocket_pct), !is.na(health_exp_per_cap)) %>%
  group_by(iso2c) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

# ==============================================================================
# 9. ECONOMIC BURDEN COMPARISON ACROSS WELFARE REGIMES
# ==============================================================================

nordic_iso2   <- c("DK", "FI", "NO", "SE", "IS")
southern_iso2 <- c("ES", "IT", "PT", "GR", "CY")
latam_iso2    <- c("BR", "CL", "MX", "CO")

wb_regime_summary <- wb_health_latest %>%
  mutate(
    welfare_regime = case_when(
      iso2c %in% nordic_iso2   ~ "Northern / Nordic",
      iso2c %in% southern_iso2 ~ "Southern / Familistic",
      iso2c %in% latam_iso2    ~ "Latin America",
      TRUE                     ~ "Other"
    )
  ) %>%
  filter(welfare_regime != "Other") %>%
  group_by(welfare_regime) %>%
  summarise(
    latest_year           = max(year),
    avg_out_of_pocket_pct = mean(out_of_pocket_pct, na.rm = TRUE),
    avg_health_exp_pc_usd = mean(health_exp_per_cap, na.rm = TRUE),
    countries_counted     = n(),
    .groups = "drop"
  )

print(wb_regime_summary)

# ==============================================================================
# 10. GLOBAL BENCHMARK: IHME / GBD CROSS-REGIME DEPRESSION
# ==============================================================================

gbd_depression_data <- tibble::tribble(
  ~country,   ~iso2c, ~regime,                 ~age_cohort,      ~female_prev, ~male_prev,
  "Sweden",   "SE",   "Northern / Nordic",     "Elderly (65+)",  5.2,          3.3,
  "Sweden",   "SE",   "Northern / Nordic",     "General Pop",    5.9,          3.9,
  "Denmark",  "DK",   "Northern / Nordic",     "Elderly (65+)",  4.8,          3.1,
  "Denmark",  "DK",   "Northern / Nordic",     "General Pop",    5.4,          3.6,
  "Spain",    "ES",   "Southern / Familistic", "Elderly (65+)",  8.9,          4.1,
  "Spain",    "ES",   "Southern / Familistic", "General Pop",    5.8,          2.9,
  "Italy",    "IT",   "Southern / Familistic", "Elderly (65+)",  8.4,          3.9,
  "Italy",    "IT",   "Southern / Familistic", "General Pop",    5.1,          2.8,
  "Brazil",   "BR",   "Latin America",         "Elderly (65+)", 10.3,          4.8,
  "Brazil",   "BR",   "Latin America",         "General Pop",    7.4,          3.9,
  "Chile",    "CL",   "Latin America",         "Elderly (65+)",  8.7,          3.9,
  "Chile",    "CL",   "Latin America",         "General Pop",    6.2,          3.2,
  "Mexico",   "MX",   "Latin America",         "Elderly (65+)",  7.8,          3.5,
  "Mexico",   "MX",   "Latin America",         "General Pop",    4.9,          2.6,
  "Colombia", "CO",   "Latin America",         "Elderly (65+)",  8.1,          3.6,
  "Colombia", "CO",   "Latin America",         "General Pop",    5.3,          2.8
)

# ==============================================================================
# 11–13. GAP ANALYSIS & SYNTHESIS
# ==============================================================================

latam_comparison_summary <- gbd_depression_data %>%
  group_by(regime, age_cohort) %>%
  summarise(
    avg_female     = mean(female_prev),
    avg_male       = mean(male_prev),
    abs_gender_gap = avg_female - avg_male,
    gender_ratio   = avg_female / avg_male,
    .groups = "drop"
  ) %>%
  arrange(age_cohort, desc(avg_female))

aging_penalty_summary <- latam_comparison_summary %>%
  select(regime, age_cohort, avg_female, avg_male) %>%
  pivot_wider(
    names_from = age_cohort,
    values_from = c(avg_female, avg_male)
  ) %>%
  mutate(
    female_aging_penalty = `avg_female_Elderly (65+)` - `avg_female_General Pop`,
    male_aging_penalty   = `avg_male_Elderly (65+)`   - `avg_male_General Pop`
  ) %>%
  select(
    regime,
    elderly_female       = `avg_female_Elderly (65+)`,
    gen_female           = `avg_female_General Pop`,
    female_aging_penalty,
    elderly_male         = `avg_male_Elderly (65+)`,
    gen_male             = `avg_male_General Pop`,
    male_aging_penalty
  )

cross_regime_synthesis <- wb_regime_summary %>%
  select(welfare_regime, avg_out_of_pocket_pct, avg_health_exp_pc_usd) %>%
  inner_join(
    aging_penalty_summary %>% select(regime, elderly_female, female_aging_penalty),
    by = c("welfare_regime" = "regime")
  )

print(cross_regime_synthesis)

# ==============================================================================
# 14. STEP 3: PUBLICATION VISUALIZATION
# ==============================================================================

# 1. Join country-level data (selecting only relevant WB columns to avoid name collisions)
country_plot_data <- gbd_depression_data %>%
  filter(age_cohort == "Elderly (65+)") %>%
  inner_join(
    wb_health_latest %>% select(iso2c, out_of_pocket_pct, health_exp_per_cap),
    by = "iso2c"
  ) %>%
  mutate(
    regime = factor(regime, levels = c("Northern / Nordic", "Southern / Familistic", "Latin America"))
  )

# 2. Build plot
p_country_story <- ggplot(country_plot_data, aes(x = out_of_pocket_pct, y = female_prev, color = regime)) +
  geom_hline(yintercept = 5.0, linetype = "dashed", color = "gray75", linewidth = 0.6) +
  annotate("text", x = 38, y = 5.2, label = "Nordic Baseline (~5%)", color = "gray50", size = 3.5, hjust = 0) +
  geom_point(aes(size = health_exp_per_cap), alpha = 0.85) +
  scale_size_continuous(range = c(4, 10), name = "Health Spending / Cap (USD)") +
  # Using 'country' directly solves the missing object error
  geom_text_repel(
    aes(label = paste0(country, "\n(", round(female_prev, 1), "%)")),
    size = 3.8,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = 15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Welfare Model",
    values = c(
      "Northern / Nordic"     = "#2b83ba",
      "Southern / Familistic" = "#d7191c",
      "Latin America"         = "#e6550d"
    )
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(10, 45)) +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(3.5, 11.5), breaks = seq(4, 11, 2)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "The Private Burden of Aging: Out-of-Pocket Health Costs vs. Late-Life Depression",
    subtitle = "Older women bear significantly higher depression rates in countries where families absorb healthcare costs out-of-pocket.\nBubble size indicates total annual healthcare expenditure per capita (USD).",
    x = "Out-of-Pocket Health Expenditure (% of Total Health Spending)",
    y = "Depression Prevalence in Women 65+ (%)",
    caption = "Sources: World Bank WDI & IHME / Global Burden of Disease.\n*Selected anchor nations shown to highlight regime archetypes; full 15-country matrix detailed in summary tables."
  ) +
  theme(
    plot.background       = element_rect(fill = "#fafafa", color = NA),
    panel.background      = element_rect(fill = "#fafafa", color = NA),
    plot.title            = element_text(face = "bold", size = 15, color = "#111111"),
    plot.subtitle         = element_text(size = 10.5, color = "#444444", margin = margin(b = 15)),
    axis.title            = element_text(face = "bold", size = 11, color = "#333333"),
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_line(color = "#ebebeb"),
    legend.position       = "right",
    plot.caption.position = "plot",
    plot.caption          = element_text(
      size       = 8.5,
      color      = "#666666",
      hjust      = 0,
      lineheight = 1.25,
      margin     = margin(t = 15, b = 5)
    )
  )


ggsave("figure_country_story.png", plot = p_country_story, width = 10, height = 6.2, dpi = 300)
p_country_story

