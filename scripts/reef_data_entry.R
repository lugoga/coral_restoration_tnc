# =============================================================================
# Coral Reef Assessment Report --- Data Entry & Analysis Script
# Unguja North and Mtwara, Tanzania
#
# PURPOSE: Replace all placeholder values below with your actual field data,
#          then re-render the QMD report.
#
# USAGE:   Source this file OR embed data directly in the QMD setup chunk.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. SITE INFORMATION
# =============================================================================
# Update coordinates, MMA status, and depth ranges with actual field data

site_info <- tribble(
  ~site,         ~region,        ~lon,    ~lat,      ~mma,   ~depth_range_m,
  "Fukuchani",   "Unguja North",  39.268,  -5.872,   TRUE,   "3-12",
  "Uroa",        "Unguja North",  39.556,  -6.145,   TRUE,   "4-11",
  "Jongowe",     "Unguja North",  39.220,  -5.972,   FALSE,  "3-12",
  "Kendwa",      "Unguja North",  39.247,  -5.762,   FALSE,  "3-10",
  "Mnazi Bay",   "Mtwara",        40.450, -10.290,   TRUE,   "5-14",
  "Msanga Mkuu", "Mtwara",        40.380, -10.360,   FALSE,  "4-12",
  "Mgano",       "Mtwara",        40.330, -10.430,   FALSE,  "3-11"
)

# =============================================================================
# 2. BENTHIC SUBSTRATE DATA
# =============================================================================
# Enter mean % cover per category per site
# Categories: Hard Coral, Soft Coral, Algae, CCA, Rubble, Sand, Other
# Values per site should sum to ~100%

substrate_data <- tribble(
  ~site,         ~region,        ~mma,   ~hard_coral, ~soft_coral, ~algae, ~cca,  ~rubble, ~sand,  ~other,
  "Fukuchani",   "Unguja North",  TRUE,   38.2,        5.1,         22.4,   8.3,   14.2,    8.7,    3.1,
  "Uroa",        "Unguja North",  TRUE,   29.7,        4.8,         31.5,   6.1,   14.8,    9.5,    3.6,
  "Jongowe",     "Unguja North",  FALSE,  42.1,        6.3,         18.9,   9.2,   12.3,    8.1,    3.1,
  "Kendwa",      "Unguja North",  FALSE,  24.5,        3.9,         38.7,   5.4,   14.9,    9.6,    3.0,
  "Mnazi Bay",   "Mtwara",        TRUE,   51.3,        7.2,         15.4,   10.8,  8.1,     5.6,    2.6,
  "Msanga Mkuu", "Mtwara",        FALSE,  35.8,        5.5,         27.3,   7.9,   12.5,    7.8,    3.2,
  "Mgano",       "Mtwara",        FALSE,  21.4,        4.1,         41.2,   4.9,   15.6,    9.9,    2.9
)

# Long format for plotting (stacked bar)
substrate_long <- substrate_data |>
  select(site, region, mma, hard_coral, soft_coral, algae, rubble) |>
  rename(
    `Hard Coral`   = hard_coral,
    `Soft Coral`   = soft_coral,
    `Algae`        = algae,
    `Rubble/Other` = rubble
  ) |>
  pivot_longer(
    cols      = c(`Hard Coral`, `Soft Coral`, `Algae`, `Rubble/Other`),
    names_to  = "category",
    values_to = "cover_pct"
  )

# =============================================================================
# 3. FISH BIOMASS DATA
# =============================================================================
# Enter mean biomass (kg/ha) and SE per site

fish_data <- tribble(
  ~site,         ~region,        ~mma,   ~biomass, ~se,
  "Fukuchani",   "Unguja North",  TRUE,   312.4,    48.2,
  "Uroa",        "Unguja North",  TRUE,   198.6,    31.5,
  "Jongowe",     "Unguja North",  FALSE,  421.8,    62.4,
  "Kendwa",      "Unguja North",  FALSE,  145.2,    24.1,
  "Mnazi Bay",   "Mtwara",        TRUE,   634.1,    87.3,
  "Msanga Mkuu", "Mtwara",        FALSE,  289.5,    41.2,
  "Mgano",       "Mtwara",        FALSE,  112.3,    18.9
)

# =============================================================================
# 4. CORAL RECRUIT DATA
# =============================================================================
# Enter mean recruit density (recruits/m2) and SE per site

recruit_data <- tribble(
  ~site,         ~region,        ~mma,   ~density, ~se,
  "Fukuchani",   "Unguja North",  TRUE,   2.8,      0.42,
  "Uroa",        "Unguja North",  TRUE,   1.4,      0.28,
  "Jongowe",     "Unguja North",  FALSE,  3.6,      0.51,
  "Kendwa",      "Unguja North",  FALSE,  0.9,      0.19,
  "Mnazi Bay",   "Mtwara",        TRUE,   5.2,      0.73,
  "Msanga Mkuu", "Mtwara",        FALSE,  2.1,      0.34,
  "Mgano",       "Mtwara",        FALSE,  0.6,      0.14
)

# =============================================================================
# 5. INVERTEBRATE DATA
# =============================================================================
# Enter mean density (individuals / 100 m2) per taxon per site

invert_data <- tribble(
  ~site,         ~region,        ~mma,   ~taxon,           ~density,
  "Fukuchani",   "Unguja North",  TRUE,   "Sea Urchins",     12.4,
  "Fukuchani",   "Unguja North",  TRUE,   "Sea Cucumbers",    3.2,
  "Fukuchani",   "Unguja North",  TRUE,   "Giant Clams",      1.8,
  "Uroa",        "Unguja North",  TRUE,   "Sea Urchins",      9.1,
  "Uroa",        "Unguja North",  TRUE,   "Sea Cucumbers",    2.7,
  "Uroa",        "Unguja North",  TRUE,   "Giant Clams",      0.9,
  "Jongowe",     "Unguja North",  FALSE,  "Sea Urchins",     15.3,
  "Jongowe",     "Unguja North",  FALSE,  "Sea Cucumbers",    4.1,
  "Jongowe",     "Unguja North",  FALSE,  "Giant Clams",      2.4,
  "Kendwa",      "Unguja North",  FALSE,  "Sea Urchins",      6.8,
  "Kendwa",      "Unguja North",  FALSE,  "Sea Cucumbers",    1.2,
  "Kendwa",      "Unguja North",  FALSE,  "Giant Clams",      0.4,
  "Mnazi Bay",   "Mtwara",        TRUE,   "Sea Urchins",     18.9,
  "Mnazi Bay",   "Mtwara",        TRUE,   "Sea Cucumbers",    6.8,
  "Mnazi Bay",   "Mtwara",        TRUE,   "Giant Clams",      3.7,
  "Msanga Mkuu", "Mtwara",        FALSE,  "Sea Urchins",     11.2,
  "Msanga Mkuu", "Mtwara",        FALSE,  "Sea Cucumbers",    3.4,
  "Msanga Mkuu", "Mtwara",        FALSE,  "Giant Clams",      1.5,
  "Mgano",       "Mtwara",        FALSE,  "Sea Urchins",      5.4,
  "Mgano",       "Mtwara",        FALSE,  "Sea Cucumbers",    0.8,
  "Mgano",       "Mtwara",        FALSE,  "Giant Clams",      0.2
)

# =============================================================================
# 6. STATISTICAL TESTS: MMA vs. Non-MMA
# =============================================================================

# Fish biomass: MMA vs non-MMA
biomass_ttest <- with(fish_data, t.test(
  biomass[mma],
  biomass[!mma],
  var.equal = FALSE
))
cat("Fish biomass MMA vs non-MMA:\n")
print(biomass_ttest)

# Recruit density: MMA vs non-MMA
recruit_ttest <- with(recruit_data, t.test(
  density[mma],
  density[!mma],
  var.equal = FALSE
))
cat("\nRecruit density MMA vs non-MMA:\n")
print(recruit_ttest)

# Hard coral cover: MMA vs non-MMA
hc_ttest <- with(substrate_data, t.test(
  hard_coral[mma],
  hard_coral[!mma],
  var.equal = FALSE
))
cat("\nHard coral cover MMA vs non-MMA:\n")
print(hc_ttest)

# =============================================================================
# 7. COMPOSITE ECOLOGICAL CONDITION SCORE
# =============================================================================
# Normalise each metric to 0-100 and average for a composite score
# Adjust weights as appropriate for your specific reporting context

score_df <- site_info |>
  left_join(
    substrate_data |> select(site, hard_coral),
    by = "site"
  ) |>
  left_join(
    fish_data |> select(site, biomass),
    by = "site"
  ) |>
  left_join(
    recruit_data |> select(site, density),
    by = "site"
  ) |>
  left_join(
    invert_data |>
      filter(taxon == "Sea Urchins") |>
      select(site, urchin_density = density),
    by = "site"
  ) |>
  mutate(
    # Normalise 0-100 (min-max scaling)
    hc_score      = (hard_coral - min(hard_coral))    / (max(hard_coral) - min(hard_coral))    * 100,
    fish_score    = (biomass - min(biomass))          / (max(biomass) - min(biomass))          * 100,
    recruit_score = (density - min(density))          / (max(density) - min(density))          * 100,
    urchin_score  = (urchin_density - min(urchin_density)) / (max(urchin_density) - min(urchin_density)) * 100,
    # Equal-weight composite
    eco_score     = (hc_score + fish_score + recruit_score + urchin_score) / 4
  )

score_df |>
  select(site, region, mma, eco_score) |>
  arrange(desc(eco_score))

# =============================================================================
# 8. EXPORT CLEANED DATA (optional)
# =============================================================================

# write_csv(substrate_data,  "data/substrate_data.csv")
# write_csv(fish_data,       "data/fish_data.csv")
# write_csv(recruit_data,    "data/recruit_data.csv")
# write_csv(invert_data,     "data/invert_data.csv")
# write_csv(score_df,        "data/composite_scores.csv")
