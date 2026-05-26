library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Read field data
field <- read_csv("field.csv", col_types = cols(
  Site = col_character(),
  Date = col_date(),
  Observer = col_character(),
  TransectID = col_character(),
  PositionStart_m = col_double(),
  PositionEnd_m = col_double(),
  InterceptLength_m = col_double(),
  SubstrateCode = col_character(),
  SpeciesCode = col_character(),
  InPark = col_logical(),
  Notes = col_character()
))

# Ensure InterceptLength is calculated
field <- field |>
  mutate(
    InterceptLength_m = ifelse(is.na(InterceptLength_m),
                               PositionEnd_m - PositionStart_m,
                               InterceptLength_m)
  )

# Calculate transect lengths
transect_lengths <- field |>
  group_by(Site, TransectID, InPark) |>
  summarize(
    TransectLength_m = max(PositionEnd_m, na.rm = TRUE) - min(PositionStart_m, na.rm = TRUE),
    .groups = "drop"
  )

# Hard coral cover by transect
coral_cover <- field |>
  filter(SubstrateCode == "HC") |>
  group_by(Site, TransectID, InPark) |>
  summarize(HardCoral_m = sum(InterceptLength_m, na.rm = TRUE), .groups = "drop") |>
  left_join(transect_lengths, by = c("Site", "TransectID", "InPark")) |>
  mutate(HardCoral_pct = 100 * HardCoral_m / TransectLength_m) |>
  replace_na(list(HardCoral_m = 0, HardCoral_pct = 0))

# Algae cover by transect (restoration indicator—high algae = priority)
algae_cover <- field |>
  filter(SubstrateCode == "AC") |>
  group_by(Site, TransectID, InPark) |>
  summarize(Algae_m = sum(InterceptLength_m, na.rm = TRUE), .groups = "drop") |>
  left_join(transect_lengths, by = c("Site", "TransectID", "InPark")) |>
  mutate(Algae_pct = 100 * Algae_m / TransectLength_m)

# Bare substrate (sand + rubble—suitable for restoration)
bare_cover <- field |>
  filter(SubstrateCode %in% c("SD", "RU")) |>
  group_by(Site, TransectID, InPark) |>
  summarize(Bare_m = sum(InterceptLength_m, na.rm = TRUE), .groups = "drop") |>
  left_join(transect_lengths, by = c("Site", "TransectID", "InPark")) |>
  mutate(Bare_pct = 100 * Bare_m / TransectLength_m)

# Restoration priority index: combine algae cover + bare substrate
restoration_index <- coral_cover |>
  select(Site, TransectID, InPark, HardCoral_pct) |>
  left_join(algae_cover |> select(Site, TransectID, Algae_pct), 
            by = c("Site", "TransectID")) |>
  left_join(bare_cover |> select(Site, TransectID, Bare_pct), 
            by = c("Site", "TransectID")) |>
  replace_na(list(Algae_pct = 0, Bare_pct = 0)) |>
  mutate(
    RestorationPriority = Algae_pct + Bare_pct,  # Higher = more urgent
    ParkStatus = ifelse(InPark, "Inside Park", "Outside Park")
  )

# Summary by park status
summary_by_park <- restoration_index |>
  group_by(ParkStatus) |>
  summarize(
    n_transects = n(),
    HardCoral_pct_mean = mean(HardCoral_pct, na.rm = TRUE),
    HardCoral_pct_sd = sd(HardCoral_pct, na.rm = TRUE),
    Algae_pct_mean = mean(Algae_pct, na.rm = TRUE),
    Bare_pct_mean = mean(Bare_pct, na.rm = TRUE),
    RestorationPriority_mean = mean(RestorationPriority, na.rm = TRUE),
    .groups = "drop"
  )

# Add transect lengths for area calculation
restoration_index <- restoration_index |>
  left_join(transect_lengths |> select(Site, TransectID, TransectLength_m),
            by = c("Site", "TransectID")) |>
  mutate(
    RestorableArea_m = (Algae_pct + Bare_pct) / 100 * TransectLength_m
  )

# Summary by park - total restorable area
summary_area <- restoration_index |>
  group_by(ParkStatus) |>
  summarize(
    TotalRestorableArea_m = sum(RestorableArea_m, na.rm = TRUE),
    MeanRestorableArea_per_transect_m = mean(RestorableArea_m, na.rm = TRUE),
    .groups = "drop"
  )

# Visualization: comparison of key metrics
p1 <- ggplot(restoration_index, aes(x = ParkStatus, y = HardCoral_pct, fill = ParkStatus)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Hard Coral Cover by Park Status",
       x = "", y = "Hard Coral Cover (%)") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(restoration_index, aes(x = ParkStatus, y = RestorationPriority, fill = ParkStatus)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Restoration Priority (Algae + Bare %)",
       x = "", y = "Restoration Priority (%)") +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(restoration_index, aes(x = ParkStatus, y = RestorableArea_m, fill = ParkStatus)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Restorable Area per Transect",
       x = "", y = "Area (m)") +
  theme_minimal() +
  theme(legend.position = "none")

# Print key results
list(
  summary_by_park = summary_by_park,
  summary_area = summary_area,
  restoration_index = restoration_index,
  plot_coral = p1,
  plot_priority = p2,
  plot_area = p3
)
