# ============================================================
# 01_clean_spotted_lanternfly_data
# Appendix 4
# Clean spotted lanternfly iNaturalist data
# ============================================================

library(readr)
library(dplyr)
library(lubridate)

# ---- File paths ----

raw_file <- "Data/Appendix 4/Appendix 4 Data.csv"

clean_data_dir <- "Outputs/Appendix 4/Clean Data"

dir.create(clean_data_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Read data ----

raw_data <- read_csv(raw_file, show_col_types = FALSE)

# ---- Clean data ----

clean_data <- raw_data %>%
  mutate(
    observed_on = as.Date(observed_on),
    year = year(observed_on),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(
    scientific_name == "Lycorma delicatula",
    quality_grade == "research",
    year >= 2015,
    year <= 2025,
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  select(
    id,
    observed_on,
    year,
    scientific_name,
    common_name,
    quality_grade,
    latitude,
    longitude,
    place_guess,
    url
  )

# ---- Check yearly observation counts ----

annual_counts <- clean_data %>%
  count(year, name = "n_observations")

print(annual_counts)

# ---- Save cleaned outputs ----

write_csv(
  clean_data,
  file.path(clean_data_dir, "spotted_lanternfly_clean.csv")
)

saveRDS(
  clean_data,
  file.path(clean_data_dir, "spotted_lanternfly_clean.rds")
)

write_csv(
  annual_counts,
  file.path(clean_data_dir, "annual_observation_counts.csv")
)

# ---- Print output locations ----

message("Clean data saved to: ", file.path(clean_data_dir, "spotted_lanternfly_clean.csv"))
message("Clean RDS saved to: ", file.path(clean_data_dir, "spotted_lanternfly_clean.rds"))
message("Annual counts saved to: ", file.path(clean_data_dir, "annual_observation_counts.csv"))