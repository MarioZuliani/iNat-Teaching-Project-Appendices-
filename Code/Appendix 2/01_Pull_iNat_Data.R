# ------------------------------------------------------------
# Download Research Grade Tidy Tips observations from iNaturalist
# and add a Plant Phenology annotation column
# ------------------------------------------------------------

# Install packages once if needed:
# install.packages(c("jsonlite", "dplyr", "purrr", "tidyr", "readr"))

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

# ------------------------------------------------------------
# 1. Set search parameters
# ------------------------------------------------------------

taxon_id <- 50876        # Tidy Tips / Layia platyglossa
place_id <- 51727        # Southern California, CA, US

date_start <- "2021-01-01"
date_end <- "2025-12-31"

per_page <- 200

# Output file location
# These are where the outputted data and other documents will be found after we run the code.
output_folder <- "Data/Appendix 2"
output_file <- file.path(output_folder, "tidy_tips_socal_with_annotation.csv")

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# iNaturalist Plant Phenology annotation codes
# These are the codes necessary to get the plaant phenology (Flower Presence) from iNaturalist.
plant_phenology <- 12

phenology_codes <- data.frame(
  term_value_id = c(13, 14, 15, 21),
  annotation = c(
    "Flowering",
    "Fruiting",
    "Flower Budding",
    "No Evidence of Flowering"
  )
)

# ------------------------------------------------------------
# 2. Function to download observations from iNaturalist
# This function will scan iNaturalist based on the criteria outlined
# in the Appendix (ie: Research Grade, our time frame, and location).
# ------------------------------------------------------------

download_inat <- function(term_value_id = NULL) {
  
  url <- paste0(
    "https://api.inaturalist.org/v1/observations?",
    "taxon_id=", taxon_id,
    "&place_id=", place_id,
    "&quality_grade=research",
    "&d1=", date_start,
    "&d2=", date_end,
    "&verifiable=any",
    "&per_page=", per_page
  )
  
  if (!is.null(term_value_id)) {
    url <- paste0(
      url,
      "&term_id=", plant_phenology,
      "&term_value_id=", term_value_id
    )
  }
  
  first_page <- fromJSON(paste0(url, "&page=1"), flatten = TRUE)
  
  total_pages <- ceiling(first_page$total_results / per_page)
  
  all_pages <- map(
    1:total_pages,
    ~ fromJSON(paste0(url, "&page=", .x), flatten = TRUE)$results
  )
  
  bind_rows(all_pages)
}

# ------------------------------------------------------------
# 3. Download the main observation dataset
# This code will download all of the Tidy Tips data.
# ------------------------------------------------------------

tidy_tips <- download_inat()

# Keep only useful columns for the classroom example
tidy_tips <- tidy_tips %>%
  transmute(
    id = id,
    url = uri,
    observed_on = observed_on,
    quality_grade = quality_grade,
    scientific_name = taxon.name,
    common_name = taxon.preferred_common_name,
    user_login = user.login,
    place_guess = place_guess
  )

# ------------------------------------------------------------
# 4. Download Plant Phenology annotation groups
# This will download our annotations data that will let us see if Flowers are
# present in the image (based on our annotations)
# ------------------------------------------------------------

annotation_lookup <- phenology_codes %>%
  mutate(data = map(term_value_id, download_inat)) %>%
  select(annotation, data) %>%
  unnest(data) %>%
  select(id, annotation) %>%
  distinct(id, .keep_all = TRUE)

# ------------------------------------------------------------
# 5. Add annotation columns to the main dataset
# This will combine the Flower Presence annotation to the main document.
# ------------------------------------------------------------

tidy_tips <- tidy_tips %>%
  left_join(annotation_lookup, by = "id") %>%
  mutate(
    flowers_present = case_when(
      annotation %in% c("Flowering", "Flower Budding") ~ "Yes",
      annotation %in% c("Fruiting", "No Evidence of Flowering") ~ "No",
      is.na(annotation) ~ NA_character_
    )
  )

# ------------------------------------------------------------
# 6. Check and save the final dataset
# Final checks to make sure everything has worked and save the files.
# ------------------------------------------------------------

tidy_tips %>%
  count(annotation, sort = TRUE)

tidy_tips %>%
  count(flowers_present, sort = TRUE)

write_csv(tidy_tips, output_file)