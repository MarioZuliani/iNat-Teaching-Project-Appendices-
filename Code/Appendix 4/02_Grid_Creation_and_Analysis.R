# ============================================================
# 02_Grid_Creation_and_Analysis
# Appendix 4 - Script 2
# Grid analysis and range-size analysis of spotted lanternfly
# ============================================================

# This script:
#   1. Reads the cleaned spotted lanternfly iNaturalist dataset.
#   2. Converts observations into spatial points.
#   3. Creates a 50-km grid across the Eastern United States.
#   4. Counts occupied grid cells by year.
#   5. Estimates annual observed range size using convex hull area.
#   6. Creates maps and figures for Appendix 4.
#
# Input:
#   Outputs/Appendix 4/Clean Data/spotted_lanternfly_clean.rds
#
# Outputs:
#   Outputs/Appendix 4/Figures
#   Outputs/Appendix 4/Tables
#   Outputs/Appendix 4/Models
# ============================================================


# ---- 1. Packages ----

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(broom)
library(tigris)

options(tigris_use_cache = TRUE)


# ---- 2. File paths ----

clean_file <- "Outputs/Appendix 4/Clean Data/spotted_lanternfly_clean.rds"

fig_dir   <- "Outputs/Appendix 4/Figures"
table_dir <- "Outputs/Appendix 4/Tables"
model_dir <- "Outputs/Appendix 4/Models"

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 3. User settings ----

# Grid cell size in meters.
# 50,000 m = 50 km grid cells.
grid_size_m <- 50000

# Eastern United States states used for this case study.
# This keeps the study area easy to explain in a teaching example.
eastern_states <- c(
  "AL", "CT", "DE", "FL", "GA", "IL", "IN", "KY", "MA", "MD",
  "ME", "MI", "MS", "NC", "NH", "NJ", "NY", "OH", "PA", "RI",
  "SC", "TN", "VA", "VT", "WV", "WI", "DC"
)


# ---- 4. Read cleaned data ----

lanternfly <- readRDS(clean_file)

message("Number of cleaned observations: ", nrow(lanternfly))
message(
  "Years included: ",
  paste(sort(unique(lanternfly$year)), collapse = ", ")
)


# ---- 5. Convert observations to spatial points ----

# Longitude/latitude version for mapping.
lanternfly_sf <- st_as_sf(
  lanternfly,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# Projected version for grid and area calculations.
# EPSG 5070 = NAD83 / Conus Albers, useful for U.S. area calculations.
lanternfly_sf_5070 <- st_transform(lanternfly_sf, 5070)


# ---- 6. Get Eastern United States state boundaries ----

us_states <- states(cb = TRUE, year = 2023, class = "sf") %>%
  st_transform(4326)

states_bg <- us_states %>%
  filter(STUSPS %in% eastern_states)

states_bg_5070 <- states_bg %>%
  st_transform(5070)


# ---- 7. Keep only observations within the selected Eastern U.S. states ----

# This ensures that all analyses and maps match the stated study region.
lanternfly_sf <- st_join(
  lanternfly_sf,
  states_bg %>% select(STUSPS),
  left = FALSE
)

lanternfly_sf_5070 <- st_transform(lanternfly_sf, 5070)

message("Observations retained inside selected Eastern U.S. states: ", nrow(lanternfly_sf))


# ---- 8. Create spatial grid ----

# Combine Eastern U.S. state polygons into one study area.
study_area <- st_union(states_bg_5070)

# Create a 50-km grid over the study area and clip it to the state boundaries.
grid <- st_make_grid(
  study_area,
  cellsize = grid_size_m,
  square = TRUE
) %>%
  st_as_sf() %>%
  st_intersection(study_area) %>%
  mutate(grid_id = row_number())


# ---- 9. Assign observations to grid cells ----

lanternfly_grid <- st_join(
  lanternfly_sf_5070,
  grid,
  join = st_within
)


# ---- 10. Annual occupied grid cells ----

occupied_by_year <- lanternfly_grid %>%
  st_drop_geometry() %>%
  filter(!is.na(grid_id)) %>%
  distinct(year, grid_id) %>%
  count(year, name = "occupied_grid_cells") %>%
  arrange(year)

print(occupied_by_year)


# ---- 11. Annual observation counts ----

observations_by_year <- lanternfly_sf %>%
  st_drop_geometry() %>%
  count(year, name = "n_observations") %>%
  arrange(year)


# ---- 12. Annual occupied states ----

occupied_states <- lanternfly_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(STUSPS)) %>%
  distinct(year, STUSPS) %>%
  count(year, name = "occupied_states") %>%
  arrange(year)


# ---- 13. Annual observed range size using convex hull area ----

# Convex hull area is a simple estimate of annual observed range size.
# It should be interpreted cautiously because it is sensitive to outliers.

hull_by_year <- lanternfly_sf_5070 %>%
  group_by(year) %>%
  summarise(
    n_observations = n(),
    .groups = "drop"
  ) %>%
  st_convex_hull() %>%
  mutate(
    hull_area_km2 = as.numeric(st_area(geometry)) / 1e6,
    hull_area_1000_km2 = hull_area_km2 / 1000
  )


# ---- 14. Combine annual summaries ----

range_summary <- observations_by_year %>%
  left_join(occupied_by_year, by = "year") %>%
  left_join(occupied_states, by = "year") %>%
  left_join(
    hull_by_year %>%
      st_drop_geometry() %>%
      select(year, hull_area_km2, hull_area_1000_km2),
    by = "year"
  ) %>%
  arrange(year)

print(range_summary)


# ---- 15. Cumulative occupied grid cells ----

first_year_by_grid <- lanternfly_grid %>%
  st_drop_geometry() %>%
  filter(!is.na(grid_id)) %>%
  group_by(grid_id) %>%
  summarise(
    first_year_observed = min(year),
    .groups = "drop"
  )

cumulative_occupied <- first_year_by_grid %>%
  count(first_year_observed, name = "new_grid_cells") %>%
  arrange(first_year_observed) %>%
  mutate(
    cumulative_grid_cells = cumsum(new_grid_cells)
  ) %>%
  rename(year = first_year_observed)

print(cumulative_occupied)


# ---- 16. Save tables ----

write_csv(
  range_summary,
  file.path(table_dir, "range_summary_by_year.csv")
)

write_csv(
  occupied_by_year,
  file.path(table_dir, "occupied_grid_cells_by_year.csv")
)

write_csv(
  cumulative_occupied,
  file.path(table_dir, "cumulative_occupied_grid_cells.csv")
)

write_csv(
  first_year_by_grid,
  file.path(table_dir, "first_year_observed_by_grid_cell.csv")
)


# ---- 17. Models ----

# Model 1: annual occupied grid cells through time.
grid_model <- lm(occupied_grid_cells ~ year, data = range_summary)

# Model 2: annual convex hull range size through time.
range_model <- lm(hull_area_km2 ~ year, data = range_summary)

# Save model outputs.
write_csv(
  tidy(grid_model),
  file.path(model_dir, "occupied_grid_cells_model_coefficients.csv")
)

write_csv(
  glance(grid_model),
  file.path(model_dir, "occupied_grid_cells_model_fit.csv")
)

write_csv(
  tidy(range_model),
  file.path(model_dir, "range_size_model_coefficients.csv")
)

write_csv(
  glance(range_model),
  file.path(model_dir, "range_size_model_fit.csv")
)

# Print model summaries to console.
message("\nOccupied grid cell model:")
print(summary(grid_model))

message("\nRange size model:")
print(summary(range_model))


# ---- 18. Shared map limits ----

map_limits <- st_bbox(states_bg)


# ---- 19. Figure 1: occupied grid cells by year ----

fig1 <- ggplot(range_summary, aes(x = year, y = occupied_grid_cells)) +
  geom_point(size = 3.2) +
  geom_line(linewidth = 1.1) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    color = "grey35"
  ) +
  scale_x_continuous(breaks = range_summary$year) +
  labs(
    x = "Year",
    y = "Occupied grid cells",
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(fig_dir, "figure_1_occupied_grid_cells_by_year.png"),
  plot = fig1,
  width = 8,
  height = 5,
  dpi = 300
)


# ---- 20. Figure 2: annual observed range size ----

fig2 <- ggplot(range_summary, aes(x = year, y = hull_area_1000_km2)) +
  geom_point(size = 3.2) +
  geom_line(linewidth = 1.1) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    color = "grey35"
  ) +
  scale_x_continuous(breaks = range_summary$year) +
  labs(
    x = "Year",
    y = expression("Convex hull area (1,000 km"^2*")"),
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(fig_dir, "figure_2_range_size_by_year.png"),
  plot = fig2,
  width = 8,
  height = 5,
  dpi = 300
)


# ---- 21. Figure 3: first-observed grid cells with geography ----

first_year_grid <- grid %>%
  left_join(first_year_by_grid, by = "grid_id") %>%
  filter(!is.na(first_year_observed)) %>%
  st_transform(4326)

fig3 <- ggplot() +
  geom_sf(
    data = states_bg,
    fill = "grey95",
    color = "grey60",
    linewidth = 0.25
  ) +
  geom_sf(
    data = first_year_grid,
    aes(fill = factor(first_year_observed)),
    color = "white",
    linewidth = 0.05,
    alpha = 0.9
  ) +
  coord_sf(
    xlim = c(map_limits["xmin"], map_limits["xmax"]),
    ylim = c(map_limits["ymin"], map_limits["ymax"])
  ) +
  scale_fill_viridis_d(
    option = "turbo",
    direction = -1
  ) +
  labs(
    fill = "First year observed",
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(fig_dir, "figure_3_first_observed_grid_cells.png"),
  plot = fig3,
  width = 10,
  height = 7,
  dpi = 300
)


# ---- 22. Figure 4: annual observation maps with geography ----

fig4 <- ggplot() +
  geom_sf(
    data = states_bg,
    fill = "grey95",
    color = "grey60",
    linewidth = 0.2
  ) +
  geom_sf(
    data = lanternfly_sf,
    color = "#d95f02",
    alpha = 0.55,
    size = 0.3
  ) +
  coord_sf(
    xlim = c(map_limits["xmin"], map_limits["xmax"]),
    ylim = c(map_limits["ymin"], map_limits["ymax"])
  ) +
  facet_wrap(~year, ncol = 4) +
  labs(
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

ggsave(
  filename = file.path(fig_dir, "figure_4_annual_observation_maps.png"),
  plot = fig4,
  width = 12,
  height = 10,
  dpi = 300
)


# ---- 23. Figure 5: cumulative occupied grid cells ----

fig5 <- ggplot(cumulative_occupied, aes(x = year, y = cumulative_grid_cells)) +
  geom_point(size = 3.2) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(breaks = cumulative_occupied$year) +
  labs(
    x = "Year",
    y = "Cumulative occupied grid cells",
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(fig_dir, "figure_5_cumulative_occupied_grid_cells.png"),
  plot = fig5,
  width = 8,
  height = 5,
  dpi = 300
)


# ---- 24. Finish ----

message("Figures saved to: ", fig_dir)
message("Tables saved to: ", table_dir)
message("Models saved to: ", model_dir)