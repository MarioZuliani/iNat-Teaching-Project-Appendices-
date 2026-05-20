# ------------------------------------------------------------
# Analyze whether the flowering window of Tidy Tips has shifted
# over the last five years
# ------------------------------------------------------------

# Install packages once if needed:
# install.packages(c("dplyr", "readr", "lubridate", "ggplot2", "broom"))

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(broom)

# ------------------------------------------------------------
# 1. Set file locations
# ------------------------------------------------------------

data_folder <- "Data/Appendix 2"

stats_folder <- "Outputs/Appendix 2/Stats"
figures_folder <- "Outputs/Appendix 2/Figures"

dir.create(stats_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)

input_file <- file.path(data_folder, "tidy_tips_socal_with_annotation.csv")

output_summary <- file.path(stats_folder, "tidy_tips_flowering_window_by_year.csv")
output_stats <- file.path(stats_folder, "tidy_tips_flowering_window_statistics.csv")

output_window_figure <- file.path(figures_folder, "tidy_tips_main_flowering_window_by_year.png")
output_duration_figure <- file.path(figures_folder, "tidy_tips_flowering_duration_by_year.png")
output_observations_figure <- file.path(figures_folder, "tidy_tips_flowering_observations_by_year.png")

# ------------------------------------------------------------
# 2. Read the dataset
# ------------------------------------------------------------

tidy_tips <- read_csv(input_file)

# ------------------------------------------------------------
# 3. Keep only observations with visible flowers
# ------------------------------------------------------------

flowering_tidy_tips <- tidy_tips %>%
  filter(flowers_present == "Yes") %>%
  mutate(
    observation_date = as.Date(observed_on),
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

# ------------------------------------------------------------
# 4. Calculate flowering timing for each year
# ------------------------------------------------------------
# Earliest and latest flowering days are included for reference.
# The main flowering window is based on the 10th and 90th percentiles.
# This avoids letting one unusually early or late observation control
# the result.

flowering_window_by_year <- flowering_tidy_tips %>%
  group_by(year) %>%
  summarize(
    number_of_flowering_observations = n(),
    
    earliest_flowering_day = min(day_of_year),
    latest_flowering_day = max(day_of_year),
    
    flowering_start_10th_percentile = quantile(day_of_year, 0.10, na.rm = TRUE),
    flowering_end_90th_percentile = quantile(day_of_year, 0.90, na.rm = TRUE),
    
    median_flowering_day = median(day_of_year, na.rm = TRUE),
    
    flowering_duration_days = flowering_end_90th_percentile - 
      flowering_start_10th_percentile + 1,
    
    .groups = "drop"
  )

flowering_window_by_year

write_csv(flowering_window_by_year, output_summary)

# ------------------------------------------------------------
# 5. Statistical tests
# ------------------------------------------------------------
# These simple linear models test whether flowering timing changes
# across years.
#
# The slope tells us the direction and size of the change:
#   Negative slope = flowering is getting earlier
#   Positive slope = flowering is getting later
#
# Important note:
# There are only five years of data, so these tests should be treated
# as a classroom example rather than strong evidence of a long-term trend.

start_model <- lm(
  flowering_start_10th_percentile ~ year,
  data = flowering_window_by_year
)

end_model <- lm(
  flowering_end_90th_percentile ~ year,
  data = flowering_window_by_year
)

duration_model <- lm(
  flowering_duration_days ~ year,
  data = flowering_window_by_year
)

median_model <- lm(
  median_flowering_day ~ year,
  data = flowering_window_by_year
)

flowering_window_statistics <- bind_rows(
  tidy(start_model) %>%
    mutate(response_variable = "Start of main flowering window"),
  
  tidy(end_model) %>%
    mutate(response_variable = "End of main flowering window"),
  
  tidy(duration_model) %>%
    mutate(response_variable = "Flowering duration"),
  
  tidy(median_model) %>%
    mutate(response_variable = "Median flowering day")
) %>%
  filter(term == "year") %>%
  transmute(
    response_variable = response_variable,
    slope_days_per_year = estimate,
    p_value = p.value
  )

flowering_window_statistics

write_csv(flowering_window_statistics, output_stats)

# ------------------------------------------------------------
# 6. Figure 1: Main flowering window by year
# ------------------------------------------------------------
# Each horizontal line shows the main flowering window for a year.
# The line starts at the 10th percentile and ends at the 90th percentile.
# The dot shows the median flowering day.

window_plot <- ggplot(
  flowering_window_by_year,
  aes(y = factor(year))
) +
  geom_segment(
    aes(
      x = flowering_start_10th_percentile,
      xend = flowering_end_90th_percentile,
      yend = factor(year)
    ),
    linewidth = 2
  ) +
  geom_point(
    aes(x = median_flowering_day),
    size = 3
  ) +
  labs(
    x = "Day of Year",
    y = "Year",
  ) +
  theme_classic()

window_plot

ggsave(
  filename = output_window_figure,
  plot = window_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 7. Figure 2: Flowering duration by year
# ------------------------------------------------------------
# Flowering duration is calculated as:
# 90th percentile flowering day - 10th percentile flowering day + 1

duration_plot <- ggplot(
  flowering_window_by_year,
  aes(x = factor(year), y = flowering_duration_days)
) +
  geom_col() +
  geom_text(
    aes(label = round(flowering_duration_days, 1)),
    vjust = -0.5
  ) +
  labs(
    x = "Year",
    y = "Flowering Duration in Days",
  ) +
  theme_classic()

duration_plot

ggsave(
  filename = output_duration_figure,
  plot = duration_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 8. Figure 3: Distribution of flowering observations by year
# ------------------------------------------------------------
# This figure shows all flowering observations, not only the summary
# statistics. It helps show whether patterns are based on many records
# or only a few early or late observations.

observations_plot <- ggplot(
  flowering_tidy_tips,
  aes(x = factor(year), y = day_of_year)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  labs(
    x = "Year",
    y = "Day of Year",
  ) +
  theme_classic()

observations_plot

ggsave(
  filename = output_observations_figure,
  plot = observations_plot,
  width = 8,
  height = 5,
  dpi = 300
)