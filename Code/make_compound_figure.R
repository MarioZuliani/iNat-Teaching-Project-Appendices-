# ============================================================
# Separate figure panels using screened-literature dataset
#
# Panel A = Publication-year histogram
# Panel C = Manuscript-level pathways through the
#           observation-to-inference framework
#
# Output:
# Figure1A_Publication_Year.pdf
# Figure1C_Framework_Pathways.pdf
# ============================================================


# ------------------------------------------------------------
# Load packages
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)


# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------

data_dir <- "Data"
literature_filename <- "Sample_Literature.csv"


# ------------------------------------------------------------
# Read literature dataset
# ------------------------------------------------------------

dat <- read.csv(
  file.path(
    data_dir,
    literature_filename
  ),
  fileEncoding = "latin1",
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# Define the observation-to-inference framework
# ------------------------------------------------------------

stage_key <- tribble(
  ~stage_order, ~variable,                              ~stage,
  
  1, "Observation design and data generation",
  "Observation design\n& data generation",
  
  2, "Annotation and metadata development",
  "Annotation & metadata\ndevelopment",
  
  3, "Data access and filtering",
  "Data access\n& filtering",
  
  4, "Question formulation",
  "Question\nformulation",
  
  5, "Analysis and interpretation",
  "Analysis &\ninterpretation",
  
  6, "Critical evaluation of limitations",
  "Critical evaluation\nof limitations"
) |>
  arrange(stage_order)


stage_columns <- stage_key$variable


# ------------------------------------------------------------
# Prepare data for Panels A and C
# ------------------------------------------------------------

# Panel A uses all screened manuscripts with a publication year.

publication_data <- dat |>
  filter(
    !is.na(`Publication Year`)
  )


# Panel C uses only manuscripts classified as relevant.

relevant_dat <- dat |>
  filter(
    `Relevant?` == "Yes"
  )


# ------------------------------------------------------------
# Quality-control checks
# ------------------------------------------------------------

# Confirm that there are exactly 16 relevant manuscripts.

if (nrow(relevant_dat) != 16) {
  stop(
    "Expected 16 relevant manuscripts, but found ",
    nrow(relevant_dat),
    "."
  )
}


# Expected totals for the six framework stages:
#
# Observation design and data generation = 15
# Annotation and metadata development = 16
# Data access and filtering = 5
# Question formulation = 2
# Analysis and interpretation = 6
# Critical evaluation of limitations = 2

expected_counts <- c(
  15,
  16,
  5,
  2,
  6,
  2
)

names(expected_counts) <- stage_columns


actual_counts <- vapply(
  relevant_dat[stage_columns],
  function(x) {
    sum(
      x == "Yes",
      na.rm = TRUE
    )
  },
  numeric(1)
)


# Stop if the coding no longer reproduces the expected totals.

if (!identical(
  unname(actual_counts),
  unname(expected_counts)
)) {
  stop(
    "Stage totals do not match expected values. Expected: ",
    paste(
      expected_counts,
      collapse = ", "
    ),
    " | Found: ",
    paste(
      actual_counts,
      collapse = ", "
    )
  )
}


# Print totals for manual confirmation.

print(actual_counts)


# ------------------------------------------------------------
# Create manuscript-level coding table
# ------------------------------------------------------------

coding <- relevant_dat |>
  transmute(
    
    manuscript_id = Key,
    
    manuscript_label = paste0(
      trimws(
        sub(
          ",.*$",
          "",
          Author
        )
      ),
      " (",
      `Publication Year`,
      ")"
    ),
    
    across(
      all_of(stage_columns),
      ~ as.integer(.x == "Yes")
    )
  )


# Make labels unique if two manuscripts have the same
# first-author/year combination.

coding$manuscript_label <- make.unique(
  coding$manuscript_label
)


# ------------------------------------------------------------
# Convert coding data to long format
# ------------------------------------------------------------

long <- coding |>
  pivot_longer(
    cols = all_of(stage_columns),
    names_to = "variable",
    values_to = "included"
  ) |>
  left_join(
    stage_key,
    by = "variable"
  )


# ------------------------------------------------------------
# Order manuscripts in Panel C
# ------------------------------------------------------------

# Manuscripts are ordered first by the furthest framework stage
# they reach, then by the total number of stages they include.

manuscript_order <- long |>
  group_by(
    manuscript_id,
    manuscript_label
  ) |>
  summarise(
    
    reach = max(
      if_else(
        included == 1,
        stage_order,
        0L
      )
    ),
    
    breadth = sum(included),
    
    .groups = "drop"
  ) |>
  arrange(
    desc(reach),
    desc(breadth),
    manuscript_label
  )


# ------------------------------------------------------------
# Prepare plotting data
# ------------------------------------------------------------

plot_data <- long |>
  mutate(
    
    manuscript_label = factor(
      manuscript_label,
      levels = rev(
        manuscript_order$manuscript_label
      )
    ),
    
    stage = factor(
      stage,
      levels = stage_key$stage
    )
  )


# ------------------------------------------------------------
# Create connecting-line data
# ------------------------------------------------------------

# Lines are drawn only when TWO ADJACENT framework stages
# are both represented in the same manuscript.

connection_data <- plot_data |>
  arrange(
    manuscript_id,
    stage_order
  ) |>
  group_by(
    manuscript_id
  ) |>
  mutate(
    next_stage_order = lead(stage_order),
    next_included = lead(included)
  ) |>
  ungroup() |>
  filter(
    included == 1,
    next_included == 1
  )


# ------------------------------------------------------------
# Shared figure theme
# ------------------------------------------------------------

shared_theme <- theme_minimal(
  base_family = "Times New Roman",
  base_size = 10
) +
  theme(
    
    text = element_text(
      family = "Times New Roman",
      size = 10,
      color = "black"
    ),
    
    axis.text = element_text(
      family = "Times New Roman",
      size = 10,
      face = "bold",
      color = "black"
    ),
    
    axis.title = element_text(
      family = "Times New Roman",
      size = 10,
      face = "bold",
      color = "black"
    ),
    
    panel.grid.minor = element_blank()
  )


# ============================================================
# PANEL A
# Publication-year histogram
# ============================================================

panel_a <- ggplot(
  publication_data,
  aes(
    x = `Publication Year`
  )
) +
  
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    closed = "left",
    fill = "grey45",
    color = "white",
    linewidth = 0.25
  ) +
  
  scale_x_continuous(
    
    breaks = c(
      2013,
      2018,
      2023
    ),
    
    limits = c(
      2012.5,
      2023.5
    ),
    
    expand = expansion(
      mult = 0
    )
  ) +
  
  labs(
    x = "Publication year",
    y = "Count"
  ) +
  
  shared_theme +
  
  theme(
    
    panel.grid.major.x = element_blank(),
    
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold",
      color = "black"
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold",
      color = "black"
    ),
    
    axis.title.x = element_text(
      family = "Times New Roman",
      size = 20,
      face = "bold",
      color = "black"
    ),
    
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 20,
      face = "bold",
      color = "black"
    )
  )


# ============================================================
# PANEL C
# Manuscript-level framework pathways
# ============================================================

panel_c <- ggplot() +
  
  # Connect adjacent stages when both are represented.
  
  geom_segment(
    
    data = connection_data,
    
    aes(
      x = stage_order,
      xend = next_stage_order,
      y = manuscript_label,
      yend = manuscript_label
    ),
    
    linewidth = 0.8,
    color = "grey55",
    lineend = "round"
  ) +
  
  # Add a point for every framework stage represented
  # within a manuscript.
  
  geom_point(
    
    data = filter(
      plot_data,
      included == 1
    ),
    
    aes(
      x = stage_order,
      y = manuscript_label,
      fill = stage
    ),
    
    shape = 21,
    size = 3.2,
    stroke = 0.3,
    color = "white"
  ) +
  
  # Framework stages are shown in the same order used
  # throughout the manuscript.
  
  scale_x_continuous(
    
    breaks = stage_key$stage_order,
    
    labels = c(
      "1. Observation",
      "2. Metadata",
      "3. Access",
      "4. Question",
      "5. Analysis",
      "6. Limitations"
    ),
    
    limits = c(
      0.75,
      6.25
    ),
    
    expand = expansion(
      mult = 0
    )
  ) +
  
  scale_fill_viridis_d(
    option = "D",
    end = 0.9,
    guide = "none"
  ) +
  
  scale_y_discrete(
    limits = rev(
      manuscript_order$manuscript_label
    ),
    drop = FALSE
  ) +
  
  labs(
    x = "Stage in the observation-to-inference framework",
    y = NULL
  ) +
  
  shared_theme +
  
  theme(
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "grey88",
      linewidth = 0.3
    ),
    
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold",
      color = "black",
      angle = 0,
      hjust = 0.5,
      vjust = 1
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold",
      color = "black"
    ),
    
    axis.title.x = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold",
      color = "black",
      margin = margin(
        t = 6
      )
    )
  )


# ============================================================
# Export Panel A
# ============================================================

ggsave(
  filename = file.path(
    data_dir,
    "Figure1A_Publication_Year.pdf"
  ),
  plot = panel_a,
  width = 3.25,
  height = 2.6,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# ============================================================
# Export Panel C
# ============================================================

ggsave(
  filename = file.path(
    data_dir,
    "Figure1C_Framework_Pathways.pdf"
  ),
  plot = panel_c,
  width = 12,
  height = 3,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# ------------------------------------------------------------
# Display panels in RStudio
# ------------------------------------------------------------

print(panel_a)

print(panel_c)
