# Load packages
library(tidyverse)
library(ggplot2)
library(UpSetR)

# Read the data
dat <- read.csv(
  "Data/Sample_Literature.csv",
  fileEncoding = "latin1",
  check.names = FALSE
)

#------------------------------------------------------------
# Percentage of all manuscripts that are relevant
#------------------------------------------------------------

relevance_summary <- dat %>%
  count(`Relevant?`) %>%
  mutate(
    Percent = 100 * n / sum(n)
  )

relevance_summary



# Figure 1 Idea
ggplot(relevance_summary,
       aes(x = `Relevant?`, y = Percent)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(n, " papers\n", round(Percent, 1), "%")),
    vjust = -0.3
  ) +
  labs(
    x = NULL,
    y = "Percentage of manuscripts",
  ) +
  ylim(0, 100) +
  theme_classic()


# ==========================================
# Relevant papers by Framework
# ==========================================

# Keep only relevant papers
relevant_dat <- dat %>%
  filter(`Relevant?` == "Yes")

# Names of the six framework columns
categories <- c(
  "Observation design and data generation",
  "Annotation and metadata development",
  "Data access and filtering",
  "Question formulation",
  "Analysis and interpretation",
  "Critical evaluation of limitations"
)

# Count the number of Yes values in each category
category_summary <- relevant_dat %>%
  select(all_of(categories)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Category",
    values_to = "Included"
  ) %>%
  group_by(Category) %>%
  summarise(
    Number = sum(Included == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Percent = 100 * Number / nrow(relevant_dat),
    Category = factor(Category, levels = categories)
  )

category_summary


# Figure 2 Idea
framework_colors <- c(
  "Observation design and data generation" = "#E88A00",
  "Annotation and metadata development" = "#0A9D9D",
  "Data access and filtering" = "#147FC1",
  "Question formulation" = "#7B35A5",
  "Analysis and interpretation" = "#08AD49",
  "Critical evaluation of limitations" = "#F02F32"
)

# Plot categories in descending order
ggplot(
  category_summary,
  aes(
    x = reorder(Category, -Number),
    y = Number,
    fill = Category
  )
) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = paste0(Number, " (", round(Percent, 1), "%)")),
    hjust = -0.12,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(values = framework_colors) +
  scale_y_continuous(
    breaks = 0:nrow(relevant_dat),
    limits = c(0, nrow(relevant_dat) + 1.5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Number of Manuscripts",
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )


