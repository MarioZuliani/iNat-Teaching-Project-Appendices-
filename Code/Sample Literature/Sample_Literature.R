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
    x = reorder(Category, Number),
    y = Number,
    fill = Category
  )
) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = paste0(Number, " (", round(Percent, 1), "%)")),
    hjust = -0.12,
    size = 5,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = framework_colors) +
  
  # Wrap long category names onto two lines
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  
  # Leave only enough room for the labels
  scale_y_continuous(
    breaks = 0:18,
    limits = c(0, 18),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    x = NULL,
    y = "Number of Manuscripts"
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    
    axis.title.x = element_text(
      size = 16,
      face = "bold",
      margin = margin(t = 10)
    ),
    
    axis.text.x = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 15,
      face = "bold",
      lineheight = 0.95
    ),
    
    # Controls the actual shape of the plotting panel
    aspect.ratio = 0.80,
    
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 10,
      l = 10
    )
  )
