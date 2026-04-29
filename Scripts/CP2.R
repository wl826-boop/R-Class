# ============================================================
# Check Point 2 - NHANES Vision and Eye Health Surveillance
# Author: Wen Liu
# ============================================================

library(tidyverse)
library(janitor)
library(dplyr)
library(tibble)
library(knitr)
library(ggplot2)

# Load file
nhanes_eye <- read_csv(
  "nhanes_vision.csv"
)

# Clean column names
nhanes_eye <- nhanes_eye |>
  janitor::clean_names()

# Variable overview table
vars <- c(
  "data_value",
  "low_confidence_limit",
  "high_confidence_limit",
  "age",
  "sex",
  "race_ethnicity",
  "risk_factor",
  "sample_size",
  "year_start",
  "year_end"
)

variable_overview <- tibble(variable = vars) |>
  mutate(
    class = sapply(nhanes_eye[vars], function(x) class(x)[1]),
    type = case_when(
      variable %in% c("data_value", "low_confidence_limit", "high_confidence_limit") ~ "Continuous",
      variable %in% c("sample_size", "year_start", "year_end") ~ "Discrete",
      TRUE ~ "Categorical"
    ),
    description = case_when(
      variable == "data_value"             ~ "Estimated prevalence (%)",
      variable == "low_confidence_limit"   ~ "Lower 95% confidence limit (%)",
      variable == "high_confidence_limit"  ~ "Upper 95% confidence limit (%)",
      variable == "age"                    ~ "Age group category",
      variable == "sex"                    ~ "Sex category",
      variable == "race_ethnicity"         ~ "Race/ethnicity category",
      variable == "risk_factor"            ~ "Risk factor subgroup",
      variable == "sample_size"            ~ "Sample size for estimate",
      variable == "year_start"             ~ "Start year of NHANES cycle",
      variable == "year_end"               ~ "End year of NHANES cycle",
      TRUE ~ NA_character_
    )
  ) |>
  select(variable, type, class, description)

kable(
  variable_overview,
  caption = "Selected demographic, risk factor, and prevalence variables from the CDC NHANES Vision and Eye Health Surveillance dataset."
)

# Figure 1: Histogram of prevalence estimates
ggplot(nhanes_eye, aes(x = data_value)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue4", color = "black",
                 alpha = 0.9, na.rm = TRUE) +
  geom_vline(aes(xintercept = mean(data_value, na.rm = TRUE), color = "Mean"),
             linewidth = 1.2) +
  geom_vline(aes(xintercept = median(data_value, na.rm = TRUE), color = "Median"),
             linewidth = 1.2) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Prevalence estimate (%)",
       y = "Number of subgroup estimates",
       color = "",
       caption = "Prevalence Estimates For Eye Health Conditions.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0.5, size = 12, margin = margin(t = 10)))

# Figure 2: Scatterplot of confidence limits
ggplot(nhanes_eye, aes(x = low_confidence_limit, y = high_confidence_limit)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Lower 95% confidence limit (%)",
       y = "Upper 95% confidence limit (%)",
       caption = "Lower vs Upper 95% Confidence Limits For Prevalence Estimates.") +
  theme_classic(base_size = 12) +
  theme(panel.background = element_rect(fill = "linen", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        plot.caption     = element_text(hjust = 0.5, size = 12, margin = margin(t = 10)))
