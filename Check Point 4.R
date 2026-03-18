---
  title: "Check Point 4"
author: "Wen Liu"
date: "`r Sys.Date()`"
editor_options:
  chunk_output_type: console
output:
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
urlcolor: blue
---
  
  ```{r setup, include = FALSE}

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

```

# Introduction

This analysis uses data from the National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance dataset. NHANES is a nationally representative survey of the U.S. civilian, non-institutionalized population. The dataset includes demographic variables (e.g., Sex) and vision-related prevalence estimates (Data_Value) across survey cycles.

# Objective and approach

## Specific objective

This report examines whether the mean prevalence estimate (Data_Value) for a selected vision-related outcome differs between males and females. Simulations are used to evaluate how effect size, variability (noise), and sample size influence the estimated difference between sexes. 

## Data Description

```{r}
# Packages 
library(tidyverse)

# Set working directory
setwd("/Users/lynnliu/Desktop/R Data/CP4/")

nhanes_eye <- read_csv(
  "National_Health_and_Nutrition_Examination_Survey_(NHANES)_–_Vision_and_Eye_Health_Surveillance_20260130.csv"
)

# Data Table

data_description <- data.frame(
  Variable = c("Sex",
               "Data Value",
               "Question",
               "Response",
               "Sample Size"),
  Type = c("Categorical",
           "Continuous",
           "Categorical",
           "Categorical",
           "Discrete"),
  R_Class = c(class(nhanes_eye$Sex),
              class(nhanes_eye$Data_Value),
              class(nhanes_eye$Question),
              class(nhanes_eye$Response),
              class(nhanes_eye$Sample_Size)),
  Meaning = c("Male or female group",
              "Estimated prevalence (%)",
              "Survey question measured",
              "Response category",
              "Group sample size")
)

knitr::kable(
  data_description,
  caption = "Description of key variables used in the analysis",
  align = c("l", "c", "c", "l")
)
```

## Data Visualization
```{r, results='hide'}
# Keep only usable rows
df <- nhanes_eye %>%
  filter(Sex %in% c("Male", "Female")) %>%
  filter(!is.na(Data_Value)) %>%
  filter(is.finite(Data_Value))

# Select one Question + Response that has both sexes
pick <- df %>%
  group_by(Question, Response) %>%
  summarise(n_sex = n_distinct(Sex), .groups = "drop") %>%
  filter(n_sex == 2) %>%
  slice(1)

chosen_question <- pick$Question[1]
chosen_response <- pick$Response[1]

analysis_df <- df %>%
  filter(Question == chosen_question,
         Response == chosen_response)

# Summary table 
analysis_df %>%
  group_by(Sex) %>%
  summarise(
    Mean_Value = mean(Data_Value),
    SD_Value = sd(Data_Value),
    n = n(),
    .groups = "drop"
  )
```

```{r}
# Visualization
plot_df <- analysis_df %>%
  group_by(Sex) %>%
  summarise(
    Mean_Value = mean(Data_Value),
    SD_Value = sd(Data_Value),
    n = n(),
    SE = SD_Value / sqrt(n),
    .groups = "drop"
  )

ggplot(plot_df, aes(x = Sex, y = Mean_Value, fill = Sex)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Mean_Value, 2)),
            vjust = -0.5) +
  geom_errorbar(aes(ymin = Mean_Value - SE,
                    ymax = Mean_Value + SE),
                width = 0.2) +
  labs(
    x = "Sex",
    y = "Mean Data_Value (± SE)",
    title = "Mean Prevalence (Data_Value) by Sex",
    subtitle = paste0("The prevalence of any vision loss appears to differ between males and females")
  ) +
  theme_minimal() +
  theme(legend.position = "none")

```


## Plauisble Relationships

The visualization suggests that the average Data_Value may differ between males and females. This relationship can be described as a difference in group means, calculated as the mean Data_Value among females minus the mean Data_Value among males. This difference represents the effect size examined in the simulation.

## Parameters of Interest

```{r}
parameters <- data.frame(
  Parameter = c("Baseline mean (males)", "Effect size", "Noise level", "Sample size per group"),
  Symbol = c("mu0", "delta", "sigma", "n"),
  Description = c("Mean prevalence among males",
                  "Female mean minus Male mean",
                  "Standard deviation of random noise in simulation",
                  "Number of observations per sex")
)

knitr::kable(parameters, caption = "Parameters used in the simulation")
```


# Simulation

## Simulation Basis

```{r,results='hide'}
male_mean <- analysis_df %>%
  filter(Sex == "Male") %>%
  summarise(m = mean(Data_Value)) %>%
  pull(m)

pooled_sd <- analysis_df %>%
  summarise(s = sd(Data_Value)) %>%
  pull(s)
```

## Simulation Output

```{r, results='hide'}

# Example simulation parameters
effect_size <- 0.5
noise_sd <- pooled_sd
n_per_group <- 100
mu0 <- male_mean

# Generate one simulated dataset (A = Data_Value, B = Sex)
sim_data <- tibble(
  Sex = rep(c("Male", "Female"), each = n_per_group),
  Data_Value = c(
    rnorm(n_per_group, mean = mu0, sd = noise_sd),
    rnorm(n_per_group, mean = mu0 + effect_size, sd = noise_sd)
  )
)

# Stratified summary statistics data frame (quartiles of A stratified by B)
summary_stats <- sim_data %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean = mean(Data_Value),
    sd = sd(Data_Value),
    q1 = quantile(Data_Value, 0.25),
    median = quantile(Data_Value, 0.50),
    q3 = quantile(Data_Value, 0.75),
    .groups = "drop"
  )

```

```{r}
# Display the summary statistics data frame
knitr::kable(
  summary_stats,
  caption = "Stratified summary statistics of simulated Data Value by Sex"
)
```

Although the true effect size in the simulation was set to be positive, the observed female mean in this single simulated dataset was slightly lower than the observed male mean. This can occur because random sampling introduces variability, especially when sample sizes are limited and noise is present. In other words, one simulated sample may not perfectly reflect the underlying population difference even when the true effect is positive.

## Simulation Function

```{r,results='hide'}
#3.3 Simulation Function

run_simulation <- function(effect_size, noise_sd, n_per_group, mu0) {
  
  sim_data <- tibble(
    Sex = rep(c("Male", "Female"), each = n_per_group),
    Data_Value = c(
      rnorm(n_per_group, mean = mu0, sd = noise_sd),
      rnorm(n_per_group, mean = mu0 + effect_size, sd = noise_sd)
    )
  )
  
  summary_stats <- sim_data %>%
    group_by(Sex) %>%
    summarise(
      n = n(),
      mean = mean(Data_Value),
      sd = sd(Data_Value),
      q1 = quantile(Data_Value, 0.25),
      median = quantile(Data_Value, 0.50),
      q3 = quantile(Data_Value, 0.75),
      .groups = "drop"
    )
  
  female_mean <- summary_stats$mean[summary_stats$Sex == "Female"]
  male_mean   <- summary_stats$mean[summary_stats$Sex == "Male"]
  estimated_difference <- female_mean - male_mean
  
  list(
    data = sim_data,
    summary_stats = summary_stats,
    estimated_difference = estimated_difference
  )
}
```

## Simulation Automation

```{r, results='hide'}

# Define parameter values
effect_sizes <- seq(0, 1, length.out = 10)
sample_sizes <- round(seq(50, 500, length.out = 10))
noise_levels <- pooled_sd * c(0.75, 1, 1.5)

# Number of repeated simulations per parameter combination
n_reps <- 100

# Store results
simulation_results <- list()
counter <- 1

for (e in effect_sizes) {
  for (n in sample_sizes) {
    for (noise in noise_levels) {
      for (rep in 1:n_reps) {
        
        res <- run_simulation(
          effect_size = e,
          noise_sd = noise,
          n_per_group = n,
          mu0 = male_mean
        )
        
        simulation_results[[counter]] <- list(
          effect_size = e,
          n_per_group = n,
          noise_sd = noise,
          replicate = rep,
          data = res$data,
          summary_stats = res$summary_stats,
          estimated_difference = res$estimated_difference
        )
        
        counter <- counter + 1
      }
    }
  }
}
```

# Visualization

```{r,results = 'hide'}
# Build summary data frame from simulation results
sim_summary <- purrr::map_dfr(simulation_results, ~ tibble(
  effect_size = .x$effect_size,
  n_per_group = .x$n_per_group,
  noise_sd = .x$noise_sd,
  replicate = .x$replicate,
  estimated_difference = .x$estimated_difference
))

# Summarize across repeated simulations
sim_summary_grouped <- sim_summary %>%
  group_by(effect_size, n_per_group, noise_sd) %>%
  summarize(
    mean_estimated_difference = mean(estimated_difference),
    sd_estimated_difference = sd(estimated_difference),
    .groups = "drop"
  ) %>%
  mutate(
    effect_size = round(effect_size, 2),
    noise_lab = case_when(
      noise_sd == min(noise_levels) ~ "Low noise",
      noise_sd == median(noise_levels) ~ "Medium noise",
      TRUE ~ "High noise"
    )
  )
```

```{r}
ggplot(sim_summary_grouped,
       aes(x = n_per_group,
           y = mean_estimated_difference,
           color = noise_lab,
           group = noise_lab)) +
  
  geom_line() +
  geom_point(size = 2) +
  
  facet_wrap(~ effect_size, ncol = 5) +
  
  labs(
    title = "Mean estimated difference across repeated simulations",
    x = "Sample size per group",
    y = "Mean estimated difference (Female - Male)",
    color = "Noise level"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```


# Interpretation

These simulations show that larger effect sizes (greater differences in mean Data_Value between females and males) produce more stable estimated differences across repeated samples. When the true effect size is small, larger sample sizes are needed to obtain consistent estimates. Higher noise increases variability in the estimated differences, especially at smaller sample sizes.

# AI Use Disclosure Statement

This document was generated using ChatGPT to assist with understanding the assignment instructions and drafting initial R code for data subsetting, visualization, and probability calculations.