# ============================================================
# Check Point 4 - NHANES Vision and Eye Health Surveillance
# Author: Wen Liu
# ============================================================

library(tidyverse)

nhanes_eye <- read_csv(
  "National_Health_and_Nutrition_Examination_Survey_(NHANES)_–_Vision_and_Eye_Health_Surveillance_20260130.csv"
)

# Data description table
data_description <- data.frame(
  Variable = c("Sex", "Data Value", "Question", "Response", "Sample Size"),
  Type     = c("Categorical", "Continuous", "Categorical", "Categorical", "Discrete"),
  R_Class  = c(class(nhanes_eye$Sex),
               class(nhanes_eye$Data_Value),
               class(nhanes_eye$Question),
               class(nhanes_eye$Response),
               class(nhanes_eye$Sample_Size)),
  Meaning  = c("Male or female group",
               "Estimated prevalence (%)",
               "Survey question measured",
               "Response category",
               "Group sample size")
)

knitr::kable(data_description,
             caption = "Description of key variables used in the analysis",
             align = c("l", "c", "c", "l"))

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
  summarise(Mean_Value = mean(Data_Value),
            SD_Value   = sd(Data_Value),
            n          = n(),
            .groups    = "drop")

# Figure 1: Bar plot of mean prevalence by sex
plot_df <- analysis_df %>%
  group_by(Sex) %>%
  summarise(Mean_Value = mean(Data_Value),
            SD_Value   = sd(Data_Value),
            n          = n(),
            SE         = SD_Value / sqrt(n),
            .groups    = "drop")

ggplot(plot_df, aes(x = Sex, y = Mean_Value, fill = Sex)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Mean_Value, 2)), vjust = -0.5) +
  geom_errorbar(aes(ymin = Mean_Value - SE, ymax = Mean_Value + SE), width = 0.2) +
  labs(x        = "Sex",
       y        = "Mean Data_Value (± SE)",
       title    = "Mean Prevalence (Data_Value) by Sex",
       subtitle = "The prevalence of any vision loss appears to differ between males and females") +
  theme_minimal() +
  theme(legend.position = "none")

# Parameters table
parameters <- data.frame(
  Parameter   = c("Baseline mean (males)", "Effect size", "Noise level", "Sample size per group"),
  Symbol      = c("mu0", "delta", "sigma", "n"),
  Description = c("Mean prevalence among males",
                  "Female mean minus Male mean",
                  "Standard deviation of random noise in simulation",
                  "Number of observations per sex")
)
knitr::kable(parameters, caption = "Parameters used in the simulation")

# Get male mean and pooled SD from data
male_mean <- analysis_df %>%
  filter(Sex == "Male") %>%
  summarise(m = mean(Data_Value)) %>%
  pull(m)

pooled_sd <- analysis_df %>%
  summarise(s = sd(Data_Value)) %>%
  pull(s)

# Single simulation example
effect_size <- 0.5
noise_sd    <- pooled_sd
n_per_group <- 100
mu0         <- male_mean

sim_data <- tibble(
  Sex = rep(c("Male", "Female"), each = n_per_group),
  Data_Value = c(
    rnorm(n_per_group, mean = mu0, sd = noise_sd),
    rnorm(n_per_group, mean = mu0 + effect_size, sd = noise_sd)
  )
)

summary_stats <- sim_data %>%
  group_by(Sex) %>%
  summarise(n      = n(),
            mean   = mean(Data_Value),
            sd     = sd(Data_Value),
            q1     = quantile(Data_Value, 0.25),
            median = quantile(Data_Value, 0.50),
            q3     = quantile(Data_Value, 0.75),
            .groups = "drop")

knitr::kable(summary_stats,
             caption = "Stratified summary statistics of simulated Data Value by Sex")

# Simulation function
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
    summarise(n      = n(),
              mean   = mean(Data_Value),
              sd     = sd(Data_Value),
              q1     = quantile(Data_Value, 0.25),
              median = quantile(Data_Value, 0.50),
              q3     = quantile(Data_Value, 0.75),
              .groups = "drop")
  female_mean          <- summary_stats$mean[summary_stats$Sex == "Female"]
  male_mean_sim        <- summary_stats$mean[summary_stats$Sex == "Male"]
  estimated_difference <- female_mean - male_mean_sim
  list(data = sim_data, summary_stats = summary_stats,
       estimated_difference = estimated_difference)
}

# Simulation automation
effect_sizes <- seq(0, 1, length.out = 10)
sample_sizes <- round(seq(50, 500, length.out = 10))
noise_levels <- pooled_sd * c(0.75, 1, 1.5)
n_reps       <- 100

simulation_results <- list()
counter <- 1

for (e in effect_sizes) {
  for (n in sample_sizes) {
    for (noise in noise_levels) {
      for (rep in 1:n_reps) {
        res <- run_simulation(effect_size = e, noise_sd = noise,
                              n_per_group = n, mu0 = male_mean)
        simulation_results[[counter]] <- list(
          effect_size          = e,
          n_per_group          = n,
          noise_sd             = noise,
          replicate            = rep,
          estimated_difference = res$estimated_difference
        )
        counter <- counter + 1
      }
    }
  }
}

# Build summary data frame
sim_summary <- purrr::map_dfr(simulation_results, ~ tibble(
  effect_size          = .x$effect_size,
  n_per_group          = .x$n_per_group,
  noise_sd             = .x$noise_sd,
  replicate            = .x$replicate,
  estimated_difference = .x$estimated_difference
))

sim_summary_grouped <- sim_summary %>%
  group_by(effect_size, n_per_group, noise_sd) %>%
  summarize(mean_estimated_difference = mean(estimated_difference),
            sd_estimated_difference   = sd(estimated_difference),
            .groups = "drop") %>%
  mutate(effect_size = round(effect_size, 2),
         noise_lab = case_when(
           noise_sd == min(noise_levels)    ~ "Low noise",
           noise_sd == median(noise_levels) ~ "Medium noise",
           TRUE                             ~ "High noise"
         ))

# Figure 2: Simulation results
ggplot(sim_summary_grouped,
       aes(x = n_per_group, y = mean_estimated_difference,
           color = noise_lab, group = noise_lab)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ effect_size, ncol = 5) +
  labs(title  = "Mean estimated difference across repeated simulations",
       x      = "Sample size per group",
       y      = "Mean estimated difference (Female - Male)",
       color  = "Noise level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))