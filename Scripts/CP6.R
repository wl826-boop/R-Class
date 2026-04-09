# ============================================================
# Check Point 6 - NHANES Vision and Eye Health Surveillance
# Author: Wen Liu
# ============================================================

library(tidyverse)
library(knitr)

df <- read_csv(
  "National_Health_and_Nutrition_Examination_Survey_(NHANES)_–_Vision_and_Eye_Health_Surveillance_20260130.csv"
)

df <- as.data.frame(df)
df <- df[is.na(df$Data_Value_Footnote_Symbol), ]
df <- df[!is.na(df$Data_Value), ]

eye_data <- df[
  df$Topic         == "Eye Health Conditions" &
    df$RiskFactor    == "All participants" &
    df$Sex           == "Both sexes" &
    df$RaceEthnicity != "All races" &
    df$Age           != "All ages",
]

eye_data <- eye_data[, c("Category", "Age", "RaceEthnicity",
                         "Data_Value", "Low_Confidence_limit",
                         "High_Confidence_Limit")]

eye_data$Age <- factor(eye_data$Age,
                       levels = c("40-64 years", "65-79 years",
                                  "80 years and older"))

eye_data$RaceEthnicity <- factor(eye_data$RaceEthnicity,
                                 levels = c("White, non-Hispanic",
                                            "Black, non-Hispanic",
                                            "Hispanic, any race",
                                            "Other"))

eye_data <- eye_data[!is.na(eye_data$Age), ]
eye_data <- eye_data[eye_data$RaceEthnicity != "Other", ]
eye_data$RaceEthnicity <- droplevels(eye_data$RaceEthnicity)

# Descriptive stats by Age
age_desc <- do.call(rbind, lapply(
  levels(eye_data$Age), function(a) {
    x <- eye_data$Data_Value[eye_data$Age == a]
    data.frame(Age = a, Mean = round(mean(x),1), SD = round(sd(x),1),
               Min = round(min(x),1), Max = round(max(x),1))
  }
))
kable(age_desc, caption = "Prevalence (%) of eye conditions by age group")

# Descriptive stats by Race/Ethnicity
race_desc <- do.call(rbind, lapply(
  levels(eye_data$RaceEthnicity), function(r) {
    x <- eye_data$Data_Value[eye_data$RaceEthnicity == r]
    data.frame(RaceEthnicity = r, Mean = round(mean(x),1), SD = round(sd(x),1),
               Min = round(min(x),1), Max = round(max(x),1))
  }
))
kable(race_desc, caption = "Prevalence (%) of eye conditions by race/ethnicity")

# Normality Check - Age
shapiro_age <- do.call(rbind, lapply(
  levels(eye_data$Age), function(a) {
    x <- eye_data$Data_Value[eye_data$Age == a]
    if (length(x) >= 3) {
      s <- shapiro.test(x)
      data.frame(Group = a, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
    } else {
      data.frame(Group = a, W = NA, p.value = NA)
    }
  }
))
kable(shapiro_age, caption = "Shapiro-Wilk test by age group")

# Normality Check - Race/Ethnicity
shapiro_race <- do.call(rbind, lapply(
  levels(eye_data$RaceEthnicity), function(r) {
    x <- eye_data$Data_Value[eye_data$RaceEthnicity == r]
    if (length(x) >= 3) {
      s <- shapiro.test(x)
      data.frame(Group = r, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
    } else {
      data.frame(Group = r, W = NA, p.value = NA)
    }
  }
))
kable(shapiro_race, caption = "Shapiro-Wilk test by race/ethnicity")

# Two-way ANOVA
anova_both <- aov(Data_Value ~ Age + RaceEthnicity, data = eye_data)
anova_summary <- as.data.frame(summary(anova_both)[[1]])
anova_summary <- round(anova_summary, 4)
kable(anova_summary, caption = "Two-way ANOVA: Age and Race/Ethnicity")

# Post-hoc Bonferroni - Age
age_posthoc <- pairwise.t.test(eye_data$Data_Value,
                               eye_data$Age,
                               p.adjust.method = "bonferroni")
kable(age_posthoc$p.value, caption = "Post-hoc Bonferroni: Age")

# Post-hoc Bonferroni - Race/Ethnicity
race_posthoc <- pairwise.t.test(eye_data$Data_Value,
                                eye_data$RaceEthnicity,
                                p.adjust.method = "bonferroni")
kable(race_posthoc$p.value, caption = "Post-hoc Bonferroni: Race/Ethnicity")

# Color palettes
pal_age  <- c("40-64 years"        = "#3498DB",
              "65-79 years"        = "#E67E22",
              "80 years and older" = "#E74C3C")

pal_race <- c("White, non-Hispanic" = "#3498DB",
              "Black, non-Hispanic" = "#E74C3C",
              "Hispanic, any race"  = "#F39C12")

# Figure 1: Prevalence by Age Group
ggplot(eye_data, aes(x = Age, y = Data_Value, fill = Age)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = Age), width = 0.1, size = 2, alpha = 0.7) +
  scale_fill_manual(values = pal_age) +
  scale_color_manual(values = pal_age) +
  labs(title    = "Prevalence of Eye Health Conditions by Age Group",
       subtitle = "NHANES Vision and Eye Health Surveillance, U.S. Adults",
       x        = "Age Group",
       y        = "Crude Prevalence (%)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

# Figure 2: Prevalence by Race/Ethnicity
ggplot(eye_data, aes(x = RaceEthnicity, y = Data_Value, fill = RaceEthnicity)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = RaceEthnicity), width = 0.1, size = 2, alpha = 0.7) +
  scale_fill_manual(values = pal_race) +
  scale_color_manual(values = pal_race) +
  scale_x_discrete(labels = c("White,\nNon-Hispanic", "Black,\nNon-Hispanic",
                              "Hispanic,\nAny Race")) +
  labs(title    = "Prevalence of Eye Health Conditions by Race/Ethnicity",
       subtitle = "NHANES Vision and Eye Health Surveillance, U.S. Adults",
       x        = NULL,
       y        = "Crude Prevalence (%)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

# Figure 3: Age x Race/Ethnicity interaction
ggplot(eye_data, aes(x = Age, y = Data_Value,
                     color = RaceEthnicity, group = RaceEthnicity)) +
  geom_point(size = 3, alpha = 0.7,
             position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 1,
            position = position_dodge(width = 0.3)) +
  scale_color_manual(values = pal_race) +
  labs(title    = "Eye Health Condition Prevalence by Age Group and Race/Ethnicity",
       subtitle = "NHANES Vision and Eye Health Surveillance, U.S. Adults",
       x        = "Age Group",
       y        = "Crude Prevalence (%)",
       color    = "Race/Ethnicity") +
  theme_classic(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "right")