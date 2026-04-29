# ============================================================
# Final Report - R Script
# Title: Vision and Eye Health Disparities Among U.S. Adults
# Author: Wen Liu
# Date: 2026-04-28
# ============================================================

# ---- Install packages if needed ----
# install.packages("tidyverse")
# install.packages("kableExtra")

# ---- Load packages ----
library(tidyverse)
library(kableExtra)

# ---- Load & Clean Data ----
# Make sure nhanes_vision.csv is in your working directory
df <- read.csv("nhanes_vision.csv") |>
  filter(!is.na(Data_Value),
         Data_Value > 0,
         !Category %in% c("Measured Visual Acuity", "Blind or Difficulty Seeing"),
         Age %in% c("40-64 years", "65-79 years", "80 years and older"),
         Sex == "Both sexes",
         RaceEthnicity %in% c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"))

df$Age <- factor(df$Age, levels = c("40-64 years", "65-79 years", "80 years and older"))
df$RaceEthnicity <- factor(df$RaceEthnicity,
                           levels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"))

# ---- Variable Table ----
var_table <- data.frame(
  Variable = c("Category", "Age", "RaceEthnicity", "Data_Value",
               "Low_Confidence_limit", "High_Confidence_Limit"),
  Type = c("Categorical", "Categorical", "Categorical",
           "Continuous", "Continuous", "Continuous"),
  Description = c(
    "Type of eye health condition (e.g., Diabetic Retinopathy, Glaucoma)",
    "Age group (40-64, 65-79, 80+ years)",
    "Race/ethnicity of respondents",
    "Crude prevalence estimate (%)",
    "Lower bound of 95% confidence interval (%)",
    "Upper bound of 95% confidence interval (%)"
  )
)
kable(var_table, caption = "Key variables used in the analysis", align = c("l", "c", "l"))

# ---- Normality Tests ----
shapiro_age <- do.call(rbind, lapply(levels(df$Age), function(a) {
  x <- df$Data_Value[df$Age == a]
  s <- shapiro.test(x)
  data.frame(Group = a, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
}))
kable(shapiro_age, caption = "Shapiro-Wilk normality test by age group")

shapiro_race <- do.call(rbind, lapply(levels(df$RaceEthnicity), function(r) {
  x <- df$Data_Value[df$RaceEthnicity == r]
  s <- shapiro.test(x)
  data.frame(Group = r, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
}))
kable(shapiro_race, caption = "Shapiro-Wilk normality test by race/ethnicity")

# ---- Figure 1: Histogram ----
ggplot(df, aes(x = Data_Value)) +
  geom_histogram(binwidth = 1, fill = "steelblue4", color = "black", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(Data_Value, na.rm = TRUE), color = "Mean"), linewidth = 1.2) +
  geom_vline(aes(xintercept = median(Data_Value, na.rm = TRUE), color = "Median"), linewidth = 1.2) +
  scale_color_manual(values = c("Mean" = "red", "Median" = "darkgreen")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Distribution of Eye Health Condition Prevalence Estimates",
       x = "Prevalence estimate (%)", y = "Number of subgroup estimates", color = "",
       caption = "Source: CDC NHANES Vision and Eye Health Surveillance.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))

# ---- Descriptive Stats by Age ----
age_desc <- do.call(rbind, lapply(levels(df$Age), function(a) {
  x <- df$Data_Value[df$Age == a]
  data.frame(Age = a, Mean = round(mean(x), 1), SD = round(sd(x), 1),
             Min = round(min(x), 1), Max = round(max(x), 1))
}))
kable(age_desc, caption = "Descriptive statistics of prevalence (%) by age group.")

# ---- Figure 2: Age Boxplot ----
pal_age <- c("40-64 years" = "#3498DB", "65-79 years" = "#E67E22", "80 years and older" = "#E74C3C")

ggplot(df, aes(x = Age, y = Data_Value, fill = Age)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = Age), width = 0.1, size = 2, alpha = 0.7) +
  scale_fill_manual(values = pal_age) +
  scale_color_manual(values = pal_age) +
  labs(title = "Prevalence of Eye Health Conditions by Age Group",
       x = "Age Group", y = "Crude Prevalence (%)",
       caption = "Source: CDC NHANES.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))

# ---- Descriptive Stats by Race/Ethnicity ----
race_desc <- do.call(rbind, lapply(levels(df$RaceEthnicity), function(r) {
  x <- df$Data_Value[df$RaceEthnicity == r]
  data.frame(RaceEthnicity = r, Mean = round(mean(x), 1), SD = round(sd(x), 1),
             Min = round(min(x), 1), Max = round(max(x), 1))
}))
kable(race_desc, caption = "Descriptive statistics of prevalence (%) by race/ethnicity.")

# ---- Figure 3: Race/Ethnicity Boxplot ----
pal_race <- c("White, non-Hispanic" = "#3498DB",
              "Black, non-Hispanic" = "#E74C3C",
              "Hispanic, any race"  = "#F39C12")

ggplot(df, aes(x = RaceEthnicity, y = Data_Value, fill = RaceEthnicity)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = RaceEthnicity), width = 0.1, size = 2, alpha = 0.7) +
  scale_fill_manual(values = pal_race) +
  scale_color_manual(values = pal_race) +
  scale_x_discrete(labels = c("White,\nNon-Hispanic", "Black,\nNon-Hispanic", "Hispanic,\nAny Race")) +
  labs(title = "Prevalence of Eye Health Conditions by Race/Ethnicity",
       x = NULL, y = "Crude Prevalence (%)",
       caption = "Source: CDC NHANES.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))

# ---- Two-Way ANOVA ----
anova_both <- aov(Data_Value ~ Age + RaceEthnicity, data = df)
anova_summary <- as.data.frame(summary(anova_both)[[1]])
anova_summary <- round(anova_summary, 4)
kable(anova_summary, caption = "Two-way ANOVA: Age and Race/Ethnicity")

# ---- Post-Hoc Bonferroni ----
age_ph <- pairwise.t.test(df$Data_Value, df$Age, p.adjust.method = "bonferroni")
kable(round(age_ph$p.value, 4), caption = "Post-hoc Bonferroni: Age group comparisons")

race_ph <- pairwise.t.test(df$Data_Value, df$RaceEthnicity, p.adjust.method = "bonferroni")
kable(round(race_ph$p.value, 4), caption = "Post-hoc Bonferroni: Race/ethnicity comparisons")

# ---- Figure 4: Interaction Plot ----
ggplot(df, aes(x = Age, y = Data_Value, color = RaceEthnicity, group = RaceEthnicity)) +
  geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = pal_race) +
  labs(title = "Prevalence by Age Group and Race/Ethnicity",
       x = "Age Group", y = "Crude Prevalence (%)", color = "Race/Ethnicity",
       caption = "Source: CDC NHANES.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))

# ---- ANOVA p-values ----
age_p  <- summary(anova_both)[[1]][["Pr(>F)"]][1]
race_p <- summary(anova_both)[[1]][["Pr(>F)"]][2]
print(paste("Age p-value:", age_p))
print(paste("Race/Ethnicity p-value:", race_p))
