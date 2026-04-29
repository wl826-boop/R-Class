---
  title: "Final Report"
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

Vision health is a critical yet often overlooked component of population health, influencing quality of life, independence, and health equity across the lifespan. Visual impairment is associated with increased risk of injury, reduced workforce participation, and poorer mental and physical health outcomes. Disparities in vision outcomes persist across age, race/ethnicity, and demographic groups, making vision health an important public health concern.

The research question evolved across earlier checkpoints of this project. In Check Point 2, the analysis focused broadly on exploring the distribution of vision-related prevalence estimates across the NHANES dataset without a specific comparison group. In Check Point 4, the question shifted to examining sex differences in prevalence estimates using a simulation approach. However, after deeper exploration of the data, sex differences were found to be limited in scope and the simulation approach was more methodological than substantive. The final research question — whether prevalence estimates are associated with age and race/ethnicity — was chosen because these two variables showed clearer patterns of variation in the data, are more directly relevant to public health disparities, and align with the interactive Shiny app developed as part of this project. This question was further refined in Check Point 6, where a two-way ANOVA framework confirmed that age is a significant predictor of eye health condition prevalence among U.S. adults.

**Research Question:** Are prevalence estimates of eye health conditions associated with age and race/ethnicity among U.S. adults (18+)?
  
  This project uses data from the National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance dataset, collected by the Centers for Disease Control and Prevention (CDC). NHANES is a nationally representative survey of the U.S. civilian, non-institutionalized population. The dataset includes demographic and clinical vision measurements collected through interviews and examinations across multiple survey cycles.

Data source: https://data.cdc.gov/Vision-Eye-Health/National-Health-and-Nutrition-Examination-Survey-N/tdbk-8ubw/data_preview

# Materials & Methods

## Data Source

The dataset was obtained from the CDC NHANES Vision and Eye Health Surveillance system. It contains prevalence estimates (%) for multiple eye health conditions stratified by age group, race/ethnicity, and sex across U.S. survey cycles.

## Data Preparation

```{r load-data, results='hide'}
library(tidyverse)
library(kableExtra)

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
```

Rows with missing or suppressed prevalence values were removed. "Measured Visual Acuity" was excluded as it is not a prevalence measure. Analysis was restricted to adults aged 40 and older, as the 18-39 age group showed very low prevalence estimates across most eye health conditions and limited representation in the surveillance data, making it less informative for comparative analysis. Only the three main racial/ethnic groups were included.
## Key Variables

```{r variable-table}
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
```

## Statistical Analysis

Normality of prevalence estimates within each age and race/ethnicity group was assessed using the Shapiro-Wilk test. A two-way ANOVA was then used to simultaneously test whether mean prevalence differed by age group and race/ethnicity. Post-hoc pairwise comparisons were conducted using Bonferroni correction. All analyses were performed in R 4.5.

# Results

## Distribution of Prevalence Estimates

```{r histogram, fig.cap="Distribution of crude prevalence estimates across all eye health conditions."}
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
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))
```

The distribution of prevalence estimates is right-skewed, with most estimates falling below 25%. The mean prevalence is `r round(mean(df$Data_Value, na.rm=TRUE), 1)`% and the median is `r round(median(df$Data_Value, na.rm=TRUE), 1)`%.

## Normality Assessment

```{r normality}
shapiro_age <- do.call(rbind, lapply(levels(df$Age), function(a) {
  x <- df$Data_Value[df$Age == a]
  s <- shapiro.test(x)
  data.frame(Group = a, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
}))

shapiro_race <- do.call(rbind, lapply(levels(df$RaceEthnicity), function(r) {
  x <- df$Data_Value[df$RaceEthnicity == r]
  s <- shapiro.test(x)
  data.frame(Group = r, W = round(s$statistic, 3), p.value = round(s$p.value, 4))
}))

kable(shapiro_age, caption = "Shapiro-Wilk normality test by age group")
kable(shapiro_race, caption = "Shapiro-Wilk normality test by race/ethnicity")
```

Shapiro-Wilk tests indicated significant departures from normality across most subgroups (p < 0.05). However, ANOVA is robust to violations of normality with larger sample sizes, so the two-way ANOVA was retained as an appropriate approximation.

## Prevalence by Age Group

```{r age-plot, fig.cap="Prevalence of eye health conditions by age group."}
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
```

```{r age-desc-table}
age_desc <- do.call(rbind, lapply(levels(df$Age), function(a) {
  x <- df$Data_Value[df$Age == a]
  data.frame(Age = a, Mean = round(mean(x), 1), SD = round(sd(x), 1),
             Min = round(min(x), 1), Max = round(max(x), 1))
}))
kable(age_desc, caption = "Descriptive statistics of prevalence (%) by age group.")
```

Prevalence estimates increased consistently with age, from a mean of 8.3% in the 40-64 age group to 31.2% in adults aged 80 and older. This pattern is biologically plausible, as older adults accumulate more years of exposure to risk factors such as UV radiation, hypertension, and diabetes, all of which contribute to conditions like cataracts, glaucoma, and age-related macular degeneration.

## Prevalence by Race/Ethnicity

```{r race-plot, fig.cap="Prevalence of eye health conditions by race/ethnicity."}
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
```

```{r race-desc-table}
race_desc <- do.call(rbind, lapply(levels(df$RaceEthnicity), function(r) {
  x <- df$Data_Value[df$RaceEthnicity == r]
  data.frame(RaceEthnicity = r, Mean = round(mean(x), 1), SD = round(sd(x), 1),
             Min = round(min(x), 1), Max = round(max(x), 1))
}))
kable(race_desc, caption = "Descriptive statistics of prevalence (%) by race/ethnicity.")
```

Descriptive statistics show modest differences across racial/ethnic groups, with Black non-Hispanic adults showing the highest mean prevalence (17.7%) and Hispanic adults the lowest (14.7%). Considerable overlap in distributions is visible in the boxplots, which is consistent with the ANOVA results discussed below.

## Two-Way ANOVA and Post-Hoc Tests

```{r two-way-anova}
anova_both <- aov(Data_Value ~ Age + RaceEthnicity, data = df)
anova_summary <- as.data.frame(summary(anova_both)[[1]])
anova_summary <- round(anova_summary, 4)
kable(anova_summary, caption = "Two-way ANOVA: Age and Race/Ethnicity")
```

```{r posthoc}
age_ph <- pairwise.t.test(df$Data_Value, df$Age, p.adjust.method = "bonferroni")
kable(round(age_ph$p.value, 4), caption = "Post-hoc Bonferroni: Age group comparisons")

race_ph <- pairwise.t.test(df$Data_Value, df$RaceEthnicity, p.adjust.method = "bonferroni")
kable(round(race_ph$p.value, 4), caption = "Post-hoc Bonferroni: Race/ethnicity comparisons")
```

```{r interaction-plot, fig.cap="Prevalence by age group and race/ethnicity."}
ggplot(df, aes(x = Age, y = Data_Value, color = RaceEthnicity, group = RaceEthnicity)) +
  geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = pal_race) +
  labs(x = "Age Group", y = "Crude Prevalence (%)", color = "Race/Ethnicity",
       caption = "Source: CDC NHANES.") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)))
```

```{r anova-results}
age_p  <- summary(anova_both)[[1]][["Pr(>F)"]][1]
race_p <- summary(anova_both)[[1]][["Pr(>F)"]][2]
```

The two-way ANOVA found that both age (F = 73.19, p < 0.001) and race/ethnicity (F = 5.78, p = 0.004) were significantly associated with eye health condition prevalence. Post-hoc Bonferroni tests showed that all age group pairs differed significantly from each other (all p < 0.001), confirming a strong age gradient. For race/ethnicity, Hispanic adults had marginally lower prevalence compared to Black non-Hispanic adults (p = 0.67), and no significant pairwise differences were detected after Bonferroni correction, suggesting the overall significance may be driven by subtle differences across the full distribution rather than any single group pair.

# Conclusions

Age was significantly associated with eye health condition prevalence among U.S. adults, with prevalence nearly tripling from the 40-64 group (mean 8.3%) to the 80+ group (mean 31.2%). Race/ethnicity was also significantly associated with prevalence (p = 0.004), though post-hoc tests did not identify specific group pairs that differed significantly after Bonferroni correction, suggesting the effect is modest and distributed across groups rather than driven by one comparison. A more targeted analysis by specific condition such as glaucoma or diabetic retinopathy separately would likely reveal clearer racial disparities.

These findings reinforce the importance of targeted screening programs for older adults and across racial/ethnic groups. Limitations include the use of aggregated surveillance estimates rather than individual-level data, which precludes adjustment for confounders such as socioeconomic status or access to care.

An interactive Shiny app was developed to allow further exploration: https://wenliu826.shinyapps.io/nhanes/
  
  # AI Use Disclosure Statement
  
  This document was produced using ChatGPT to assist with R code development, debugging, and report writing. All analytical decisions, interpretations, and final edits were reviewed and approved by the author.