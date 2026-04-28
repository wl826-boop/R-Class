# Project Title
Sex Differences in Vision Health: A Simulation Study Using NHANES Data

## Brief Description
This project analyzes vision and eye health outcomes using data from the National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance dataset. The analysis focuses on differences in prevalence estimates (Data_Value) between males and females. A simulation approach is used to assess how sample size, variability (noise), and effect size influence the estimated differences between groups.

## Author(s) and Affiliations
Wen Liu  
Cornell University, Master of Public Health (MPH)
wl826@cornell.edu

## Contact Information
Email: wl826@cornell.edu
GitHub: https://github.com/wl826-boop/R-Class 

## Research Question / Objectives
- Is there a difference in mean vision-related prevalence estimates (Data_Value) between males and females?
- How does sample size affect the stability of estimated differences?
- How does variability (noise) influence the accuracy of estimates?
- How does effect size impact the ability to detect differences between groups?

## Data Source and Description
The dataset is from the CDC’s National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance system. NHANES is a nationally representative survey of the U.S. population.

Key variables used in this project include:
- **Sex** (categorical): Male or Female  
- **Data_Value** (continuous): Prevalence estimate for vision-related outcomes  
- **Question** (categorical): Survey question describing the outcome  
- **Response** (categorical): Participant response category  
- **Sample Size** (discrete): Number of observations contributing to the estimate  

The dataset provides population-level estimates rather than individual-level observations.

## Links to Reports / Deliverables
- R Markdown Report: https://github.com/wl826-boop/R-Class/tree/main/Scripts
- Figures/Simulation Outputs: https://github.com/wl826-boop/R-Class/tree/main/output 
- GitHub Repository: https://github.com/wl826-boop/R-Class
- ShinnyApps Website: https://wenliu826.shinyapps.io/nhanes-vision/

## AI Tool Disclosure
AI tools (ChatGPT) were used to assist with debugging R code, improving formatting, and refining written explanations. All analytical decisions, interpretation of results, and conclusions were completed independently by the author.

## References / Citations
- Centers for Disease Control and Prevention (CDC). National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance System.
- CDC Vision Health Initiative. https://www.cdc.gov/visionhealth/
