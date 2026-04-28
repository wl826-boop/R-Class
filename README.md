# Project Title
Vision and Eye Health Disparities Among U.S. Adults: An Interactive Analysis of NHANES Surveillance Data

## Brief Description
This project analyzes vision and eye health outcomes using data from the National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance dataset. The analysis explores how prevalence estimates of eye health conditions vary across age groups and race/ethnicity among U.S. adults (18+). An interactive Shiny app allows users to filter by age and race/ethnicity and visualize mean prevalence with 95% confidence intervals across multiple eye health conditions.

## Author(s) and Affiliations
Wen Liu  
Cornell University, Master of Public Health (MPH)

## Contact Information
Email: wl826@cornell.edu
GitHub: https://github.com/wl826-boop/R-Class 

## Research Question / Objectives
- Are prevalence estimates of eye health conditions associated with age among U.S. adults?
- Are prevalence estimates of eye health conditions associated with race/ethnicity among U.S. adults?
- How do prevalence estimates vary across different eye health conditions (e.g., diabetic retinopathy, glaucoma, macular degeneration)?

## Data Source and Description
The dataset is from the CDC’s National Health and Nutrition Examination Survey (NHANES) Vision and Eye Health Surveillance system. NHANES is a nationally representative survey of the U.S. population.

Key variables used in this project include:
- Category (categorical): Type of eye health condition (e.g., Exam-Based Diabetic Retinopathy, Exam-Based Glaucoma, Self-Report Age Related Macular Degeneration)
- Age (categorical): Age group of respondents (e.g., 18-39 years, 40-64 years, 65-79 years, 80 years and older)
- RaceEthnicity (categorical): Race/ethnicity of respondents (e.g., White non-Hispanic, Black non-Hispanic, Hispanic any race)
- Data_Value (continuous): Crude prevalence estimate (%) for each eye health condition
- Low_Confidence_limit / High_Confidence_Limit (continuous): Lower and upper bounds of the 95% confidence interval around the prevalence estimate 

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
