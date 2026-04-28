library(shiny)
library(ggplot2)
library(dplyr)

# ---- Load & Clean Data ----
df <- read.csv("nhanes_vision.csv") |>
  filter(!is.na(Data_Value),
         Data_Value > 0,
         !Category %in% c("Measured Visual Acuity", "Blind or Difficulty Seeing"),
         Age != "0-17 years")

age_levels  <- c("18-39 years", "40-64 years", "65-79 years", "80 years and older", "All ages")
race_levels <- sort(unique(df$RaceEthnicity))
cat_levels  <- sort(unique(df$Category))
group_label <- c(Age = "Age Group", RaceEthnicity = "Race/Ethnicity")
y_max       <- ceiling(max(df$Data_Value, na.rm = TRUE))

# ---- UI ----
ui <- fluidPage(
  titlePanel("NHANES Vision & Eye Health Explorer"),
  wellPanel(
    h4("About this App"),
    p(strong("Author:"), "Wen Liu | Cornell University | wl826@cornell.edu"),
    p(strong("Research Question:"),
      "Are prevalence estimates of eye health conditions associated with age and race/ethnicity among U.S. adults?"),
    p(strong("Data Source:"),
      "NHANES Vision and Eye Health Surveillance dataset (CDC), covering U.S. civilian,
      non-institutionalized adults across age and race/ethnicity groups.",
      a("Access dataset here.", href = "https://data.cdc.gov/Vision-Eye-Health/National-Health-and-Nutrition-Examination-Survey-N/tdbk-8ubw/data_preview", target = "_blank")),
    p(strong("Methods:"),
      "Prevalence estimates (%) were filtered to remove suppressed values and non-prevalence measures
      (Measured Visual Acuity). Analysis was restricted to U.S. adults (18+). Data were grouped
      by age or race/ethnicity and summarized using mean prevalence with 95% confidence intervals."),
    p(strong("GitHub:"),
      a("github.com/wl826-boop/R-Class", href = "https://github.com/wl826-boop/R-Class", target = "_blank")),
    p(strong("AI Disclosure:"),
      "Claude (Anthropic) was used to assist with Shiny app development and code optimization.")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Eye Health Condition:",
                  choices = cat_levels),
      radioButtons("group_var", "Compare prevalence by:",
                   choices = c("Age Group"      = "Age",
                               "Race/Ethnicity" = "RaceEthnicity"),
                   selected = "Age"),
      hr(),
      checkboxGroupInput("age_filter", "Age Group:", choices = age_levels, selected = age_levels),
      checkboxGroupInput("race_filter", "Race/Ethnicity:", choices = race_levels, selected = race_levels)
    ),
    mainPanel(
      plotOutput("bar_plot", height = "420px"),
      br(),
      h4("Summary Table"),
      p(em("Mean prevalence (%) with 95% confidence intervals by selected grouping variable.")),
      tableOutput("summary_table")
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    df |>
      filter(Category      == input$category,
             Age           %in% input$age_filter,
             RaceEthnicity %in% input$race_filter) |>
      group_by(Group = .data[[input$group_var]]) |>
      summarise(Mean_Prevalence = round(mean(Data_Value, na.rm = TRUE), 2),
                Low_CI          = round(mean(Low_Confidence_limit, na.rm = TRUE), 2),
                High_CI         = round(mean(High_Confidence_Limit, na.rm = TRUE), 2),
                .groups = "drop")
  })
  
  output$bar_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No data found. Please adjust your selections."))
    
    data$Group <- if (input$group_var == "Age") {
      factor(data$Group, levels = age_levels)
    } else {
      reorder(data$Group, data$Mean_Prevalence)
    }
    
    ggplot(data, aes(x = Group, y = Mean_Prevalence, fill = Group)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_errorbar(aes(ymin = Low_CI, ymax = High_CI), width = 0.2, color = "gray30") +
      coord_flip() +
      scale_y_continuous(limits = c(0, y_max)) +
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73",
                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
      labs(title    = paste("Prevalence of", input$category, "by", group_label[input$group_var]),
           subtitle = "NHANES Vision & Eye Health Surveillance",
           x = NULL, y = "Prevalence (%)",
           caption = "Error bars show 95% CI. Source: CDC NHANES.") +
      theme_minimal(base_size = 14) +
      theme(plot.title         = element_text(face = "bold"),
            plot.subtitle      = element_text(color = "gray40"),
            panel.grid.major.y = element_blank())
  })
  
  output$summary_table <- renderTable({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No data to display."))
    rename(data, `Group` = Group, `Mean Prevalence (%)` = Mean_Prevalence,
           `Lower CI (%)` = Low_CI, `Upper CI (%)` = High_CI)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ---- Run ----
shinyApp(ui = ui, server = server)