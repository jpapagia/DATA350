# Yianni Papagiannopoulos

# app.R
library(shiny)
library(dplyr)
library(ggplot2)

NHANESraw <- read.csv("NHANESraw.csv")

# Try to guess income and education columns by name
income_var <- grep("income", names(NHANESraw), ignore.case = TRUE, value = TRUE)[1]
education_var <- grep("educ", names(NHANESraw), ignore.case = TRUE, value = TRUE)[1]

# Build SES choices based on what we actually find
ses_choices <- c()
if (!is.na(income_var) && length(income_var) == 1) {
  ses_choices["Household income"] <- "income"
}
if (!is.na(education_var) && length(education_var) == 1) {
  ses_choices["Education level"] <- "education"
}

if (length(ses_choices) == 0) {
  stop("Could not find any SES-like variables (no columns with 'income' or 'educ' in the name).")
}

# Get gender choices from the data itself
gender_choices <- NHANESraw$Gender |>
  unique() |>
  sort()

# ----------------------------------------
# UI
# ----------------------------------------
ui <- fluidPage(
  
  titlePanel("Healthy Sleep and Socioeconomic Factors"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      
      sliderInput(
        inputId = "age_range",
        label   = "Age range:",
        min     = 16,
        max     = max(NHANESraw$Age, na.rm = TRUE),
        value   = c(18, 65),
        step    = 1
      ),
      
      checkboxGroupInput(
        inputId = "gender_sel",
        label   = "Gender:",
        choices = gender_choices,
        selected = gender_choices
      ),
      
      selectInput(
        inputId = "ses_factor",
        label   = "Socioeconomic factor:",
        choices = ses_choices,
        selected = ses_choices[1]
      ),
      
      checkboxInput(
        inputId = "add_noise",
        label   = "Add visual noise (rnorm) to proportions",
        value   = FALSE
      )
    ),
    
    mainPanel(
      h3("Proportion of Adults with Healthy Sleep (7–9 hours)"),
      p("This dashboard shows how the proportion of adults achieving healthy sleep ",
        "(defined as 7–9 hours per night) varies across socioeconomic groups, ",
        "and how these patterns change with age and gender (NHANES 2009–2011)."),
      plotOutput("ses_plot"),
      br(),
      h4("Summary Table"),
      tableOutput("ses_table")
    )
  )
)

# ----------------------------------------
# SERVER
# ----------------------------------------
server <- function(input, output, session) {
  
  ses_filtered <- reactive({
    
    df <- NHANESraw %>%
      filter(
        !is.na(SleepHrsNight),
        !is.na(Age),
        Age >= input$age_range[1],
        Age <= input$age_range[2],
        !is.na(Gender),
        Gender %in% input$gender_sel
      ) %>%
      mutate(
        Gender = factor(Gender),
        
        # "Healthy sleep": 7–9h
        HealthySleep = case_when(
          SleepHrsNight >= 7 & SleepHrsNight <= 9 ~ "Healthy (7–9h)",
          TRUE                                    ~ "Not healthy"
        ),
        HealthySleep = factor(
          HealthySleep,
          levels = c("Healthy (7–9h)", "Not healthy")
        )
      )
    
    df
  })
  
  # SUMMARY TABLE
  ses_summary <- reactive({
    
    df <- ses_filtered()
    validate(
      need(nrow(df) > 0, "No data for the current filters. Try broadening your selection.")
    )
    
    # Map the UI choice ("income"/"education") to actual column name
    ses_code <- input$ses_factor
    if (ses_code == "income") {
      ses_col <- income_var
      x_lab   <- "Household income"
    } else {
      ses_col <- education_var
      x_lab   <- "Education level"
    }
    
    validate(
      need(!is.na(ses_col) && ses_col %in% names(df),
           "Selected SES variable is not found in the dataset.")
    )
    
    out <- df %>%
      filter(!is.na(.data[[ses_col]])) %>%
      mutate(
        SES = factor(.data[[ses_col]])
      ) %>%
      group_by(
        Gender,
        SES
      ) %>%
      summarise(
        n           = n(),
        n_healthy   = sum(HealthySleep == "Healthy (7–9h)"),
        prop_healthy = n_healthy / n,
        .groups     = "drop"
      )
    
    # Optional visual jitter
    if (input$add_noise) {
      out <- out %>%
        mutate(
          prop_healthy = pmin(pmax(prop_healthy + rnorm(n(), 0, 0.01), 0), 1)
        )
    }
    
    attr(out, "x_lab") <- x_lab
    out
  })
  
  # PLOT
  output$ses_plot <- renderPlot({
    df <- ses_summary()
    x_lab <- attr(df, "x_lab")
    
    ggplot(df, aes(x = SES, y = prop_healthy, fill = Gender)) +
      geom_col(position = position_dodge(width = 0.8)) +
      labs(
        x     = x_lab,
        y     = "Proportion with healthy sleep (7–9 hours)",
        fill  = "Gender",
        title = "Healthy Sleep by Socioeconomic Group and Gender"
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # TABLE OUTPUT
  output$ses_table <- renderTable({
    ses_summary() %>%
      arrange(SES, Gender) %>%
      mutate(prop_healthy = round(prop_healthy, 3))
  })
}

# RUN APP
shinyApp(ui = ui, server = server)
