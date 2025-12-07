#
# Yianni Papagiannopoulos
#

# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)   # for percent formatting

NHANESraw <- read.csv("NHANESraw.csv")

# Helper to clean generic category labels (e.g., NeverMarried -> Never Married)
clean_category_label <- function(x) {
  x <- as.character(x)
  gsub("([a-z])([A-Z])", "\\1 \\2", x)
}

## Identify SES-related columns

income_var    <- grep("income", names(NHANESraw), ignore.case = TRUE, value = TRUE)[1]
education_var <- grep("educ",   names(NHANESraw), ignore.case = TRUE, value = TRUE)[1]

marital_var <- grep("marital|marstat", names(NHANESraw),
                    ignore.case = TRUE, value = TRUE)[1]

sexorient_var <- grep("sexorient|sex_orient|sexual", names(NHANESraw),
                      ignore.case = TRUE, value = TRUE)[1]

# Helper: safely add a SES choice
add_ses_choice <- function(choices, colname, label = NULL) {
  if (is.null(colname) || is.na(colname) || !colname %in% names(NHANESraw)) return(choices)
  if (is.null(label)) {
    label <- tools::toTitleCase(gsub("_", " ", colname))
  }
  choices[label] <- colname
  choices
}

# Start SES choices with the variables you care about (if present)
ses_choices <- c()
ses_choices <- add_ses_choice(ses_choices, marital_var,    "Marital status")
ses_choices <- add_ses_choice(ses_choices, sexorient_var,  "Sexual orientation")
ses_choices <- add_ses_choice(ses_choices, income_var,     "Household income")
ses_choices <- add_ses_choice(ses_choices, education_var,  "Educational level")

# If none of those are found, fall back to generic SES-like categoricals
if (length(ses_choices) == 0) {
  candidate_ses <- names(NHANESraw)[
    sapply(
      NHANESraw,
      function(x) (is.factor(x) || is.character(x)) &&
        dplyr::n_distinct(x, na.rm = TRUE) >= 3 &&
        dplyr::n_distinct(x, na.rm = TRUE) <= 8
    )
  ]
  candidate_ses <- setdiff(candidate_ses, c("Gender", "SleepHrsNight"))
  for (col in candidate_ses) {
    ses_choices <- add_ses_choice(ses_choices, col)
  }
}

if (length(ses_choices) == 0) {
  stop("Could not find any SES-like variables.")
}

# Get gender choices directly from the data
gender_choices_raw <- NHANESraw$Gender |>
  unique() |>
  sort()

## UI

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
        choices = gender_choices_raw,
        selected = gender_choices_raw
      ),
      
      selectInput(
        inputId = "ses_factor",
        label   = "Socioeconomic factor:",
        choices = ses_choices,
        selected = ses_choices[1]
      ),
      
      sliderInput(
        inputId = "min_n",
        label   = "Minimum group size (N) to display:",
        min     = 5,
        max     = 200,
        value   = 30,
        step    = 5
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

## Server

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
        # Clean up gender labels (e.g., "male" -> "Male")
        Gender = factor(
          tools::toTitleCase(as.character(Gender))
        ),
        
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
  
  # Summary Table
  ses_summary <- reactive({
    
    df <- ses_filtered()
    validate(
      need(nrow(df) > 0, "No data for the current filters. Try broadening your selection.")
    )
    
    # input$ses_factor directly stores the column name
    ses_col <- input$ses_factor
    
    validate(
      need(!is.null(ses_col) && ses_col %in% names(df),
           "Selected SES variable is not found in the dataset.")
    )
    
    # Pretty x-axis label from the named choices
    x_lab <- names(ses_choices)[ses_choices == ses_col][1]
    
    out <- df %>%
      filter(!is.na(.data[[ses_col]])) %>%
      mutate(
        SES_raw = factor(.data[[ses_col]])
      ) %>%
      group_by(
        Gender,
        SES_raw
      ) %>%
      summarise(
        n            = n(),
        n_healthy    = sum(HealthySleep == "Healthy (7–9h)"),
        prop_healthy = n_healthy / n,
        .groups      = "drop"
      ) %>%
      # Drop very small groups (unstable proportions)
      filter(n >= input$min_n) %>%
      # 95% CI for proportion
      mutate(
        se       = sqrt(prop_healthy * (1 - prop_healthy) / n),
        ci_lower = pmax(prop_healthy - 1.96 * se, 0),
        ci_upper = pmin(prop_healthy + 1.96 * se, 1)
      )
    
    validate(
      need(nrow(out) > 0,
           "No groups meet the minimum sample size. Try lowering the minimum N.")
    )
    
    ## Special handling for income: nice ordered brackets
    if (!is.null(income_var) && !is.na(income_var) &&
        ses_col == income_var) {
      
      income_order_raw <- c(
        "0-4999",
        "5000-9999",
        "10000-14999",
        "15000-19999",
        "20000-24999",
        "25000-34999",
        "35000-44999",
        "45000-54999",
        "55000-64999",
        "65000-74999",
        "75000-99999",
        "more 99999"
      )
      
      income_labels_pretty <- c(
        "$0–4,999",
        "$5,000–9,999",
        "$10,000–14,999",
        "$15,000–19,999",
        "$20,000–24,999",
        "$25,000–34,999",
        "$35,000–44,999",
        "$45,000–54,999",
        "$55,000–64,999",
        "$65,000–74,999",
        "$75,000–99,999",
        "$100,000+"
      )
      
      # Relevel SES_raw according to the defined income order
      out <- out %>%
        mutate(
          SES_raw = factor(as.character(SES_raw), levels = income_order_raw)
        )
      
      # Keep only labels for levels that are actually present
      present_raw <- levels(out$SES_raw)[levels(out$SES_raw) %in% income_order_raw]
      pretty_map  <- setNames(income_labels_pretty, income_order_raw)
      present_pretty <- pretty_map[present_raw]
      
      out <- out %>%
        mutate(
          SES = factor(
            pretty_map[as.character(SES_raw)],
            levels = present_pretty
          )
        )
      
    } else {
      ##  Generic cleaning for other SES variables
      ses_levels_clean <- clean_category_label(levels(out$SES_raw))
      
      out <- out %>%
        mutate(
          SES = factor(
            clean_category_label(SES_raw),
            levels = ses_levels_clean
          )
        )
    }
    
    out <- out %>% select(-SES_raw)
    
    attr(out, "x_lab") <- x_lab
    out
  })
  
  # PLOT
  output$ses_plot <- renderPlot({
    df    <- ses_summary()
    x_lab <- attr(df, "x_lab")
    
    dodge <- position_dodge(width = 0.8)
    
    ggplot(df, aes(x = SES, y = prop_healthy, fill = Gender)) +
      geom_col(
        position = dodge,
        width    = 0.7
      ) +
      geom_errorbar(
        aes(ymin = ci_lower, ymax = ci_upper),
        width    = 0.2,
        position = dodge
      ) +
      labs(
        x     = x_lab,
        y     = "Percent with healthy sleep (7–9 hours)",
        fill  = "Gender",
        title = "Healthy Sleep by Socioeconomic Group and Gender"
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        labels = percent_format(accuracy = 1)
      ) +
      # Softer colors: Female = soft red, Male = soft teal/blue
      scale_fill_manual(
        values = c("Female" = "#F8766D", "Male" = "#00BFC4"),
        drop   = FALSE
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "top"
      )
  })
  
  # Table Output
  output$ses_table <- renderTable({
    ses_summary() %>%
      arrange(SES, Gender) %>%
      mutate(
        `Healthy sleep (%)` = percent(prop_healthy, accuracy = 0.1)
      ) %>%
      rename(
        `N (healthy)`     = n_healthy,
        `N (total)`       = n,
        `Standard Error`  = se,
        `Lower CI`        = ci_lower,
        `Upper CI`        = ci_upper
      ) %>%
      select(Gender, `N (total)`, `N (healthy)`,
             `Healthy sleep (%)`, `Standard Error`,
             `Lower CI`, `Upper CI`, SES)
  })
  
}

# RUN APP
shinyApp(ui = ui, server = server)
