#
# Alexandra Julka, Yianni Papagiannopoulos, Madhavan Narkeeran
#

library(shiny)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DT)      # DTOutput / renderDT
library(scales)  # percent formatting
library(tools)   # toTitleCase()

NHANESraw <- read.csv("NHANESraw.csv")

#------------------------
# Helpers for Tab 3 (SES)
#------------------------

# Clean generic category labels (e.g., NeverMarried -> Never Married)
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

# Safely add SES choice
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

# Gender choices for Tab 3
gender_choices_raw <- NHANESraw$Gender |>
  unique() |>
  sort()

#------------------------
# Tab 2 (Madhavan) data
#------------------------

cleaned_work <- NHANESraw |>
  filter(
    !is.na(Gender),
    !is.na(Education),
    !is.na(HomeOwn),
    !is.na(Work),
    !is.na(SleepHrsNight)
  ) |>
  select(Gender, Education, HomeOwn, Work, SleepHrsNight) |>
  mutate(
    # small jitter so densities don’t stack on exact integers
    SleepHrsNight = SleepHrsNight + runif(n(), 0, 1)
  )

#-------------
# UI
#-------------

ui <- fluidPage(
  titlePanel("NHANES Sleep, Work, and Socioeconomic Inequality"),
  
  tabsetPanel(
    
    #--------------------------
    # TAB 0: Summary
    #--------------------------
    tabPanel(
      title = "Summary",
      fluidPage(
        br(),
        h3("Overall Summary"),
        textOutput("summary_intro"),
        br(),
        
        h3("Multivariate Patterns (Tab 1 – PCA/MCA)"),
        textOutput("summary_pca_mca"),
        br(),
        
        h3("Work & Daily Functioning (Tab 2 – Work Impact)"),
        textOutput("summary_work"),
        br(),
        
        h3("Socioeconomic Differences (Tab 3 – Social-economic Factors)"),
        textOutput("summary_ses"),
        br(),
        
        h3("Limitations & Next Steps"),
        textOutput("summary_limits"),
        br()
        
      )
    ),
    
    #--------------------------
    # TAB 1: PCA & MCA explorer
    #--------------------------
    tabPanel(
      title = "Tab 1 - PCA/MCA",
      
      sidebarLayout(
        sidebarPanel(
          h4("Analysis Options"),
          radioButtons(
            inputId = "choice",
            label = "Select an analysis:",
            choices = c("PCA", "MCA"),
            selected = "PCA"
          ),
          actionButton("submit", "Run Analysis"),
          hr(),
          
          h4("Axis Selection"),
          helpText("For PCA, choose principal components (PCs) for the X and Y axes."),
          uiOutput("pcx_ui"),
          uiOutput("pcy_ui"),
          br(),
          
          helpText("For MCA, choose dimensions for the X and Y axes."),
          uiOutput("mcx_ui"),
          uiOutput("mcy_ui")
        ),
        
        mainPanel(
          h4("What does this tab do?"),
          tags$p(
            "This tab lets you explore the NHANESraw dataset using two multivariate methods: ",
            tags$strong("Principal Component Analysis (PCA)"),
            " for numeric variables and ",
            tags$strong("Multiple Correspondence Analysis (MCA)"),
            " for categorical variables."
          ),
          tags$ul(
            tags$li(
              tags$strong("PCA:"),
              " reduces many numeric variables into a few principal components that capture most of the variation."
            ),
            tags$li(
              tags$strong("MCA:"),
              " does something similar for categorical variables, mapping categories into a lower-dimensional space."
            ),
            tags$li(
              "Use the sliders below to control point transparency and size."
            )
          ),
          hr(),
          
          h4("Plot Appearance"),
          sliderInput("alpha", "Point transparency (alpha):", min = 0, max = 1, value = 0.7),
          sliderInput("size",  "Point size:",              min = 0.05, max = 4.05, value = 1.56),
          checkboxInput(
            inputId = "exclude_na_sleep",
            label   = "Exclude participants with missing sleep hours (NA) from plots",
            value   = FALSE
          ),
          hr(),
          
          # PCA
          conditionalPanel(
            condition = "input.choice == 'PCA'",
            h3("PCA Results"),
            tags$p(
              "Each point represents an individual in the dataset, projected onto the selected principal components (PCs). ",
              "Color represents reported sleep hours per night."
            ),
            plotOutput("pcaPlot"),
            br(),
            tags$strong("PCA Summary (variance explained):"),
            verbatimTextOutput("pcaSummary"),
            br(),
            tags$strong("PCA Scores Table (rounded):"),
            DTOutput("pcaTable")
          ),
          
          # MCA
          conditionalPanel(
            condition = "input.choice == 'MCA'",
            h3("MCA Results"),
            tags$p(
              "Each point represents an individual in the MCA space, based on categorical variables. ",
              "Dimensions are analogous to components in PCA."
            ),
            plotOutput("mcaPlot"),
            br(),
            tags$strong("MCA Eigenvalues (variance by dimension):"),
            verbatimTextOutput("mcaSummary"),
            br(),
            tags$strong("MCA Coordinates Table (rounded):"),
            DTOutput("mcaTable")
          )
        )
      )
    ),
    
    #--------------------------
    # TAB 2: Work Impact (Madhavan)
    #--------------------------
    tabPanel(
      title = "Tab 2 - Work Impact",
      
      sidebarLayout(
        sidebarPanel(
          h3("Filters"),
          
          radioButtons(
            "gender", 
            "Select Gender:",
            choices  = c("Male", "Female"),
            selected = "Male"
          ),
          
          radioButtons(
            "education", 
            "Select Education:", 
            choices  = c("9 - 11th Grade", "High School", "Some College", "College Grad"),
            selected = "9 - 11th Grade"
          ),
          
          radioButtons(
            "homeown", 
            "Select Home Ownership:",
            choices  = c("Own", "Rent"),
            selected = "Own"
          ),
          width = 3
        ),
        
        mainPanel(
          p("This dashboard shows how nightly sleep duration varies by work status, 
            with filters for gender, education level, and home ownership."),
          br(),
          
          # Dynamic title and caption in the UI (not inside ggplot)
          h3(textOutput("plotTitle")),
          br(),
          
          plotOutput("densityPlot", height = "550px"),
          
          br(),
          p(
            textOutput("captionText"),
            style = "font-style: italic; font-size: 12px;"
          ),
          width = 9
        )
      )
    ),
    
    #-------------------------------
    # TAB 3: Healthy Sleep & SES
    #-------------------------------
    tabPanel(
      title = "Tab 3 - Social-economic Factors",
      
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
  )
)

#-------------
# Server
#-------------

server <- function(input, output, session) {
  
  #========================
  # TAB 1: PCA / MCA - Alexandra
  #========================
  
  # PCA data
  dataInput <- reactive({
    if (input$choice != "PCA") return(NULL)
    req(NHANESraw, input$submit)
    
    numeric_df <- NHANESraw[sapply(NHANESraw, is.numeric)]
    if (ncol(numeric_df) < 2) {
      showNotification("Need at least two numeric columns for PCA.", type = "error")
      return(NULL)
    }
    
    # Mean-impute numeric variables
    if (sum(is.na(numeric_df)) != 0) {
      for (col in names(numeric_df)) {
        mean_val <- mean(numeric_df[[col]], na.rm = TRUE)
        numeric_df[[col]][is.na(numeric_df[[col]])] <- mean_val
      }
    }
    
    # Exclude SleepHrsNight from predictors
    X_df <- subset(numeric_df, select = -SleepHrsNight)
    X_df
  })
  
  # PCA result
  pcaResult <- reactive({
    if (input$choice != "PCA") return(NULL)
    req(dataInput(), input$submit)
    prcomp(dataInput(), scale. = TRUE)
  })
  
  # Dynamic PC selectors
  output$pcx_ui <- renderUI({
    req(pcaResult(), input$submit)
    choices <- paste0("PC", 1:ncol(pcaResult()$x))
    selectInput("pcx", "X-axis PC:", choices = choices, selected = "PC1")
  })
  
  output$pcy_ui <- renderUI({
    req(pcaResult(), input$submit)
    choices <- paste0("PC", 1:ncol(pcaResult()$x))
    selectInput("pcy", "Y-axis PC:", choices = choices, selected = "PC2")
  })
  
  # PCA Plot
  output$pcaPlot <- renderPlot({
    req(pcaResult(), input$pcx, input$pcy, input$submit)
    pca_df <- as.data.frame(pcaResult()$x)
    sleep  <- NHANESraw$SleepHrsNight
    
    if (isTRUE(input$exclude_na_sleep)) {
      keep <- !is.na(sleep)
      pca_df <- pca_df[keep, , drop = FALSE]
      sleep  <- sleep[keep]
    }
    
    ggplot(pca_df, aes_string(x = input$pcx, y = input$pcy)) +
      geom_point(
        aes(color = sleep),
        size  = input$size,
        alpha = input$alpha
      ) +
      scale_color_gradient(
        low  = "red",
        high = "blue",
        name = "Sleep Hours (night)"
      ) +
      theme_bw() +
      labs(
        title = "PCA Scatter Plot",
        x = input$pcx,
        y = input$pcy
      )
  })
  
  # PCA Summary
  output$pcaSummary <- renderPrint({
    req(pcaResult())
    summary(pcaResult())
  })
  
  # PCA Table
  output$pcaTable <- renderDT({
    req(pcaResult(), input$submit)
    pca_df <- as.data.frame(pcaResult()$x)
    pca_df <- round(pca_df, 3)
    datatable(pca_df, options = list(pageLength = 10))
  })
  
  # MCA data (with aligned sleep vector)
  mcaData <- reactive({
    if (input$choice != "MCA") return(NULL)
    req(NHANESraw, input$submit)
    
    categorical_df <- NHANESraw[sapply(NHANESraw, function(x) is.factor(x) || is.character(x))]
    
    # Convert character -> factor
    for (col in names(categorical_df)) {
      if (is.character(categorical_df[[col]])) {
        categorical_df[[col]] <- as.factor(categorical_df[[col]])
      }
    }
    
    # Downsample rows
    max_n <- 1000
    idx <- seq_len(nrow(categorical_df))
    if (nrow(categorical_df) > max_n) {
      idx <- sample.int(nrow(categorical_df), size = max_n)
      categorical_df <- categorical_df[idx, , drop = FALSE]
    }
    
    # Missing -> "No Response"
    if (sum(is.na(categorical_df)) != 0) {
      for (col in names(categorical_df)) {
        levels(categorical_df[[col]]) <- c(levels(categorical_df[[col]]), "No Response")
      }
      categorical_df[is.na(categorical_df)] <- "No Response"
    }
    
    if (ncol(categorical_df) < 2) {
      showNotification("Need at least two categorical columns for MCA.", type = "error")
      return(NULL)
    }
    
    sleep_vec <- NHANESraw$SleepHrsNight[idx]
    list(df = categorical_df, sleep = sleep_vec)
  })
  
  # MCA result
  mcaResult <- reactive({
    if (input$choice != "MCA") return(NULL)
    req(mcaData(), input$submit)
    MCA(mcaData()$df, ncp = 29)
  })
  
  # Dynamic MCA axis selectors
  output$mcx_ui <- renderUI({
    req(mcaResult(), input$submit)
    dims <- seq_len(ncol(mcaResult()$ind$coord))
    dim_names <- paste0("Dim ", dims)
    selectInput("mcx", "X-axis Dimension:", choices = dim_names, selected = "Dim 1")
  })
  
  output$mcy_ui <- renderUI({
    req(mcaResult(), input$submit)
    dims <- seq_len(ncol(mcaResult()$ind$coord))
    dim_names <- paste0("Dim ", dims)
    selectInput("mcy", "Y-axis Dimension:", choices = dim_names, selected = "Dim 2")
  })
  
  # MCA Plot
  output$mcaPlot <- renderPlot({
    req(mcaResult(), mcaData(), input$mcx, input$mcy, input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    sleep  <- mcaData()$sleep
    
    if (isTRUE(input$exclude_na_sleep)) {
      keep <- !is.na(sleep)
      mca_df <- mca_df[keep, , drop = FALSE]
      sleep  <- sleep[keep]
    }
    
    max_points <- 1000
    if (nrow(mca_df) > max_points) {
      idx <- sample.int(nrow(mca_df), size = max_points)
      mca_df <- mca_df[idx, , drop = FALSE]
      sleep  <- sleep[idx]
    }
    
    ggplot(
      mca_df,
      aes(x = .data[[input$mcx]], y = .data[[input$mcy]])
    ) +
      geom_point(
        aes(color = sleep),
        size  = input$size,
        alpha = input$alpha
      ) +
      scale_color_gradient(
        low  = "red",
        high = "blue",
        name = "Sleep Hours (night)"
      ) +
      theme_bw() +
      labs(
        title = "MCA Scatter Plot",
        x = input$mcx,
        y = input$mcy
      )
  })
  
  # MCA Summary
  output$mcaSummary <- renderPrint({
    req(mcaResult())
    get_eigenvalue(mcaResult())
  })
  
  # MCA Table
  output$mcaTable <- renderDT({
    req(mcaResult(), input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    mca_df <- round(mca_df, 3)
    datatable(mca_df, options = list(pageLength = 10))
  })
  
  #========================
  # TAB 2: Work Impact (Madhavan)
  #========================
  
  # Dynamic title shown above the plot (wraps automatically in UI)
  output$plotTitle <- renderText({
    gender    <- tolower(input$gender)
    education <- tolower(input$education)
    homeown   <- ifelse(input$homeown == "Own", "own", "rent")
    
    paste0(
      "Distribution of sleep hours per night for ",
      gender, " adults with ",
      education, " education who ",
      homeown, " their home by work status"
    )
  })
  
  # Dynamic caption shown below the plot
  output$captionText <- renderText({
    gender    <- tolower(input$gender)
    education <- tolower(input$education)
    homeown   <- ifelse(input$homeown == "Own", "own", "rent")
    
    paste0(
      "Kernel density plot showing nightly sleep duration for ",
      gender, " adults with ", education,
      " education who ", homeown, " their home, separated by work status. ",
      "Small Uniform(0, 1) noise was added to sleep hours for visualization ",
      "(NHANES 2009–2011)."
    )
  })
  
  # Plot
  output$densityPlot <- renderPlot({
    filtered <- cleaned_work |>
      filter(
        Gender == ifelse(input$gender == "Male", "male", "female"),
        Education == input$education,
        HomeOwn == input$homeown,
        Work %in% c("Working", "NotWorking")
      )
    
    ggplot(filtered, aes(x = SleepHrsNight, color = Work)) +
      geom_density(linewidth = 1.0, adjust = 1.0) +
      scale_color_manual(
        name   = "Work Status",
        values = c("Working" = "blue", "NotWorking" = "red"),
        labels = c("Working" = "Working",
                   "NotWorking" = "Not Working")
      ) +
      scale_x_continuous(breaks = 0:15, limits = c(0, 15)) +
      scale_y_continuous(limits = c(0, 0.7)) +
      labs(
        x = "Sleep Hours per Night",
        y = "Density"
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.title.x    = element_text(face = "bold", size = 14),
        axis.title.y    = element_text(face = "bold", size = 14),
        legend.title    = element_text(face = "bold", size = 13),
        plot.margin     = margin(20, 40, 40, 30),
        legend.position = "right",
        legend.box      = "vertical"
      ) +
      coord_cartesian(clip = "off")
  })
  
  #========================
  # TAB 3: Healthy Sleep & SES (Yianni)
  #========================
  
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
        Gender = factor(
          tools::toTitleCase(as.character(Gender))
        ),
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
  
  ses_summary <- reactive({
    df <- ses_filtered()
    validate(
      need(nrow(df) > 0, "No data for the current filters. Try broadening your selection.")
    )
    
    ses_col <- input$ses_factor
    validate(
      need(!is.null(ses_col) && ses_col %in% names(df),
           "Selected SES variable is not found in the dataset.")
    )
    
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
      filter(n >= input$min_n) %>%
      mutate(
        se       = sqrt(prop_healthy * (1 - prop_healthy) / n),
        ci_lower = pmax(prop_healthy - 1.96 * se, 0),
        ci_upper = pmin(prop_healthy + 1.96 * se, 1)
      )
    
    validate(
      need(nrow(out) > 0,
           "No groups meet the minimum sample size. Try lowering the minimum N.")
    )
    
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
      
      out <- out %>%
        mutate(
          SES_raw = factor(as.character(SES_raw), levels = income_order_raw)
        )
      
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
  
  # SES plot
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
  
  # SES table
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
  
  #========================
  # TAB 0: Summary text
  #========================
  
  output$summary_intro <- renderText({
    "This project uses NHANES data from 2009 to 2011 to examine relationships between adult sleep duration and a range of 
    demographic, socioeconomic, and health characteristics. Three complementary parts make up the analysis. The first uses 
    multivariate dimensionality-reduction techniques (PCA and MCA) to visualize broad structural patterns in the dataset and 
    summarize intricate relationships between variables. The second examines sleep patterns among different subgroups and 
    concentrates on work status. The third assesses the differences in healthy sleep between socioeconomic groups, including 
    income, education, and marital status, and compares them by gender. When combined, these techniques provide an organized 
    method for evaluating the relationship between sleep duration and socioeconomic, employment, and demographic factors; each 
    tab displays its own findings and visual interpretation."
  })
  
  output$summary_pca_mca <- renderText({
    "Using two multivariate techniques, Principal Component Analysis (PCA) for numerical data and Multiple Correspondence 
    Analysis (MCA) for categorical data, Tab 1 presents an interactive method for examining the NHANES 2009–2011 dataset.
    For easier interpretation, users can change between PCA and MCA, select which elements or dimensions to show, and modify 
    visual parameters like transparency and point size. Users are able to visually compare sleep behavior across various 
    multivariate patterns because each point in the survey represents an individual, with color denoting their reported nightly 
    sleep length. Additionally, the tab offers rounded coordinate tables and statistical summaries (variance explained for PCA 
    and eigenvalues for MCA) so that the underlying numerical structure may be understood in conjunction with the visual 
    correlations in the graphs.This interface makes it easier to see how sleep duration relates to broader demographic and 
    health characteristics within NHANES. This interface makes it easier to see how sleep duration relates to broader demographic 
    and health characteristics within NHANES."
  })
  
  output$summary_work <- renderText({
    "Tab 2 portrays the distribution of sleep hours per night for non-workers and workers and how this differs between factors such 
    as gender, education level, and home ownership status. The distributions of sleep hours per night for non-workers and workers 
    peak between 7 and 9 hours across gender, education level, and home ownership status, showing broadly similar sleep habits. 
    Working adults often have a slightly sharper peak near 7.5 hours of sleep per night, while non-working adults exhibit broader 
    right tails that reach 10+ hours of sleep per night, hinting at slightly longer sleep when unemployed. The largest difference 
    between the distributions of sleep hours per night for non-workers and workers appears for male college graduates who rent, where 
    working adults have a narrower distribution with a sharper peak at 7 to 8 hours of sleep per night, whereas non-working adults 
    have a lower and broader distribution and stretches farther beyond 10 hours of sleep per night. For females and adults with lower 
    education levels, the distribution of sleep hours per night for workers and non-workers is nearly identical, suggesting work 
    status adds little explanatory power for sleep hours per night in this case."
  })
  
  output$summary_ses <- renderText({
    "Tab 3 examines the relationship between gender and socioeconomic factors and healthy sleep (7 to 9 hours per night). 
    The percentage of adults who meet this recommendation is generally consistent across income, education, marital status, 
    and sexual orientation, typically ranging from 45 to 65 percent for both sexes. Higher income groups and college 
    graduates seem to have a marginally higher percentage of healthy sleepers, though these differences are small and
    many confidence intervals overlap. Smaller groups, especially sexual minorities and some marital categories, have 
    the widest confidence intervals because there are fewer survey respondents in these categories. These smaller groups 
    may be underrepresented in NHANES 2009–2011, so any comparisons should be interpreted with caution and not as 
    compelling proof of significant differences."
  })
  
  output$summary_limits <- renderText({
    "Given that these results are based on NHANES data from 2009 to 2011, which might not accurately reflect current demographics 
    or work patterns, care should be taken when interpreting them. Due to their small sample sizes, a number of subgroups,such as 
    sexual minorities and smaller marital categories, have larger confidence intervals and lower precision. It would be inappropriate 
    to interpret every observed gap as proof of a significant behavioral pattern because some group differences are tiny and statistically 
    uncertain. Newer survey years, larger subgroup samples, and other variables such as sleep disorders, shift work, caregiving obligations, 
    or chronic illness (all of which may have a more direct impact on sleep than socioeconomic status alone) would all contribute to 
    a more thorough analysis."
  })
}

# Run the app
shinyApp(ui = ui, server = server)
