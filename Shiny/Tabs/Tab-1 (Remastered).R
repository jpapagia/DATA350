#
# Alexandra Julka
#

library(shiny)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DT)   # for DTOutput / renderDT

# Safely load NHANESraw from CSV if it's not already in the environment
if (!exists("NHANESraw")) {
  NHANESraw <- read.csv("NHANESraw.csv")
}

ui <- fluidPage(
  titlePanel("PCA & MCA Explorer — NHANESraw"),
  
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
        "This interface lets users explore the NHANESraw dataset using two multivariate methods:",
        tags$strong("Principal Component Analysis (PCA)"),
        "for numeric variables and",
        tags$strong("Multiple Correspondence Analysis (MCA)"),
        "for categorical variables."
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
          "Use the controls below to adjust point transparency, size, and how missing sleep values are handled."
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
        tags$strong("PCA Variable Loadings (contribution of each variable to each PC):"),
        tableOutput("loadingsTablePCA"),
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
        tags$strong("MCA Category Loadings (contribution of each category to each dimension):"),
        tableOutput("loadingsTableMCA"),
        br(),
        tags$strong("MCA Eigenvalues (variance by dimension):"),
        tableOutput("mcaSummary"),
        br(),
        tags$strong("MCA Coordinates Table (rounded):"),
        DTOutput("mcaTable")
      )
    )
  )
)

#  Server
server <- function(input, output, session) {
  
  #========================
  # PCA DATA
  #========================
  dataInput <- reactive({
    if (input$choice != "PCA") return(NULL)
    req(NHANESraw, input$submit)
    
    # Keep only numeric columns for PCA
    numeric_df <- NHANESraw[sapply(NHANESraw, is.numeric)]
    
    # Remove ID, since it's not a predictor
    numeric_df <- numeric_df[, !(names(numeric_df) %in% c("ID")), drop = FALSE]
    
    if (ncol(numeric_df) < 2) {
      showNotification("Need at least two numeric columns for PCA.", type = "error")
      return(NULL)
    }
    
    # Mean-impute missing values in numeric variables
    if (sum(is.na(numeric_df)) != 0) {
      for (col in names(numeric_df)) {
        mean_val <- mean(numeric_df[[col]], na.rm = TRUE)
        numeric_df[[col]][is.na(numeric_df[[col]])] <- mean_val
      }
    }
    
    # Exclude SleepHrsNight from predictors (used only as color)
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
  
  # PCA Plot (with NA toggle + "Missing" shape when included)
  output$pcaPlot <- renderPlot({
    req(pcaResult(), input$pcx, input$pcy, input$submit)
    pca_df <- as.data.frame(pcaResult()$x)
    sleep  <- NHANESraw$SleepHrsNight
    
    if (isTRUE(input$exclude_na_sleep)) {
      # Exclude NA sleep completely
      keep <- !is.na(sleep)
      pca_df <- pca_df[keep, , drop = FALSE]
      sleep  <- sleep[keep]
      
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
      
    } else {
      # Show both observed and missing sleep, with separate shape for missing
      non_na <- !is.na(sleep)
      
      ggplot(pca_df, aes_string(x = input$pcx, y = input$pcy)) +
        geom_point(
          data = pca_df[non_na, , drop = FALSE],
          aes(color = sleep[non_na]),
          size  = input$size,
          alpha = input$alpha
        ) +
        geom_point(
          data = pca_df[!non_na, , drop = FALSE],
          aes(shape = "Missing"),
          size  = input$size,
          alpha = input$alpha,
          color = "gray50"
        ) +
        scale_color_gradient(
          low  = "red",
          high = "blue",
          name = "Sleep Hours (night)"
        ) +
        scale_shape_manual(
          values = c("Missing" = 16),
          name   = "Sleep Hours (night)"
        ) +
        theme_bw() +
        labs(
          title = "PCA Scatter Plot",
          x = input$pcx,
          y = input$pcy
        )
    }
  })
  
  # PCA Variable Loadings
  output$loadingsTablePCA <- renderTable({
    req(pcaResult())
    loadings <- pcaResult()$rotation
    loads <- as.data.frame(round(loadings, 3))
    loads$Variable <- rownames(loadings)
    loads <- loads[, c("Variable", colnames(loadings))]
    loads
  })
  
  # PCA Summary (variance explained)
  output$pcaSummary <- renderPrint({
    req(pcaResult())
    summary(pcaResult())
  })
  
  # PCA Scores Table (rounded)
  output$pcaTable <- renderDT({
    req(pcaResult(), input$submit)
    pca_df <- as.data.frame(pcaResult()$x)
    pca_df <- round(pca_df, 3)
    datatable(pca_df, options = list(pageLength = 10))
  })
  
  #========================
  # MCA DATA
  #========================
  mcaData <- reactive({
    if (input$choice != "MCA") return(NULL)
    req(NHANESraw, input$submit)
    
    # Treat both factor and character columns as categorical
    categorical_df <- NHANESraw[sapply(NHANESraw, function(x) is.factor(x) || is.character(x))]
    
    # Remove ID if it happens to be stored as character/factor
    categorical_df <- categorical_df[, !(names(categorical_df) %in% c("ID")), drop = FALSE]
    
    # Convert character columns to factors
    for (col in names(categorical_df)) {
      if (is.character(categorical_df[[col]])) {
        categorical_df[[col]] <- as.factor(categorical_df[[col]])
      }
    }
    
    # Downsample rows to keep MCA tractable, but keep indices
    max_n <- 1000
    idx <- seq_len(nrow(categorical_df))
    if (nrow(categorical_df) > max_n) {
      idx <- sample.int(nrow(categorical_df), size = max_n)
      categorical_df <- categorical_df[idx, , drop = FALSE]
    }
    
    # Handle missing values by adding "No Response"
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
  
  # MCA Plot (NA toggle + "Missing" shape when included)
  output$mcaPlot <- renderPlot({
    req(mcaResult(), mcaData(), input$mcx, input$mcy, input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    sleep  <- mcaData()$sleep
    
    if (isTRUE(input$exclude_na_sleep)) {
      keep <- !is.na(sleep)
      mca_df <- mca_df[keep, , drop = FALSE]
      sleep  <- sleep[keep]
      
      # Optional downsample for plotting
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
      
    } else {
      # Show both observed and missing sleep, with "Missing" shape
      max_points <- 1000
      if (nrow(mca_df) > max_points) {
        idx <- sample.int(nrow(mca_df), size = max_points)
        mca_df <- mca_df[idx, , drop = FALSE]
        sleep  <- sleep[idx]
      }
      
      non_na <- !is.na(sleep)
      
      ggplot(
        mca_df,
        aes(x = .data[[input$mcx]], y = .data[[input$mcy]])
      ) +
        geom_point(
          data = mca_df[non_na, , drop = FALSE],
          aes(color = sleep[non_na]),
          size  = input$size,
          alpha = input$alpha
        ) +
        geom_point(
          data = mca_df[!non_na, , drop = FALSE],
          aes(shape = "Missing"),
          size  = input$size,
          alpha = input$alpha,
          color = "gray50"
        ) +
        scale_color_gradient(
          low  = "red",
          high = "blue",
          name = "Sleep Hours (night)"
        ) +
        scale_shape_manual(
          values = c("Missing" = 16),
          name   = "Sleep Hours (night)"
        ) +
        theme_bw() +
        labs(
          title = "MCA Scatter Plot",
          x = input$mcx,
          y = input$mcy
        )
    }
  })
  
  # MCA Loadings table
  output$loadingsTableMCA <- renderTable({
    req(mcaResult())
    coords <- mcaResult()$var$coord     
    loads <- as.data.frame(round(coords, 3))
    loads$Category <- rownames(coords)
    loads <- loads[, c("Category", colnames(coords))]
    loads
  })
  
  # MCA Eigenvalues table (formatted)
  output$mcaSummary <- renderTable({
    req(mcaResult())
    
    eig <- get_eigenvalue(mcaResult())
    df  <- as.data.frame(eig)
    
    # Move rownames (Dim.1, Dim.2, ...) into a proper column
    df$Dimension <- rownames(df)
    df <- df[, c("Dimension",
                 "eigenvalue",
                 "variance.percent",
                 "cumulative.variance.percent")]
    
    # Round numeric values
    df$eigenvalue                  <- round(df$eigenvalue, 3)
    df$variance.percent            <- round(df$variance.percent, 2)
    df$cumulative.variance.percent <- round(df$cumulative.variance.percent, 2)
    
    # Clean column names for display
    colnames(df) <- c("Dimension",
                      "Eigenvalue",
                      "Variance (%)",
                      "Cumulative variance (%)")
    
    df
  })
  
  # MCA Coordinates Table (rounded)
  output$mcaTable <- renderDT({
    req(mcaResult(), input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    mca_df <- round(mca_df, 3)
    datatable(mca_df, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
