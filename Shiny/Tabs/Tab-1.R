library(shiny)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DT)   # for DTOutput / renderDT

#NHANESraw <- read.csv("NHANESraw.csv")

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
        "This interface lets you explore the NHANESraw dataset using two multivariate methods:",
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
          "Use the sliders below to control point transparency and size."
        )
      ),
      hr(),
      
      h4("Plot Appearance"),
      sliderInput("alpha", "Point transparency (alpha):", min = 0, max = 1, value = 0.7),
      sliderInput("size",  "Point size:",              min = 0.05, max = 4.05, value = 1.56),
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
        tags$strong("PCA Summary (weights/contributions of each variable for each principal component):"),
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
        tags$strong("MCA Loadings (weights/contributions of each variable for each dimension):"), 
        tableOutput("loadingsTableMCA"),
        br(),
        tags$strong("MCA Eigenvalues (variance by dimension):"),
        verbatimTextOutput("mcaSummary"),
        br(),
        tags$strong("MCA Coordinates Table (rounded):"),
        DTOutput("mcaTable")
      )
    )
  )
)

#  Server
server <- function(input, output, session) {
  
  # Reactive: Load and validate data for PCA or MCA
  dataInput <- reactive({
    
    # PCA branch
    if (input$choice == "PCA") {
      req(NHANESraw, input$submit)
      
      # Keep only numeric columns for PCA
      numeric_df <- NHANESraw[sapply(NHANESraw, is.numeric)]
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
      return(X_df)
    }
    
    # MCA branch (still heavy, but structured)
    if (input$choice == "MCA") {
      req(NHANESraw, input$submit)
      
      # Treat both factor and character columns as categorical
      categorical_df <- NHANESraw[sapply(NHANESraw, function(x) is.factor(x) || is.character(x))]
      
      # Convert character columns to factors
      for (col in names(categorical_df)) {
        if (is.character(categorical_df[[col]])) {
          categorical_df[[col]] <- as.factor(categorical_df[[col]])
        }
      }
      
      # OPTIONAL: downsample rows to keep MCA tractable
      max_n <- 1000
      if (nrow(categorical_df) >  max_n) {
        categorical_df <- dplyr::slice_sample(categorical_df, n = max_n)
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
      
      return(categorical_df)
    }
    
  })
  
  # Reactive: Perform PCA
  pcaResult <- reactive({
    if (input$choice == "PCA") {
      req(dataInput(), input$submit)
      prcomp(dataInput(), scale. = TRUE)
    }
  })
  
  # Dynamically update PC selection inputs
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
    max_points <- 1000
    if (nrow(pca_df) >  max_points) {
      pca_df <- dplyr::slice_sample(pca_df, n = max_points)
    }
    
    ggplot(pca_df, aes_string(x = input$pcx, y = input$pcy)) +
      geom_point(
        aes(color = NHANESraw$SleepHrsNight[seq_len(nrow(pca_df))]),
        size  = input$size,
        alpha = input$alpha
      ) +
      geom_point(
        data = pca_df[is.na(NHANESraw$SleepHrsNight[seq_len(nrow(pca_df))]), ],
        aes(x = .data[[input$pcx]],
            y = .data[[input$pcy]],
            shape = "Missing"),
        size  = input$size,
        alpha = input$alpha,
        color="gray50"
      ) +
      scale_color_gradientn(
        colors=c("red", "blue"),
        na.value="gray50",
        name = "Sleep Hours (night)"
      ) + guides(color = guide_colorbar()) +
      theme_bw() +
      labs(
        title = "PCA Scatter Plot",
        x = input$pcx,
        y = input$pcy
      )
  })
  
  #PCA Variable Loadings
  output$loadingsTablePCA <- renderTable({
    req(pcaResult())
    loadings <- pcaResult()$rotation
    loads <- as.data.frame(round(loadings, 3))
    loads$Variable <- rownames(loadings)         
    loads <- loads[, c("Variable", colnames(loadings))] 
    loads
  })
  # PCA Summary
  output$pcaSummary <- renderPrint({
    req(pcaResult())
    summary(pcaResult())
  })
  
  # PCA Table (rounded)
  output$pcaTable <- renderDT({
    req(pcaResult(), input$submit)
    pca_df <- as.data.frame(pcaResult()$x)
    pca_df <- round(pca_df, 3)
    datatable(pca_df, options = list(pageLength = 10))
  })
  
  # Reactive: Perform MCA
  mcaResult <- reactive({
    if (input$choice == "MCA") {
      req(dataInput(), input$submit)
      MCA(dataInput(), ncp = 29)
    }
  })
  
  # Dynamically update Component selection input
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
  
  # MCA Plot (with .data[[ ]] usage and optional downsample for plotting)
  output$mcaPlot <- renderPlot({
    req(mcaResult(), input$mcx, input$mcy, input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    
    # Optional: downsample points further just for plotting
    max_points <- 1000
    if (nrow(mca_df)  > max_points) {
      mca_df <- dplyr::slice_sample(mca_df, n = max_points)
    }
    
    ggplot(
      mca_df,
      aes(x = .data[[input$mcx]], y = .data[[input$mcy]])
    ) +
      # main points (non-NA values get gradient)
      geom_point(
        aes(color = NHANESraw$SleepHrsNight[seq_len(nrow(mca_df))]),
        size  = input$size,
        alpha = input$alpha
      ) +
      # add NA points as a separate layer so we can give them a legend label
      geom_point(
        data = mca_df[is.na(NHANESraw$SleepHrsNight[seq_len(nrow(mca_df))]), ],
        aes(x = .data[[input$mcx]],
            y = .data[[input$mcy]],
            shape = "Missing"),
        size  = input$size,
        alpha = input$alpha,
        color = "gray50"
      ) +
      # color scale for non-NA
      scale_color_gradient(
        low  = "red",
        high = "blue",
        name = "Sleep Hours (night)",
        na.value = "gray50"   # ensures NA matches the gray point color
      ) +
      # shape scale creates a legend entry for NA
      scale_shape_manual(
        values = c("Missing" = 16),
        name   = "Sleep Hours (night)"
      )  +
      theme_bw() +
      labs(
        title = "MCA Scatter Plot",
        x = input$mcx,
        y = input$mcy
      )
  })
  #Loadings for MCA
  output$loadingsTableMCA<- renderTable({
    req(mcaResult())
    coords <- mcaResult()$var$coord     
    loads <- as.data.frame(round(coords, 3))
    loads$Category <- rownames(coords)
    loads <- loads[, c("Category", colnames(coords))]
    loads
  })
  
  # MCA Summary
  output$mcaSummary <- renderPrint({
    req(mcaResult())
    get_eigenvalue(mcaResult())
  })
  
  # MCA Table (rounded)
  output$mcaTable <- renderDT({
    req(mcaResult(), input$submit)
    mca_df <- as.data.frame(mcaResult()$ind$coord)
    mca_df <- round(mca_df, 3)
    datatable(mca_df, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
