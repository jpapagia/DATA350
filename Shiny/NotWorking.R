ui <- fluidPage(
  
  titlePanel("Multiple Linear Regression with Dummy Variables"),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("response_ui"),
      uiOutput("predictor_ui"),
      
      checkboxInput("show_pred", "Enable Prediction", value = FALSE),
      uiOutput("newdata_ui")
    ),
    
    mainPanel(
      h3("Model Summary"),
      verbatimTextOutput("model_summary"),
      
      h3("Coefficient Table"),
      tableOutput("coeff_table"),
      
      conditionalPanel(
        condition = "input.show_pred == true",
        h3("Prediction for New Input"),
        verbatimTextOutput("prediction")
      )
    )
  )
)

server <- function(input, output) {
  
  # -------------------------
  # Read Data
  # -------------------------
  data_df <- reactive({
    req(NHANESraw)
    df<-NHANESraw
    numeric_df<-df[sapply(df, is.numeric)]
    print(numeric_df)
    column_means <- apply(numeric_df, 2, mean, na.rm = TRUE)
    if (sum(is.na(numeric_df)) != 0) {
      numeric_means <- mean(numeric_df, na.rm=TRUE)
      numeric_df[is.na(numeric_df)]<-numeric_means
    }
    print(numeric_df)
    # Convert character strings to factors
    #df[,sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], factor)
    categorical_df<-df[sapply(df, is.factor)]
    for (col in names(categorical_df)) {
      levels(categorical_df[[col]]) <- c(levels(categorical_df[[col]]), "No Response")
    }
    categorical_df[is.na(categorical_df)] <- "No Response"
    print(categorical_df)
    df_2<-cbind(categorical_df, numeric_df)
    print(df_2)
    return(df_2)
  })
  
  # -------------------------
  # UI: Response variable
  # -------------------------
  output$response_ui <- renderUI({
    req(data_df())
    selectInput("response", "Response Variable:", names(data_df()))
  })
  
  # -------------------------
  # UI: Predictor variables
  # -------------------------
  output$predictor_ui <- renderUI({
    req(data_df())
    selectInput("predictors", "Predictor Variables:",
                names(data_df()),
                multiple = TRUE)
  })
  
  # -------------------------
  # Reactive model matrix (dummy variables generated here)
  # -------------------------
  model_data <- reactive({
    req(input$response, input$predictors)
    df <- data_df()
    
    
    # Build formula for model.matrix (no intercept)
    form <- as.formula(
      paste("~", paste(input$predictors, collapse = "+"))
    )
    
    # Create dummy variables for predictors
    X <- model.matrix(form, data = df)[, -1, drop = FALSE]  # remove intercept
    
    # Response vector
    y <- df[[input$response]]
    
    list(X = X, y = y)
  })
  
  # -------------------------
  # Fit Linear Model
  # -------------------------
  model_fit <- reactive({
    dat <- model_data()
    lm(dat$y ~ dat$X)
  })
  
  # -------------------------
  # Outputs
  # -------------------------
  output$model_summary <- renderPrint({
    req(model_fit())
    summary(model_fit())
  })
  
  output$coeff_table <- renderTable({
    req(model_fit())
    coef(summary(model_fit()))
  }, rownames = TRUE)
  
  # -------------------------
  # Prediction Inputs (UI)
  # -------------------------
  output$newdata_ui <- renderUI({
    req(input$show_pred)
    req(input$predictors)
    
    df <- data_df()
    
    # Create inputs for each selected predictor
    lapply(input$predictors, function(var) {
      if (is.numeric(df[[var]])) {
        
        numericInput(paste0("pred_", var),
                     label = paste("Value for", var),
                     value = mean(df[[var]], na.rm = TRUE))
        
      } else {
        # categorical predictor
        selectInput(paste0("pred_", var),
                    label = paste("Category for", var),
                    choices = levels(df[[var]]))
      }
    })
  })
  
  # -------------------------
  # Prediction using dummy variables
  # -------------------------
  output$prediction <- renderPrint({
    req(model_fit())
    req(input$show_pred)
    
    df <- data_df()
    
    # Create a one-row newdata frame
    newrow <- data.frame(lapply(input$predictors, function(var) {
      input[[paste0("pred_", var)]]
    }))
    names(newrow) <- input$predictors
    
    # Match types (ensure factors have correct levels)
    for (v in input$predictors) {
      if (is.factor(df[[v]]))
        newrow[[v]] <- factor(newrow[[v]], levels = levels(df[[v]]))
    }
    
    # Recreate design matrix for prediction
    form <- as.formula(paste("~", paste(input$predictors, collapse = "+")))
    Xnew <- model.matrix(form, newrow)[, -1, drop = FALSE]
    
    # Predict
    predict(model_fit(), newdata = data.frame(X = Xnew))
  })
}

shinyApp(ui, server)