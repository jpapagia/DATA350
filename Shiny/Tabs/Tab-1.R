#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Define Server
server <- function(input, output, session) {
    
    # Reactive: Load and validate data
    dataInput <- reactive({
    if (input$choice=="PCA") {
        req(NHANESraw, input$submit) 
        
        # Keep only numeric columns for PCA
            numeric_df <- NHANESraw[sapply(NHANESraw, is.numeric)]
            if (ncol(numeric_df) < 2) {
                showNotification("Need at least two numeric columns for PCA.", type = "error")
                return(NULL)
            }
          
        
            if (sum(is.na(numeric_df)) != 0) {
                for (col in names(numeric_df)) {
                    mean_val <- mean(numeric_df[[col]], na.rm = TRUE)
                    numeric_df[[col]][is.na(numeric_df[[col]])] <- mean_val
                }
            }
            X_df<-subset(numeric_df, select=-SleepHrsNight)
            return(X_df)
        }
            
            if (input$choice == "MCA") {
                 req(NHANESraw) 
                 
                 # Keep only categorical columns for MCA
                 categorical_df <- NHANESraw[sapply(NHANESraw, is.factor)]
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
                  
            if (input$choice=="Neither") {
                    showNotification("Not a Valid Option", type="error")
                    return(NULL)
             }
            
        })
        
        # Reactive: Perform PCA
        pcaResult <- reactive({
            if (input$choice=="PCA") {
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
            ggplot(pca_df, aes_string(x = input$pcx, y = input$pcy)) +
                geom_point(aes(color= numeric_df$SleepHrsNight),size=input$size, alpha=input$alpha) + scale_color_gradient(
                    low = "red",
                    high = "blue", 
                    name = "Sleep Hours Scale"  # Legend title
                ) + theme_bw() +
                labs(title = "PCA Scatter Plot",
                     x = input$pcx,
                     y = input$pcy)
        })
        
        # PCA Summary
        output$pcaSummary <- renderPrint({
            req(pcaResult())
            summary(pcaResult())
        })
        
        # PCA Table
        output$pcaTable <- renderDT({
            req(pcaResult(), input$submit)
            datatable(as.data.frame(pcaResult()$x), options = list(pageLength = 10))
        })
        # Reactive: Perform MCA
        mcaResult <- reactive({
            if (input$choice=="MCA") {
                req(dataInput(), input$submit)
                MCA(dataInput(), ncp=29)
            }
        })
        
        # Dynamically update Component selection input
          output$mcx_ui <- renderUI({
            req(mcaResult(), input$submit)
            dims <- seq_len(ncol(mcaResult()$ind$coord))
            dim_names <- paste0("Dim ", dims)
            selectInput("mcx", "X-Axis Dimension:", choices = dim_names, selected = "Dim 1")
        })
           output$mcy_ui <- renderUI({
               req(mcaResult(), input$submit)
               dims <- seq_len(ncol(mcaResult()$ind$coord))
               dim_names <- paste0("Dim ", dims)
               selectInput("mcy", "Y-Axis Dimension:", choices = dim_names, selected = "Dim 2")
           })
 
        
      # MCA Plot
        output$mcaPlot <- renderPlot({
            req(mcaResult(), input$mcx, input$mcy, input$submit)
            mca_df <- as.data.frame(mcaResult()$ind$coord)
            ggplot(mca_df, aes(x=mca_df[[input$mcx]],y=mca_df[[input$mcy]])) +
                geom_point(aes(color= numeric_df$SleepHrsNight),size=input$size, alpha=input$alpha) + scale_color_gradient(
                    low = "red",
                    high = "blue", 
                    name = "Sleep Hours Scale"  # Legend title
                ) + theme_bw() +
                labs(title = "MCA Scatter Plot",
                     x = input$mcx,
                     y = input$mcy)
        })
        
        # MCA Summary
        output$mcaSummary <- renderPrint({
            req(mcaResult())
            get_eigenvalue(mcaResult())
        })
        
        # MCA Table
        output$mcaTable <- renderDT({
            req(mcaResult(), input$submit)
            datatable(as.data.frame(mcaResult()$ind$coord), options = list(pageLength = 10))
        })
      }

# Run the app
shinyApp(ui = ui, server = server)

}

# Run the application 
shinyApp(ui = ui, server = server)
