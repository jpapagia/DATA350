# app.R
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  
  titlePanel("PROJECT THEME TITLE GOES HERE"),
  
  sidebarLayout(
    sidebarPanel(
      h3("App Navigation"),
      p("Use the tabs on the right to explore the dashboard."),
      hr(),
      p("You can add global filters here later if needed.")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # -------------------------
        # SUMMARY TAB
        # -------------------------
        tabPanel(
          title = "Summary",
          
          h3("Data Summary & Key Findings"),
          p("Use this tab for dataset overview, key variables, and high-level summaries."),
          
          fluidRow(
            column(
              width = 4,
              h4("Summary inputs"),
              
              # TODO: update choices once your data is loaded (e.g., names(gss_sm))
              selectInput(
                inputId = "summary_var1",
                label   = "Variable 1:",
                choices = character(0)
              ),
              
              selectInput(
                inputId = "summary_var2",
                label   = "Variable 2:",
                choices = character(0)
              ),
              
              # REQUIRED BY INSTRUCTIONS: action button
              actionButton(
                inputId = "summary_go",
                label   = "Update summary"
              )
            ),
            
            column(
              width = 8,
              h4("Summary output"),
              verbatimTextOutput("summary_text"),
              tableOutput("summary_table")
              # You can also add a plotOutput here later if you want distributions
            )
          )
        ),
        
        # -------------------------
        # TAB 1
        # -------------------------
        tabPanel(
          title = "Tab 1",
          
          h3("Tab 1 Title (Owner: ___)"),
          p("Replace this text with a short description of your question / plot."),
          
          # TODO: replace this layout with your own inputs + plot
          textOutput("tab1_placeholder")
        ),
        
        # -------------------------
        # TAB 2
        # -------------------------
        tabPanel(
          title = "Tab 2",
          
          h3("Tab 2 Title (Owner: ___)"),
          p("Replace this text with a short description of your question / plot."),
          
          textOutput("tab2_placeholder")
        ),
        
        # -------------------------
        # TAB 3
        # -------------------------
        tabPanel(
          title = "Tab 3",
          
          h3("Tab 3 Title (Owner: ___)"),
          p("Replace this text with a short description of your question / plot."),
          
          textOutput("tab3_placeholder")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  summary_data <- eventReactive(input$summary_go, {
    # TODO: replace with real summary using your dataset
    head(mtcars)
  })
  
  output$summary_text <- renderPrint({
    cat("Summary output goes here.\n\nReplace this with your own explanation + stats.")
  })
  
  output$summary_table <- renderTable({
    summary_data()
  })
  
  # ----- TAB 1 PLACEHOLDER -----
  # Owner of Tab 1: replace this with your own inputs + plot(s)
  output$tab1_placeholder <- renderText({
    "Tab 1 content goes here. Replace me with your interactive plot and controls."
  })
  
  # ----- TAB 2 PLACEHOLDER -----
  output$tab2_placeholder <- renderText({
    "Tab 2 content goes here. Replace me with your interactive plot and controls."
  })
  
  # ----- TAB 3 PLACEHOLDER -----
  output$tab3_placeholder <- renderText({
    "Tab 3 content goes here. Replace me with your interactive plot and controls."
  })
}

# RUN APP
shinyApp(ui = ui, server = server)
