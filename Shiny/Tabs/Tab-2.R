#
# Madhavan Narkeeran
#

library(shiny)
library(ggplot2)
library(dplyr)

NHANESraw <- read.csv("NHANESraw.csv")

cleaned <- NHANESraw |>
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

ui <- fluidPage(
  titlePanel("Sleep Hours per Night and Work Status"),
  
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
      p(textOutput("captionText"),
        style = "font-style: italic; font-size: 12px;"),
      width = 9
    )
  )
)

server <- function(input, output) {
  
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
    filtered <- cleaned |>
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
        axis.title.x   = element_text(face = "bold", size = 14),
        axis.title.y   = element_text(face = "bold", size = 14),
        legend.title   = element_text(face = "bold", size = 13),
        plot.margin    = margin(20, 40, 40, 30),
        legend.position = "right",
        legend.box     = "vertical"
      ) +
      coord_cartesian(clip = "off")
  })
}

shinyApp(ui = ui, server = server)
