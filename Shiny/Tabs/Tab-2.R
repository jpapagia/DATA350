#
# Madhavan Narkeeran
#


library(shiny)
library(ggplot2)
library(dplyr)

NHANESraw <- read.csv("NHANESraw.csv")

cleaned <- NHANESraw |> 
  filter(!is.na(Gender), !is.na(Education), !is.na(HomeOwn), !is.na(Work), !is.na(SleepHrsNight)) |>
  select(Gender, Education, HomeOwn, Work, SleepHrsNight)

ui <- fluidPage(
  titlePanel("Sleep Hours per Night and Work Status"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      
      radioButtons(
        "gender", 
        "Select Gender:",
        choices = c("male", "female"),
        selected = "male"
      ),
      
      radioButtons(
        "education", 
        "Select Education:", 
        choices = c("9 - 11th Grade", "High School", "Some College", "College Grad"),
        selected = "9 - 11th Grade"
      ),
      
      radioButtons(
        "homeown", 
        "Select Home Ownership:",
        choices = c("Own", "Rent"),
        selected = "Own"
      )
    ),
    
    mainPanel(
      p("This dashboard shows the Distribution of Sleep Hours per Night and how it differs by Work Status. Users can toggle the Gender, Education Level, and Home Ownnership Status for comparison."),
      plotOutput("densityPlot")
    )
  )
)

server <- function(input, output) {
  
  output$densityPlot <- renderPlot({
    filtered <- cleaned |>
      filter(
        Gender == input$gender,
        Education == input$education,
        HomeOwn == input$homeown,
        Work %in% c("Working", "NotWorking")
      )
    
    plot_title <- paste(
      "Distribution of Sleep Hours per Night for",
      input$gender,
      "with Education \nlevel", input$education,
      "who", input$homeown, "their Home and how it varies by Work Status"
    )
    
    ggplot(filtered, aes(
      x = SleepHrsNight,
      color = Work
    )) +
      geom_density(alpha = 0.25, linewidth = 1, adjust = 2) +
      scale_color_manual(
        name = "Work Status",
        values = c("Working" = "blue", "NotWorking" = "red"),
        labels = c("Working" = "Working",
                   "NotWorking" = "Not Working")
      ) +
      scale_x_continuous(breaks = 2:12, limits = c(2, 12)) +
      scale_y_continuous(limits = c(0, 0.7)) +
      labs(
        title = plot_title,
        x = "Sleep Hours per Night",
        y = "Density",
        color = "Work Status"
      ) +
      theme_bw(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)

