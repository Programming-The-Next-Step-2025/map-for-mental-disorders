# map-for-mental-disorders

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#library(shiny)
#library(tidyverse)
#install.packages("shinydashboard")
#library(shinydashboard)

# Load your dataset
dalys <- read.csv("mental_health_dalys_Europe.csv")
dalys <- dalys[nchar(dalys$location) < 50, ]

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mental Health DALYs"),
  dashboardSidebar(
    selectInput("selected_location", "Select a country:",
                choices = sort(unique(dalys$location)),
                selected = "Netherlands")
  ),
  dashboardBody(
    h4(textOutput("country_title")),
    fluidRow(
      box(
        title = "DALYs for Mental Disorders",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput("daly_plot", height = "400px")
      )
    )
  )
)

# Server
server <- function(input, output) {

  filtered_data <- reactive({
    dalys %>%
      filter(location == input$selected_location,
             age == "All ages",
             sex == "Both")
  })

  output$country_title <- renderText({
    paste("DALYs per 100,000 for Mental Disorders for 2021 in", input$selected_location)
  })

  output$daly_plot <- renderPlot({
    data <- filtered_data()

    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected country.", cex = 1.5)
    } else {
      ggplot(data, aes(x = cause, y = val)) +
        geom_col(fill = "#4682B4") +
        labs(x = "Cause", y = "DALY rate per 100,000") +
        theme_minimal() +
        coord_flip()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
