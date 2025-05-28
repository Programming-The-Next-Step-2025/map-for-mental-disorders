#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import sf
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr filter group_by summarise left_join mutate %>% recode
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal coord_flip
#' @importFrom utils read.csv


# Load the dataset
dalys_path <- system.file("extdata", "mental_health_dalys_Europe.csv", package = "secondassignment")
dalys <- read.csv(dalys_path)
dalys <- dalys[nchar(dalys$location) < 50, ]


# Prepare map data
europe_map <- ne_countries(continent = "Europe", returnclass = "sf")

# Rename long country names to match map names
dalys$location <- recode(dalys$location,
                         "State of Israel" = "Israel",
                         "Republic of Moldova" = "Moldova",
                         "Republic of Austria" = "Austria",
                         "Grand Duchy of Luxembourg" = "Luxembourg",
                         "Republic of San Marino" = "San Marino",
                         "Republic of Poland" = "Poland",
                         "Bosnia and Herzegovina" = "Bosnia and Herz.",
                         "Principality of Monaco" = "Monaco",
                         "Republic of Cyprus" = "Cyprus",
                         "Kingdom of the Netherlands" = "Netherlands",
                         "Republic of Croatia" = "Croatia",
                         "Republic of Serbia" = "Serbia",
                         "Republic of Albania" = "Albania",
                         "Kingdom of Denmark" = "Denmark",
                         "Slovak Republic" = "Slovakia",
                         "Republic of Finland" = "Finland",
                         "Kingdom of Norway" = "Norway",
                         "Republic of Slovenia" = "Slovenia",
                         "Portuguese Republic" = "Portugal",
                         "French Republic" = "France",
                         "Republic of Estonia" = "Estonia",
                         "Swiss Confederation" = "Switzerland",
                         "Republic of Belarus" = "Belarus",
                         "Hellenic Republic" = "Greece",
                         "Kingdom of Spain" = "Spain",
                         "Federal Republic of Germany" = "Germany",
                         "Republic of Bulgaria" = "Bulgaria",
                         "Republic of Lithuania" = "Lithuania",
                         "Republic of Iceland" = "Iceland",
                         "Republic of Latvia" = "Latvia",
                         "Kingdom of Sweden" = "Sweden",
                         "Czech Republic" = "Czechia",
                         "Russian Federation" = "Russia",
                         "Republic of Italy" = "Italy",
                         "Principality of Andorra" = "Andorra",
                         "Republic of Malta" = "Malta",
                         "Kingdom of Belgium" = "Belgium"
)


dalys_map <- dalys %>%
  filter(age == "All ages", sex == "Both") %>%
  group_by(location) %>%
  summarise(total_dalys = sum(val, na.rm = TRUE))

map_data <- europe_map %>%
  left_join(dalys_map, by = c("name" = "location"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mental Health DALYs"),
  dashboardSidebar(
    selectInput("selected_location", "Select a country:", choices = unique(dalys$location))
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "DALY Bar Chart", status = "primary", solidHeader = TRUE, width = 6,
        textOutput("country_title"),
        plotOutput("daly_plot")
      ),
      box(
        title = "Map of Mental Health DALYs", status = "info", solidHeader = TRUE, width = 6,
        leafletOutput("daly_map", height = "500px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  selected_country <- reactiveVal("Netherlands")

  filtered_data <- reactive({
    data <- dalys %>%
      filter(location == selected_country(),
             age == "All ages",
             sex == "Both")

    if (nrow(data) == 0) {
      return(NULL)
    } else {
      return(data)
    }
  })

  output$country_title <- renderText({
    if (is.null(filtered_data())) {
      paste("No data available for", selected_country())
    } else {
      paste("DALYs per 100,000 for Mental Disorders for 2021 in", selected_country())
    }
  })

  output$daly_plot <- renderPlot({
    data <- filtered_data()

    if (is.null(data)) {
      plot.new()
      text(0.5, 0.5, "No data available for selected country.", cex = 1.5)
    } else {
      data <- data %>%
        mutate(percentage = val / sum(val, na.rm = TRUE) * 100)

      ggplot(data, aes(x = reorder(cause, val), y = val)) +
        geom_col(fill = "#4682B4") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")),
                  hjust = -0.1, size = 3.5) +
        labs(x = "Cause", y = "DALY rate per 100,000",
             title = paste("Mental Disorders in", selected_country())) +
        theme_minimal() +
        coord_flip()
    }
  })

  # Select from dropdown
  observeEvent(input$selected_location, {
    selected_country(input$selected_location)
  })

  # Select from map
  observeEvent(input$daly_map_shape_click, {
    clicked_country <- input$daly_map_shape_click$id
    if (!is.null(clicked_country) && clicked_country %in% dalys$location) {
      selected_country(clicked_country)
    }
  })

  # Sync dropdown with clicked country
  observe({
    updateSelectInput(session, "selected_location", selected = selected_country())
  })

  # Map rendering
  output$daly_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = map_data$total_dalys)

    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(total_dalys),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~paste0(name, "<br>",
                        "DALYs: ", round(total_dalys, 0), "<br>",
                        "Percent of max: ", round((total_dalys / max(total_dalys, na.rm = TRUE)) * 100, 1), "%"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        layerId = ~name
      ) %>%
      addLegend(pal = pal, values = ~total_dalys, opacity = 0.7,
                title = "DALYs per 100,000", position = "bottomright")
  })
}

# Run the app
#' Title
#'
#' @return
#' @export
#'
#' @examples
startApp <- function() {
  shinyApp(ui = ui, server = server)
}

startApp()
shinyApp(ui = ui, server = server)

#Title: Mental Health DALYs in Europe

#This interactive Shiny app visualizes the burden of mental health disorders
#across European countries using Disability-Adjusted Life Years (DALYs) per
#100,000 population for the year 2021.

# Users can:

# - Select a country to view a detailed bar chart showing the DALY rates for
#   specific mental health conditions such as anxiety, depression, bipolar disorder, and more.
#   Each condition is labeled with its percentage contribution to the country’s total mental health burden.
# - Explore a color-coded map of Europe that displays the total DALYs from mental
#   disorders by country, highlighting regional differences in mental health burdens.
#   Hovering over each country shows its DALY total and how it compares — as a percentage — to the most affected country.

# This app not only provides an accessible overview of mental health burdens
# across Europe, but also helps researchers, policymakers, and public health
# professionals identify countries that align with their area of interest. By
# visualizing and comparing DALY data, users can quickly locate regions with
# particularly high or low mental health burdens — supporting targeted research,
# policy development, and international collaboration.

