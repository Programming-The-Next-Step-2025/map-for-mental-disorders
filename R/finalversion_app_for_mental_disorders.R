

# Load the dataset
dalys_path <- system.file("extdata", "mental_health_dalys_Europe.csv", package = "secondassignment")
dalys <- read.csv(dalys_path)
dalys <- dalys[nchar(dalys$location) < 50, ]
europe_map <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf")


# Rename long country names to match map names
dalys$location <- dplyr::recode(dalys$location,
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
dalys_map <- dalys |>
  dplyr::filter(age == "All ages", sex == "Both") |>
  dplyr::group_by(location) |>
  dplyr::summarise(total_dalys = sum(val, na.rm = TRUE))

map_data <- dplyr::left_join(europe_map, dalys_map, by = c("name" = "location"))


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
      data <- data %>%
        mutate(percentage = val / sum(val, na.rm = TRUE) * 100)

      ggplot(data, aes(x = reorder(cause, val), y = val)) +
        geom_col(fill = "#4682B4") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")),
                  hjust = -0.1, size = 3.5) +
        labs(x = "Cause", y = "DALY rate per 100,000",
             title = paste("Mental Disorders in", input$selected_location)) +
        theme_minimal() +
        coord_flip()
    }
  })

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

  observeEvent(input$daly_map_shape_click, {
    clicked_country <- input$daly_map_shape_click$id
    if (!is.null(clicked_country) && clicked_country %in% dalys$location) {
      updateSelectInput(session, "selected_location", selected = clicked_country)
    }
  })
}

#' @export
startApp <- function() {
  shinyApp(ui = ui, server = server)
}

