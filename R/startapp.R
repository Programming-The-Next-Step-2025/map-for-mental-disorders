#' Launch the Mental Health DALYs Shiny App
#'
#' @import shiny
#' @import leaflet
#' @import sf
#' @import rnaturalearth
#' @import dplyr
#' @import ggplot2
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @export
startApp <- function() {

  # usethis::use_data()
  # Load CSV from package
  dalys_path <- system.file("extdata", "mental_health_dalys_Europe.csv", package = "yourpackagename")
  if (dalys_path == "") stop("CSV file not found. Make sure it is in inst/extdata/")

  dalys <- read.csv(dalys_path)
  dalys <- dalys[nchar(dalys$location) < 50, ]
  # Fix names
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

  europe_map <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf")

  dalys_map <- dalys %>%
    dplyr::filter(age == "All ages", sex == "Both") %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(total_dalys = sum(val, na.rm = TRUE))

  map_data <- dplyr::left_join(europe_map, dalys_map, by = c("name" = "location"))

  # UI
  ui <- dashboardPage(
    dashboardHeader(title = "Mental Health DALYs"),
    dashboardSidebar(
      selectInput("selected_location", "Select a country:",
                  choices = sort(unique(dalys$location)),
                  selected = "Netherlands")
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
        dplyr::filter(location == selected_country(),
                      age == "All ages",
                      sex == "Both")
      if (nrow(data) == 0) NULL else data
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
          dplyr::mutate(percentage = val / sum(val, na.rm = TRUE) * 100)

        ggplot2::ggplot(data, ggplot2::aes(x = reorder(cause, val), y = val)) +
          ggplot2::geom_col(fill = "#4682B4") +
          ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage, 1), "%")),
                             hjust = -0.1, size = 3.5) +
          ggplot2::labs(x = "Cause", y = "DALY rate per 100,000",
                        title = paste("Mental Disorders in", selected_country())) +
          ggplot2::theme_minimal() +
          ggplot2::coord_flip()
      }
    })

    observeEvent(input$selected_location, {
      selected_country(input$selected_location)
    })

    observeEvent(input$daly_map_shape_click, {
      clicked_country <- input$daly_map_shape_click$id
      if (!is.null(clicked_country) && clicked_country %in% dalys$location) {
        selected_country(clicked_country)
      }
    })

    observe({
      updateSelectInput(session, "selected_location", selected = selected_country())
    })

    output$daly_map <- renderLeaflet({
      pal <- leaflet::colorNumeric("YlOrRd", domain = map_data$total_dalys)
      leaflet::leaflet(map_data) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          fillColor = ~pal(total_dalys),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = ~paste0(name, "<br>",
                          "DALYs: ", round(total_dalys, 0), "<br>",
                          "Percent of max: ", round((total_dalys / max(total_dalys, na.rm = TRUE)) * 100, 1), "%"),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          layerId = ~name
        ) %>%
        leaflet::addLegend(pal = pal, values = ~total_dalys, opacity = 0.7,
                           title = "DALYs per 100,000", position = "bottomright")
    })
  }

  shinyApp(ui = ui, server = server)
}

