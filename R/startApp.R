#' @return Launches the mental health DALYs Shiny app
#' @export
startApp <- function() {
  shiny::shinyApp(ui = ui, server = server)
}
