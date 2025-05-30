#' Launch the Mental Health DALYs App
#'
#' @export
startApp <- function() {
  shinyApp(ui = ui, server = server)
}

