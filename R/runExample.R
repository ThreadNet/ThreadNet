#' @export
runExample <- function() {
  appDir <- system.file("ThreadNet", package = "ThreadNet")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `ThreadNet`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
