#' @title ThreadNet_Core
#' @description This function launches the Shiny App called ThreadNet
#' @name ThreadNet_Core
#' @export ThreadNet
 ThreadNet <- function() { shiny::runApp(system.file('ThreadNet', package='ThreadNet')) }
