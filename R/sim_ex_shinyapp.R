#' Shiny app for simulation of Gaussian Haar-based multifractional processes
#' @description
#' Plots
#'
#' @return Plor
#' @importFrom shiny runApp
#' @export shinyapp_sim
#'
#' @examples
#' shinyapp_sim()
shinyapp_sim<- function() {
  appDir <- system.file("GHBMP_app", package = "Rmultifractional")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Rmultifractional` package.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
