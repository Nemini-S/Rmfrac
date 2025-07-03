#' Shiny app to plot realisations of Gaussian Haar-based multifractional processes with theoretical and estimated Hurst functions
#'
#' @description
#' Launches a shiny app that plots the realisations of Gaussian Haar-based multifractional
#' processes simulated using the function \code{\link{GHBMP}} with theoretical
#' Hurst function, Hurst function estimated using \code{\link{Hurst}}
#' and the smoothed estimated Hurst function.
#'
#'
#' @return An interactive shiny app with the following user interface controls:
#' \describe{
#'   \item{\code{Hurst function}}{Input the Hurst function in terms of \code{t}.
#'   The default is set to \code{0.2+0*t}.}
#'   \item{\code{Time sequence}}{Input the time sequence which belongs to the interval \eqn{[0,1]}. The default is set to \code{seq(0,1,by=(1/2)^10)}.}
#'   \item{\code{J}}{Input or select a positive integer. For large \code{J} could be rather time consuming. Default is set to 15.}
#'   \item{\code{Number of sub-intervals for estimation}}{Default is set to 100.}
#'   \item{\code{Q}}{Input or select an integer greater than or equal to 2. Default is set to 2.}
#'   \item{\code{L}}{Input or select an integer greater than or equal to 2. Default is set to 2.}
#'   \item{\code{Hurst function to plot}}{Select: \code{Theoretical Hurst function}, \code{Raw estimate of Hurst function}, \code{Smoothed estimate of Hurst function}}}
#'
#' @importFrom shiny runApp
#' @importFrom shinycssloaders withSpinner
#' @export shinyapp_sim
#'
#' @seealso \code{\link{GHBMP}}, \code{\link{Hurst}}
#'
#' @examples
#' \dontrun{
#' #To run the shiny app
#' shinyapp_sim()
#' }
#'
#'
shinyapp_sim<- function() {
  appDir <- system.file("GHBMP_app", package = "Rmfrac")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Rmultifractional` package.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
