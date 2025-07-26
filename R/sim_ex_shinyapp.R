#' Shiny app to visualise and analyse processes
#'
#' @description
#' Launches a shiny app to visualise and analyse realisations of Brownian motion (see \code{\link{Bm}}),
#' Brownian bridge (see \code{\link{Bbridge}}), fractional Brownian motion (see \code{\link{FBm}}),
#' fractional Brownian bridge (see \code{\link{FBbridge}}), fractional Gaussian noise(see \code{\link{FGn}}),
#' Gaussian Haar-based multifractional processes (see \code{\link{GHBMP}}) and user-provided time series data.
#'
#' @return An interactive shiny app with the following user interface controls.
#'
#' **Hurst function estimation**
#' \describe{
#'   \item{\code{Hurst function}}{Input the Hurst function in terms of \code{t}.
#'   The default is set to \code{0.5+0*t}.}
#'   \item{\code{Time sequence}}{Input the time sequence which belongs to the interval \eqn{[0,1]}. The default is set to \code{seq(0,1,by=(1/2)^10)}.}
#'   \item{\code{J}}{Input or select a positive integer. For large \code{J} could be rather time consuming. Default is set to 15.}
#'   \item{\code{Number of sub-intervals for estimation}}{Default is set to 100.}
#'   \item{\code{Q}}{Input or select an integer greater than or equal to 2. Default is set to 2.}
#'   \item{\code{L}}{Input or select an integer greater than or equal to 2. Default is set to 2.}
#'   \item{\code{Hurst function to plot}}{Select: \code{Theoretical Hurst function}, \code{Raw estimate of Hurst function}, \code{Smoothed estimate of Hurst function}.}}
#' **Sojourn measure and excursion area**
#'  \describe{
#'   \item{\code{Number of time steps}}{Input the number of steps the time interval needs to be split into.}
#'   \item{\code{Constant level}}{Input the constant level.}
#'   \item{\code{Level}}{Select: \code{Greater}, \code{Lower}.}
#'   \item{}{Select: \code{Sojourn measure}, \code{Excursion area}.}}
#' **Longest streak**
#'  \describe{
#'   \item{}{Select: \code{Increasing}, \code{Decreasing}.}}
#' **Maximum and minimum**
#'  \describe{
#'   \item{}{Select: \code{Maximum}, \code{Minimum}.}}
#' @importFrom shiny runApp
#' @importFrom shinycssloaders withSpinner
#' @export shinyapp_sim
#'
#' @seealso \code{\link{Bm}}, \code{\link{FBm}}, \code{\link{FGn}},
#' \code{\link{Bbridge}}, \code{\link{FBbridge}}, \code{\link{GHBMP}}, \code{\link{Hurst}}
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
