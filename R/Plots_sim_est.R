#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @export
autoplot.mp <- function(object, ..., H = NULL, H_Est = TRUE, H_Smooth_Est = TRUE,LFD_Est = TRUE, LFD_Smooth_Est = TRUE, N = 100, Q = 2, L = 2)
{

  if (!is.logical(H_Est) | !is.logical(H_Smooth_Est) | !is.logical(LFD_Est) | !is.logical(LFD_Smooth_Est)) {
    stop("H_Est, H_Smooth_Est, LFD_Est and LFD_Smooth_Est should have logical inputs either TRUE or FALSE")
  }

  object <- object[order(object[,1]),]
  H_est <- Hurst(object, N, Q, L)
  colnames(H_est)  <- c("x","y")
  colnames(object) <- c("t1", "PP")
  t1<-object[,1]

  LFD_EST <- LFD(object, N, Q, L)
  colnames(LFD_EST) <- c("x1", "y1")

  p <- ggplot(object, aes(x =.data$t1, y =.data$PP)) + geom_line() +
       labs(y = "X(t)", x = "t",color = "")

  if(H_Est){
    p <- p + geom_line(data = H_est, aes(x =.data$x, y =.data$y, color = factor("Raw Estimate H")), linewidth = 1)

  }

  if(H_Smooth_Est){
    p <- p + geom_smooth(data = H_est, aes(x = .data$x, y = .data$y, color = factor("Smoothed Estimate H")),
                         method = "loess", se = FALSE, span = 0.3, linewidth = 1)

  }

  if (!is.null(H)){

    H1 <- sapply(t1, H)
    data1 <- data.frame(t1, H1) #Data for the theoretical Hurst function

    if (!is.numeric(H1) | !all(H1 >= 0 & H1 <= 1)) {
      stop("H must be a function which returns a numeric list between 0 and 1")
    }

    p <- p + geom_line(data = data1, aes(x = .data$t1, y = .data$H1, color = factor("Theoretical H")), linewidth = 1)


  }

  if(LFD_Est){

    p <- p + geom_line(data = LFD_EST, aes(x = .data$x1, y = .data$y1, color = factor("Raw Estimate LFD")), linewidth = 1)

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x = .data$x1, y =.data$y1, color = factor("Smoothed Estimate LFD"))
                         , method = "loess", se = FALSE, span = 0.3, linewidth = 1)

  }

}


#' Plot Gaussian Haar-based multifractional processes with their
#' theoretical and estimated Hurst functions and local fractal dimension
#'
#' @description
#' Creates a plot of the Gaussian Haar-based multifractional process
#' simulated by using \code{\link{GHBMP}} with theoretical Hurst function (if provided),
#' Hurst function estimated using \code{\link{Hurst}}, the smoothed estimated
#' Hurst function and local fractal dimension estimated using \code{\link{LFD}}
#' and smoothed estimates of local fractal dimension.
#'
#' @param x Return from \code{\link{GHBMP}}. To get reliable results for simulated trajectories, it is recommended
#' to use at least 500 time points.
#' @param H Theoretical Hurst function. Optional: If provided, the theoretical Hurst function is plotted.
#' @param H_Est Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param H_Smooth_Est Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param N Argument used for the estimation of Hurst functions and LFD. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param ... Other arguments.
#' @return A ggplot object which is used to plot the multifractional process with theoretical, raw and smoothed estimates of Hurst function
#' and raw and smoothed estimates of local fractal dimension.
#' @exportS3Method Rmfrac::plot
#'
#' @importFrom graphics plot
#' @seealso \code{\link{GHBMP}}, \code{\link{Hurst}}, \code{\link{LFD}}
#'
#' @examples
#' \donttest{
#' #Simulation of the multifractional process and estimation of the Hurst function
#' T <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.5 - 0.4 * sin(6 * 3.14 * t))}
#' X <- GHBMP(T, H)
#'
#' #Plot of process, theoretical Hurst function, estimated and smoothed Hurst and LFD estimates
#' plot(X, H = H)
#'
#' #Plot of process, estimated and smoothed Hurst and LFD estimates
#' plot(X)
#' }
plot.mp <- function(x, H = NULL, H_Est = TRUE, H_Smooth_Est = TRUE, LFD_Est = TRUE, LFD_Smooth_Est = TRUE, N = 100, Q = 2,L = 2, ...) {

  pp <- autoplot(x, H = H, H_Est = H_Est, H_Smooth_Est = H_Smooth_Est, LFD_Est = LFD_Est, LFD_Smooth_Est = LFD_Smooth_Est, N = N, Q = Q, L = L)

  if (interactive()) {
    print(pp)
  }

  invisible(pp)
}

#' Creates objects of class \code{H_LFD}
#' @description
#' For user provided time series creates objects of class \code{"H_LFD"} with the
#' Hurst function estimated using \code{\link{Hurst}}, local fractal dimension
#' estimated using \code{\link{LFD}} and smoothed estimated Hurst function and LFD added.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' To get reliable results, it is recommended to use at least 500 time points.
#' @param N Argument used for the estimation of Hurst functions and LFD. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#' @return The return from \code{\link{H_LFD}} is an object list of class \code{"H_LFD"} with the following components:
#' \describe{
#' \item{\code{Raw_Hurst_estimates}}{A data frame of where the first column is a time sequence and second column is estimated values of the Hurst function.}
#' \item{\code{Smoothed_Hurst_estimates}}{A data frame of where the first column is a time sequence and second column is smoothed estimates of the Hurst function.}
#' \item{\code{Raw_LFD_estimates}}{A data frame of where the first column is a time sequence and second column is Local fractal dimension estimates.}
#' \item{\code{Smoothed_LFD_estimates}}{A data frame of where the first column is a time sequence and second column is smoothed estimates of Local fractal dimension.}
#' \item{\code{Data}}{User provided time series.}}
#' @export H_LFD
#' @seealso \code{plot.H_LFD} \code{\link{Hurst}}, \code{\link{LFD}}
#' @examples
#' TS <- data.frame("t"=seq(0,1,length = 1000),"X(t)" = rnorm(1000))
#' Object <- H_LFD(TS)
#' #Plot of time series, estimated and smoothed Hurst and LFD estimates
#' plot(Object)
#'
H_LFD <- function(X, N = 100, Q = 2, L = 2){

  X<-na.omit(X)

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0))) {
    stop("X must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  X <- X[order(X[,1]),]

  H_est <- Hurst(X, N, Q, L)

  smoothed_H_est <- data.frame(time = H_est[,1], smoothed_Hurst = (loess(H_est[,2] ~ H_est[,1], data = H_est, span = 0.3))$fitted)

  smoothed_H_est[,2] <- pmax(pmin(smoothed_H_est[,2], 1), 0)

  LFD_est <- LFD(X, N, Q, L)

  LFD_smoothed_est <- data.frame(time = LFD_est[,1], smoothed_LFD = (loess(LFD_est[,2]~LFD_est[,1], data = LFD_est, span = 0.3))$fitted)

  structure(list(Raw_Hurst_estimates = H_est, Smoothed_Hurst_estimates = smoothed_H_est, Raw_LFD_estimates = LFD_est,Smoothed_LFD_estimates = LFD_smoothed_est, Data = X),class = "H_LFD")

}

#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes sec_axis scale_y_continuous
#' @importFrom rlang .data
#' @importFrom stats IQR quantile
#' @export
autoplot.H_LFD<-function(object, ..., H_Est = TRUE, H_Smooth_Est = TRUE, LFD_Est = TRUE, LFD_Smooth_Est = TRUE){

  if (!is.logical(H_Est) | !is.logical(H_Smooth_Est) | !is.logical(LFD_Est) | !is.logical(LFD_Smooth_Est)) {
    stop("H_Est, H_Smooth_Est, LFD_Est and LFD_Smooth_Est should have logical inputs either TRUE or FALSE")
  }

  X <- object$Data
  H_est <- object$Raw_Hurst_estimates

  X <- na.omit(X)
  X <- X[order(X[,1]),]
  colnames(H_est) <- c("x", "y")
  colnames(X) <- c("t1", "PP")
  t1 <- X[,1]

  LFD_EST <- object$Raw_LFD_estimates
  colnames(LFD_EST) <- c("x1", "y1")

  IQR_H <- IQR(X[,2])
  range_H <- range(X[,2])
  q1_H <-quantile(X[,2], 0.25)

  IQR_L <- IQR(X[,2])
  range_L <- range(X[,2])
  q1_L <- quantile(X[,2], 0.25)

  p <- ggplot(X, aes(x = .data$t1, y = .data$PP)) + geom_line() +
       labs(y = "Time series", x = "Time", color = "")


  if(H_Est){
    p <- p + geom_line(data = H_est, aes(x = .data$x, y = ((.data$y * (IQR_H)) + q1_H), color = factor("Raw Estimate H")), linewidth = 1)+
             scale_y_continuous(name = "Time Series", limits = range_H,
                       sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))


  }

  if(H_Smooth_Est){
    p <- p + geom_smooth(data = H_est, aes(x = .data$x,y=((.data$y*(IQR_H))+q1_H),color = factor("Smoothed Estimate H"))
                         ,method = "loess", se=FALSE, span = 0.3, linewidth = 1) +
             scale_y_continuous(name = "Time Series", limits = range_H,
                         sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))

  }

  if(LFD_Est){

    p <- p + geom_line(data = LFD_EST, aes(x = .data$x1, y = ((.data$y1 * (IQR_L)) + q1_L),color = factor("Raw Estimate LFD")), linewidth = 1)+
             scale_y_continuous(name = "Time Series",limits = range_L,
                         sec.axis = sec_axis(transform = function(x){(x - q1_L) / (IQR_L)},name = "Estimator"))

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x = .data$x1, y = ((.data$y1 * (IQR_L)) + q1_L), color = factor("Smoothed Estimate LFD"))
                         , method = "loess", se = FALSE, span = 0.3, linewidth = 1) +
             scale_y_continuous(name = "Time Series", limits = range_L,
                         sec.axis = sec_axis(transform = function(x){(x - q1_L) / (IQR_L)}, name = "Estimator"))
  }

}


#' Plot the estimated Hurst functions and local fractal dimension estimates
#' for objects of class \code{H_LFD}
#'
#' @description
#' Creates a plot of the user provided time series with the
#' Hurst function estimated using \code{\link{Hurst}}, the smoothed estimated
#' Hurst function and local fractal dimension estimated using \code{\link{LFD}}
#' for objects of class \code{"H_LFD"}.
#'
#' @param x Return from \code{\link{H_LFD}}.
#' @param H_Est Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param H_Smooth_Est Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param ... Other arguments.
#' @return A ggplot object which is used to plot the time series with theoretical, raw and smoothed estimates of Hurst function
#' and raw and smoothed estimates of local fractal dimension.
#' @details Compared to \code{\link{plot_tsest}}, the function's argument is a \code{"H_LFD"}
#' object, not a time series.
#' @exportS3Method Rmfrac::plot
#' @importFrom graphics plot
#' @seealso \code{H_LFD} \code{\link{Hurst}}, \code{\link{LFD}}, \code{\link{plot_tsest}}
#'
#' @examples
#' TS <- data.frame("t" = seq(0, 1, length = 1000), "X(t)" = rnorm(1000))
#' Object <- H_LFD(TS)
#' #Plot of time series, estimated and smoothed Hurst and LFD estimates
#' plot(Object)
#'
plot.H_LFD <- function(x, H_Est = TRUE,H_Smooth_Est = TRUE, LFD_Est = TRUE, LFD_Smooth_Est = TRUE,...) {

  pp <- (autoplot(x, H_Est = H_Est, H_Smooth_Est = H_Smooth_Est, LFD_Est = LFD_Est, LFD_Smooth_Est = LFD_Smooth_Est))

  if (interactive()) {
    print(pp)
  }

  invisible(pp)
}


#' Plot the estimated Hurst functions and local fractal dimension estimates
#' for a user provided time series
#'
#' @description
#' Creates a plot of the user provided time series with the
#' Hurst function estimated using \code{\link{Hurst}}, the smoothed estimated
#' Hurst function and local fractal dimension estimated using \code{\link{LFD}}
#' and smoothed estimates of local fractal dimension.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' To get reliable results for time series, it is recommended to use at least 500 time points.
#' @param H_Est Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param H_Smooth_Est Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param N Argument used for the estimation of Hurst functions and LFD. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions and LFD. Fixed integer greater than or equal to 2. Default is set to 2.
#'
#' @return A ggplot object which is used to plot the time series with theoretical, raw and smoothed estimates of Hurst function
#' and raw and smoothed estimates of local fractal dimension.
#' @details
#' Compared to \code{\link{plot.H_LFD}} the function's first argument is a time series,
#' not \code{H_LFD} object.
#'
#' @importFrom ggplot2 ggplot geom_line geom_smooth scale_color_manual labs aes sec_axis scale_y_continuous
#' @importFrom rlang .data
#' @importFrom stats IQR quantile
#' @seealso \code{\link{Hurst}}, \code{\link{LFD}}, \code{\link{plot.H_LFD}}
#' @export plot_tsest
#'
#' @examples
#' TS <- data.frame("t" = seq(0, 1, length = 1000), "X(t)" = rnorm(1000))
#' #Plot of time series, estimated and smoothed Hurst and LFD estimates
#' plot_tsest(TS)
#'
plot_tsest<-function(X, H_Est = TRUE, H_Smooth_Est = TRUE, LFD_Est = TRUE, LFD_Smooth_Est = TRUE, N = 100, Q = 2, L = 2){

  X <- na.omit(X)

  if (!is.logical(H_Est) | !is.logical(H_Smooth_Est) | !is.logical(LFD_Est) | !is.logical(LFD_Smooth_Est)) {
    stop("H_Est, H_Smooth_Est, LFD_Est and LFD_Smooth_Est should have logical inputs either TRUE or FALSE")
  }

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0))) {
    stop("X must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  X <- X[order(X[,1]),]
  H_est <- Hurst(X, N, Q, L)
  colnames(H_est) <- c("x", "y")
  colnames(X) <- c("t1", "PP")
  t1 <- X[,1]

  LFD_EST <- LFD(X, N, Q, L)
  colnames(LFD_EST) <- c("x1", "y1")

  IQR_H <- IQR(X[,2])
  range_H <- range(X[,2])
  q1_H <- quantile(X[,2], 0.25)

  IQR_L <- IQR(X[,2])
  range_L <- range(X[,2])
  q1_L <- quantile(X[,2], 0.25)

  p <- ggplot(X, aes(x = .data$t1, y = .data$PP)) + geom_line() +
       labs(y = "Time series", x = "Time", color = "")


  if(H_Est){
    p <- p + geom_line(data = H_est, aes(x = .data$x, y = ((.data$y * (IQR_H)) + q1_H), color = factor("Raw Estimate H")), linewidth = 1) +
             scale_y_continuous(name = "Time Series", limits = range_H,
                         sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))

  }

  if(H_Smooth_Est){
    p <- p + geom_smooth(data = H_est, aes(x = .data$x, y = ((.data$y * (IQR_H)) + q1_H), color = factor("Smoothed Estimate H"))
                         , method = "loess", se = FALSE, span = 0.3, linewidth = 1) +
             scale_y_continuous(name = "Time Series", limits = range_H,
                         sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name="Estimator"))

  }

  if(LFD_Est){


    p <- p + geom_line(data = LFD_EST, aes(x = .data$x1, y = ((.data$y1 * (IQR_H)) + q1_H), color = factor("Raw Estimate LFD")), linewidth = 1) +
      scale_y_continuous(name = "Time Series", limits=range_H,
                         sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)}, name = "Estimator"))

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x =.data$x1,y=((.data$y1*(IQR_H))+q1_H),color = factor("Smoothed Estimate LFD"))
                         , method="loess", se = FALSE,span = 0.3, linewidth = 1) +
      scale_y_continuous(name = "Time Series", limits = range_H,
                         sec.axis = sec_axis(transform = function(x){(x - q1_H) / (IQR_H)} ,name = "Estimator"))
  }

  if(interactive()){
    print(p)
  }

  invisible(p)

}

