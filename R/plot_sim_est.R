#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @export
autoplot.mp<-function(object,...,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE,N=100,Q=2,L=2)
{

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H) | !is.logical(LFD_Est)) {
    stop("Raw_Est_H, Smooth_Est_H and LFD_EST should have logical inputs either TRUE or FALSE")
  }

  object<-object[order(object[,1]),]
  H_est<-Hurst(object,N,Q,L)
  colnames(H_est)<-c("x","y")
  colnames(object) <- c("t1","PP")
  t1<-object[,1]

  LFD_EST<-LFD(object,N,Q,L)
  colnames(LFD_EST)<-c("x1","y1")

  p <- ggplot(object, aes(x =.data$t1, y =.data$PP))+geom_line()+
    scale_color_manual(name = "",breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H","Raw Estimate LFD","Smoothed Estimate LFD"),
                       values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green","Raw Estimate LFD"="cyan","Smoothed Estimate LFD"="brown"))+
    labs(y="X(t)",x="t")

  if (!is.null(H)){

    H1<-sapply(t1,H)
    data1<-data.frame(t1,H1) #Data for the theoretical Hurst function

    if (!is.numeric(H1) | !all(H1 >= 0 & H1<= 1)) {
      stop("H must be a function which returns a numeric list between 0 and 1")
    }

    p <- p + geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))


  }

  if(Raw_Est_H){
    p <- p + geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H"))

  }

  if(Smooth_Est_H){
    p <- p + geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  if(LFD_Est){

    p <- p + geom_line(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Raw Estimate LFD"))

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Smoothed Estimate LFD")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  print(p)
}


#' Plot Gaussian Haar-based multifractional processes with their
#' theoretical and estimated Hurst functions and local fractal dimension estimates
#'
#' @description
#' Creates a plot of the Gaussian Haar-based multifractional process
#' simulated by using \code{\link{GHBMP}} with theoretical Hurst function (if provided),
#' Hurst function estimated using \code{\link{Hurst}}, the smoothed estimated
#' Hurst function and local fractal dimension estimated using \code{\link{LFD}}
#' and smoothed estimates of local fractal dimension.
#'
#' @param x Return from \code{\link{GHBMP}}. For accurate estimated Hurst functions, \code{x} should be of at least 500 data points.
#' @param H Theoretical Hurst function. Optional: If provided, the theoretical Hurst function is plotted.
#' @param Raw_Est_H Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param N Argument used for the estimation of Hurst functions. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param ... Other arguments.
#' @return A ggplot object which plots the multifractional process with theoretical, raw and smoothed estimates of Hurst function
#' and raw and smoothed estimates of local fractal dimension.
#' @exportS3Method Rmfrac::plot
#'
#' @importFrom graphics plot
#' @seealso \code{\link{GHBMP}}, \code{\link{Hurst}}, \code{\link{LFD}}
#'
#' @examples
#' #Simulation of the multifractional process and estimation of the Hurst function
#' T <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X <- GHBMP(T,H)
#'
#' #Plot of process, theoretical, estimated and smoothed Hurst functions and LFD estimate
#' plot(X,H=H)
#'
#' #Plot of process, estimated and smoothed Hurst functions and LFD estimate
#' plot(X)
plot.mp <- function(x,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE,N=100,Q=2,L=2,...) {

  print(autoplot(x,H=H,Raw_Est_H=Raw_Est_H,Smooth_Est_H=Smooth_Est_H,LFD_Est=LFD_Est,LFD_Smooth_Est=LFD_Smooth_Est,N=N,Q=Q,L=L))

}


#' @param X Data frame where the first column is a time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' For accurate estimated Hurst functions, \code{X} should be of at least 500 data points.
#' @param N Argument used for the estimation of Hurst functions. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param ... Other arguments.
#' @return The return from \code{\link{H_LFD}} will be an object list of class \code{"H_LFD"} with the following components:
#' \describe{
#' \item{Raw_Hurst_estimates}{A data frame of where the first column is a time sequence and second column is estimated values of the Hurst function.}
#' \item{Smoothed_Hurst_estimates}{A data frame of where the first column is a time sequence and second column is smoothed estimates of the Hurst function.}
#' \item{LFD_estimates}{A data frame of where the first column is a time sequence and second column is Local fractal dimension estimates.}
#' \item{LFD_Smoothed_estimates}{A data frame of where the first column is a time sequence and second column is smoothed estimates of Local fractal dimension.}
#' \item{Data}{User provided time series.}}
#' @export H_LFD
#' @rdname TS_plot
#'
H_LFD <- function(X,N=100,Q=2,L=2){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0))) {
    stop("X must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  X <- X[order(X[,1]),]

  H_est <- Hurst(X,N,Q,L)

  smoothed_H_est <- data.frame(time=H_est[,1],smoothed_Hurst=(loess(H_est[,2]~H_est[,1],data=H_est,span=0.3))$fitted)

  LFD_est <- LFD(X,N,Q,L)

  LFD_smoothed_est <- data.frame(time=LFD_est[,1],smoothed_LFD=(loess(LFD_est[,2]~LFD_est[,1],data=LFD_est,span=0.3))$fitted)

  structure(list(Raw_Hurst_estimates=H_est,Smoothed_Hurst_estimates=smoothed_H_est,LFD_estimates=LFD_est,LFD_Smoothed_estimates=LFD_smoothed_est,Data=X),class="H_LFD")

}

#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @export
autoplot.H_LFD<-function(object,...,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE){

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H) | !is.logical(LFD_Est)) {
    stop("Raw_Est_H, Smooth_Est_H and LFD_EST should have logical inputs either TRUE or FALSE")
  }

  X <- object$Data
  H_est <- object$Raw_Hurst_estimates

  X<-X[order(X[,1]),]
  colnames(H_est)<-c("x","y")
  colnames(X) <- c("t1","PP")
  t1<-X[,1]

  LFD_EST <- object$LFD_estimates
  colnames(LFD_EST) <- c("x1","y1")

  p <- ggplot(X, aes(x =.data$t1, y =.data$PP))+geom_line()+
    scale_color_manual(name = "",breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H","Raw Estimate LFD","Smoothed Estimate LFD"),
                       values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green","Raw Estimate LFD"="cyan","Smoothed Estimate LFD"="brown"))+
    labs(y="X(t)",x="t")


  if(Raw_Est_H){
    p <- p + geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H"))

  }

  if(Smooth_Est_H){
    p <- p + geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  if(LFD_Est){

    p <- p + geom_line(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Raw Estimate LFD"))

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Smoothed Estimate LFD")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  print(p)
}



#' Plot the estimated Hurst functions and local fractal dimension estimates
#' for objects of class \code{"H_LFD"}
#'
#' @description
#' Creates a plot of the user provided time series with the
#' Hurst function estimated using \code{\link{Hurst}}, the smoothed estimated
#' Hurst function and local fractal dimension estimated using \code{\link{LFD}}
#' for objects of class \code{"H_LFD"}.
#'
#' @param x Return from \code{\link{H_LFD}}.
#' @param Raw_Est_H Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param ... Other arguments.
#'
#' @exportS3Method Rmfrac::plot
#' @rdname TS_plot
#' @importFrom graphics plot
#' @seealso \code{\link{Hurst}}, \code{\link{LFD}}, \code{\link{plot_ts_est}}
#'
#' @examples
#' TS <- data.frame("t"=seq(0,1,length=1000),"X(t)"=rnorm(1000))
#' Object <- H_LFD(TS,N=100,Q=2,L=2)
#' plot(Object,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_EST=TRUE,LFD_Smooth_Est=TRUE)
#'
plot.H_LFD <- function(x,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE,...) {

  print(autoplot(x,Raw_Est_H=Raw_Est_H,Smooth_Est_H=Smooth_Est_H,LFD_Est=LFD_Est,LFD_Smooth_Est=LFD_Smooth_Est))

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
#' @param X Data frame where the first column is a time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' For accurate estimated Hurst functions, \code{X} should be of at least 500 data points.
#' @param Raw_Est_H Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param LFD_Est Logical: If \code{TRUE}, the local fractal dimension estimates are plotted.
#' @param LFD_Smooth_Est Logical: If \code{TRUE}, the smoothed estimates of local fractal dimension is plotted.
#' @param N Argument used for the estimation of Hurst functions. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#'
#' @return A ggplot object which plots the time series with theoretical, raw and smoothed estimates of Hurst function
#' and raw and smoothed estimates of local fractal dimension.
#' @importFrom ggplot2 ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @seealso \code{\link{Hurst}}, \code{\link{LFD}}
#' @export plot_ts_est
#'
#' @examples
#' TS <- data.frame("t"=seq(0,1,length=1000),"X(t)"=rnorm(1000))
#' plot_ts_est(TS,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE)
#'
plot_ts_est<-function(X,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE,N=100,Q=2,L=2){

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H) | !is.logical(LFD_Est)) {
    stop("Raw_Est_H, Smooth_Est_H and LFD_EST should have logical inputs either TRUE or FALSE")
  }

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0))) {
    stop("X must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  X<-X[order(X[,1]),]
  H_est<-Hurst(X,N,Q,L)
  colnames(H_est)<-c("x","y")
  colnames(X) <- c("t1","PP")
  t1<-X[,1]

  LFD_EST<-LFD(X,N,Q,L)
  colnames(LFD_EST)<-c("x1","y1")

  p <- ggplot(X, aes(x =.data$t1, y =.data$PP))+geom_line()+
    scale_color_manual(name = "",breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H","Raw Estimate LFD","Smoothed Estimate LFD"),
                       values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green","Raw Estimate LFD"="cyan","Smoothed Estimate LFD"="brown"))+
    labs(y="X(t)",x="t")


  if(Raw_Est_H){
    p <- p + geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H"))

  }

  if(Smooth_Est_H){
    p <- p + geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  if(LFD_Est){


    p <- p + geom_line(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Raw Estimate LFD"))

  }

  if(LFD_Smooth_Est){

    p <- p + geom_smooth(data = LFD_EST, aes(x =.data$x1, y =.data$y1,col="Smoothed Estimate LFD")
                         ,method="loess",se=FALSE,span = 0.3,linewidth=0.5)

  }

  print(p)
}


