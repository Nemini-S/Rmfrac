#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @export
autoplot.mp<-function(X.t,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,N=100,Q=2,L=2)
{

  if (!is.data.frame(X.t) | !ncol(X.t) == 2 | !(all(sapply(X.t, is.numeric))) | !(all(X.t[[1]] >= 0 & X.t[[1]] <= 1))) {
    stop("X.t must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H)) {
    stop("Raw_Est_H and Smooth_Est_H should have logical inputs either TRUE or FALSE")
  }

  X.t<-X.t[order(X.t[,1]),]
  H_est<-Hurst(X.t,N,Q,L)
  colnames(H_est)<-c("x","y")
  colnames(X.t) <- c("t1", "PP")
  t1<-X.t[,1]


  if (!is.null(H)) {

    H1<-sapply(t1,H)
    data1<-data.frame(t1,H1) #Data for the theoretical Hurst function
    if (!is.numeric(H1) | !all(H1 >= 0 & H1<= 1)) {
      stop("H must be a function which returns a numeric list between 0 and 1")
    }

    if(Raw_Est_H && Smooth_Est_H){

      p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
        geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
        labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


    } else if (Raw_Est_H && !(Smooth_Est_H)){

      p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red"))+
        labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


    } else if (!(Raw_Est_H) && (Smooth_Est_H)) {

      p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Smoothed Estimate H"="green"))+
        labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


    } else if (!(Raw_Est_H) && !(Smooth_Est_H)) {

      p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H"),
                           values=c("Theoretical H"="blue"))+
        labs(main="GHBMP and Hurst functions",y="X(t)",x="t")

    }

  } else { if (Raw_Est_H && Smooth_Est_H) {
    p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
      geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(name = "Hurst function", breaks=c( "Raw Estimate H", "Smoothed Estimate H"),
                         values=c( "Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
      labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


  } else if(!(Raw_Est_H) && Smooth_Est_H){
    p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(name = "Hurst function", breaks=c( "Smoothed Estimate H"),
                         values=c(  "Smoothed Estimate H"="green"))+
      labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


  } else if(Raw_Est_H && !(Smooth_Est_H)){
    p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_line(data = H_est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
      scale_color_manual(name = "Hurst function", breaks=c( "Raw Estimate H"),
                         values=c( "Raw Estimate H"="red"))+
      labs(main="GHBMP and Hurst functions",y="X(t)",x="t")


  } else if(!(Raw_Est_H) && !(Smooth_Est_H)){
    p<-ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      labs(main="GHBMP",y="X(t)",x="t")

  }
  }

}


#' Plot Gaussian Haar-based multifractional processes with theoretical and estimated Hurst functions
#'
#' @description
#' Creates a plot of the Gaussian Haar-based multifractional process
#' simulated using \code{\link{GHBMP}} with theoretical Hurst function (if provided),
#' Hurst function estimated using \code{\link{Hurst}} and
#' the smoothed estimated Hurst function.
#'
#'
#' @param x Return from \code{\link{GHBMP}}. For accurate plot of the process, should be of at least 500 data points.
#' @param H Theoretical Hurst function. Optional: If provided, the theoretical Hurst function is plotted.
#' @param Raw_Est_H Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param N Argument used for the estimation of Hurst functions. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param ... Unused arguments.
#'
#' @return A ggplot object which plots the multifractional process with theoretical, estimated and smoothed Hurst functions.
#'
#' @exportS3Method Rmultifractional::plot
#'
#' @importFrom graphics plot
#'
#' @seealso \code{\link{GHBMP}}, \code{\link{Hurst}}
#'
#' @examples
#' #Simulation of the multifractional process and estimation of the Hurst function
#' T <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X <- GHBMP(T,H)
#'
#' #Plot of multifractional process, theoretical, estimated and smoothed Hurst functions
#' plot(X,H=H)
#'
#' #Plot of multifractional process, estimated and smoothed Hurst functions
#' plot(X)
#'
#'
plot.mp <- function(x,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,N=100,Q=2,L=2,...) {
  print(autoplot(x,H,Raw_Est_H,Smooth_Est_H,N,Q,L))
}


#' @importFrom ggplot2 autoplot ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#' @export
autoplot.est<-function(H_est,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE)
{

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H)) {
    stop("Raw_Est_H and Smooth_Est_H should have logical inputs either TRUE or FALSE")
  }

  colnames(H_est)<-c("x","y")
  t1<-H_est[,1]

  if (!is.null(H)) {

    H1<-sapply(t1,H)
    data1<-data.frame(t1,H1) #Data for the theoretical Hurst function
    if (!is.numeric(H1) | !all(H1 >= 0 & H1<= 1)) {
      stop("H must be a function which returns a numeric list between 0 and 1")
    }

    if(Raw_Est_H && Smooth_Est_H){

      p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
        geom_line(aes(col="Raw Estimate H")) +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
        labs(main="Hurst functions",y="H(t)",x="t")


    } else if (Raw_Est_H && !(Smooth_Est_H)){
      p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
        geom_line(aes(col="Raw Estimate H")) +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Raw Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red"))+
        labs(main="Hurst functions",y="H(t)",x="t")


    } else if (!(Raw_Est_H) && (Smooth_Est_H)) {

      p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(name = "Hurst function", breaks=c("Theoretical H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Smoothed Estimate H"="green"))+
        labs(main="Hurst functions",y="H(t)",x="t")


    } else if (!(Raw_Est_H) && !(Smooth_Est_H)) {
      p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
                scale_color_manual(name = "Hurst function", breaks=c("Theoretical H"),
                           values=c("Theoretical H"="blue"))+
        labs(main="Hurst functions",y="H(t)",x="t")

    }

  } else { if (Raw_Est_H && Smooth_Est_H) {
    p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
      geom_line(aes(col="Raw Estimate H")) +
      geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(name = "Hurst function", breaks=c( "Raw Estimate H", "Smoothed Estimate H"),
                         values=c("Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
      labs(main="Hurst functions",y="H(t)",x="t")


  } else if(!(Raw_Est_H) && Smooth_Est_H){

    p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
      geom_smooth(data = H_est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(name = "Hurst function", breaks=c("Smoothed Estimate H"),
                         values=c("Smoothed Estimate H"="green"))+
      labs(main="Hurst functions",y="H(t)",x="t")


  } else if(Raw_Est_H && !(Smooth_Est_H)){

    p<-ggplot(H_est, aes(x =.data$x, y =.data$y))+
      geom_line(aes(col="Raw Estimate H")) +
      scale_color_manual(name = "Hurst function", breaks=c("Raw Estimate H"),
                         values=c("Raw Estimate H"="red"))+
      labs(main="Hurst functions",y="H(t)",x="t")


  } else if(!(Raw_Est_H) && !(Smooth_Est_H)){

    p<-"Provide at least one option out of H, Raw_Est_H and Smooth_Est_H"
  }
  }

}




#' Plot theoretical and estimated Hurst functions
#'
#' @description
#' Creates a plot of the estimated Hurst function with the theoretical
#' Hurst function and the smoothed estimated Hurst function using the return from \code{\link{Hurst}}.
#'
#' @param x Return from \code{\link{Hurst}}.
#' @param H Theoretical Hurst function. Optional: If provided, the theoretical Hurst function is plotted.
#' @param Raw_Est_H Logical: If \code{TRUE}, the Hurst function estimated by using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical: If \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method.
#' @param ... Unused arguments.
#'
#' @return A ggplot object which plots the theoretical, estimated and smoothed Hurst functions.
#'
#' @exportS3Method Rmultifractional::plot
#'
#' @importFrom graphics plot
#'
#' @seealso \code{\link{Hurst}}
#'
#' @examples
#' #Simulation of the multifractional process and estimation of the Hurst function
#' T <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X <- GHBMP(T,H)
#' Est_H <- Hurst(X)
#'
#' #Plot of theoretical, estimated and smoothed Hurst functions
#' plot(Est_H,H=H)
#'
#' #Plot of estimated and smoothed Hurst functions
#' plot(Est_H)
#'
#'
plot.est <- function(x,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,...) {
  print(autoplot(x,H,Raw_Est_H,Smooth_Est_H))
}
