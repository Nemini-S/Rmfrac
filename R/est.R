#' Statistical estimation of the Hurst function
#'
#' @description
#' This function provides statistical estimates for the Hurst function of multifractional processes.
#'
#' @param X.t Data frame where the first column is a time sequence from 0 to 1 and the second the data of the multifractional process.
#' For reliable estimates the data frame should be of at least XXX data points.
#' @param Q Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Arbitrary fixed integer greater than or equal to 2. Default is set to 2.
#' @param N Number of sub intervals the estimation is performed on. Default is set to 100 sub intervals.
#'
#' @return A data frame where the first column is a time sequence and second column is estimated data for the Hurst function.
#' Each row gives estimated values for each time point using \code{N} sub intervals.
#'
#' @details
#' Statistical estimation of the Hurst function is done based on the results of Ayache, A.,
#' & Bouly, F. (2023). The estimator is built through generalized quadratic variations
#' of the process associated with its generalized increments.
#'
#' @export Hurst
#'
#' @importFrom zoo rollapply
#' @importFrom matrixStats sum2
#' @importFrom stats na.omit
#'
#' @references Ayache, A. and Bouly, F. (2023). Uniformly and strongly consistent estimation for
#' the random Hurst function of a multifractional process. Latin American Journal of
#' Probability and Mathematical Statistics, 20(2):1587–1614.
#'
#' @examples
#' #Example 1: For a multifractional process simulated using GHBMP function
#' T <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X <- GHBMP(T,H)
#' Hurst(X)
#'
#'
#' \dontrun{
#' #Example 2: For a Fractional Brownian motion simulated using the fbm function of somebm package.
#' library(somebm)
#' T <- seq(0,1,length=10000)
#' FBM<-data.frame("t"=T,"p"=fbm(hurst=0.7,n=9999))
#' Hurst(FBM)
#' }
#'
Hurst<-function(X.t,N=100,Q=2,L=2)
{
  if (!is.data.frame(X.t) | !ncol(X.t) == 2 | !(all(sapply(X.t, is.numeric))) | !(all(X.t[[1]] >= 0 & X.t[[1]] <= 1))) {
    stop("X.t must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.numeric(Q)) {
    stop("Q must be numeric")
  } else if (!(Q %% 1 == 0) | !(Q > 1)) {
    stop("Q must be a positive integer greater than 1")
  }

  if (!is.numeric(L)) {
    stop("L must be numeric")
  } else if (!(L %% 1 == 0) | !(L > 1)) {
    stop("L must be a positive integer greater than 1")
  }

  X.t<-X.t[order(X.t[,1]),]
  tQ<-X.t[,2]
  t<-tQ[seq(1, length(tQ), by = Q)]

  l<-0:L

  al<-(-1)^(L-l)*((factorial(L))/((factorial(l))*(factorial(L-l))))

  ms <- function(b)
  {
    sum(al*b)
  }

  log_Q<-function(x,y,q)
  {
    (log(x/y))/log(q^2)
  }

  #val <- ifelse(L<N, "Continue", "Stop")
  #print(val)

  XNQ<-X.t[,2]
  XN <- XNQ[seq(1, length(XNQ), by = Q)]

  dNk<-rollapply(XN,width=(L+1),ms)
  dNk2<-dNk^2
  dNQk<-rollapply(XNQ,width=(L+1),ms)
  dNQk2<-dNQk^2

  sub_intervals <- seq(0, 1, 1/N)
  start_points <- sub_intervals[-length(sub_intervals)]
  end_points <- sub_intervals[-1]

  H_est<-function(np)
  {
    I_L<-start_points[np]
    I_U<-end_points[np]
    cx<-floor(I_U*(length(t)-1))-ceiling(I_L*(length(t)-1))+1
    cy<-floor(I_U*((length(tQ)-1)))-ceiling(I_L*((length(tQ)-1)))+1
    v1<-((I_L*(length(t)-1))+1) : (I_U*(length(t)-1)+1)
    vx<-(1/cx)*sum2(dNk2,v1)
    v2<-((I_L*((length(tQ)-1)))+1) :(I_U*((length(tQ)-1))+1)
    vy<-(1/cy)*sum2(dNQk2,v2)
    a<-min(1,max(log_Q(vx,vy,Q),0))
    matrix(c(I_L,a))
  }

  H_est_v<-Vectorize(H_est)
  p1<-H_est_v(1:N)
  est_data<-na.omit(as.data.frame(t(p1))) #Estimated data for the Hurst function
  colnames(est_data)<-c("Time","Hurst_estimate")

  class(est_data)<-c("est",class(est_data))

  return(est_data)

}

#' Plot real and estimated Hurst function with multifractional process
#'
#' @description
#' Creates a plot of the multifractional process with theoretical Hurst function, Hurst function estimated
#' using \code{\link{Hurst}} and the smoothed estimated Hurst function.
#'
#'
#' @param H.est Data frame of Hurst function estimated from \code{\link{Hurst}}.
#' @param X.t Data frame where the first column is a time sequence from 0 to 1 and the second the data of the multifractional process.
#' For accurate plot of the process, it should be of at least 200 data points.
#' @param H Theoretical Hurst function. Optional; if provided, the theoretical Hurst function is plotted.
#' @param Raw_Est_H Logical; if \code{TRUE}, the Hurst function estimated using \code{\link{Hurst}} is plotted.
#' @param Smooth_Est_H Logical; if \code{TRUE}, the smoothed estimated Hurst function is plotted.
#' The estimated Hurst function is smoothed using the loess method and is plotted.
#' @param ... Other arguments
#'
#' @return Plot of the multifractional process with theoretical, estimated and smoothed Hurst functions.
#'
#' @exportS3Method GHBMP1::plot
#'
#' @importFrom ggplot2 ggplot geom_line geom_smooth scale_color_manual labs aes
#' @importFrom rlang .data
#'
#' @examples
#' #Simulation of the multifractional process and estimation of the Hurst function
#' T <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X <- GHBMP(T,H)
#' Est_H<-Hurst(X)
#'
#' #Plot of multifractional process, theoretical, estimated and smoothed Hurst functions
#' plot(Est_H,X,H=H)
#'
#' #Plot of multifractional process, estimated and smoothed Hurst functions
#' plot(Est_H,X)
plot.est<-function(H.est,X.t,H=NULL,Raw_Est_H=TRUE,Smooth_Est_H=TRUE,...)
{

  if (!is.data.frame(X.t) | !ncol(X.t) == 2 | !(all(sapply(X.t, is.numeric))) | !(all(X.t[[1]] >= 0 & X.t[[1]] <= 1))) {
    stop("X.t must be a numeric data frame with time sequence from 0 to 1 given as the first column")
  }

  if (!is.logical(Raw_Est_H) | !is.logical(Smooth_Est_H)) {
    stop("Raw_Est_H and Smooth_Est_H should have logical inputs either TRUE or FALSE")
  }

  colnames(H.est)<-c("x","y")
  X.t<-X.t[order(X.t[,1]),]
  colnames(X.t) <- c("t1", "PP")
  t1<-X.t[,1]


  if (!is.null(H)) {

    H1<-sapply(t1,H)
    data1<-data.frame(t1,H1) #Data for the real Hurst function
    if (!is.numeric(H1) | !all(H1 >= 0 & H1<= 1)) {
      stop("H must be a function which returns a numeric list between 0 and 1")
    }

    if(Raw_Est_H && Smooth_Est_H){

      ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_line(data = H.est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
        geom_smooth(data = H.est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(breaks=c("Theoretical H", "Raw Estimate H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
        labs(y="X(t)",x="t")

    } else if (Raw_Est_H && !(Smooth_Est_H)){

      ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_line(data = H.est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
        scale_color_manual(breaks=c("Theoretical H", "Raw Estimate H"),
                           values=c("Theoretical H"="blue", "Raw Estimate H"="red"))+
        labs(y="X(t)",x="t")

    } else if (!(Raw_Est_H) && (Smooth_Est_H)) {

      ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Theoretical H"))+
        geom_smooth(data = H.est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                    ,method="loess",se=FALSE,span = 0.3) +
        scale_color_manual(breaks=c("Theoretical H", "Smoothed Estimate H"),
                           values=c("Theoretical H"="blue", "Smoothed Estimate H"="green"))+
        labs(y="X(t)",x="t")

    } else if (!(Raw_Est_H) && !(Smooth_Est_H)) {

      ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
        geom_line(data =data1, aes(x =.data$t1, y =.data$H1,col="Real H"))+
        scale_color_manual(breaks=c("Theoretical H"),
                           values=c("Theoretical H"="blue"))+
        labs(y="X(t)",x="t")
    }

  } else { if (Raw_Est_H && Smooth_Est_H) {
    ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_line(data = H.est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
      geom_smooth(data = H.est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(breaks=c( "Raw Estimate H", "Smoothed Estimate H"),
                         values=c( "Raw Estimate H"="red", "Smoothed Estimate H"="green"))+
      labs(y="X(t)",x="t")

  } else if(!(Raw_Est_H) && Smooth_Est_H){
    ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_smooth(data = H.est, aes(x =.data$x, y =.data$y,col="Smoothed Estimate H")
                  ,method="loess",se=FALSE,span = 0.3) +
      scale_color_manual(breaks=c( "Smoothed Estimate H"),
                         values=c(  "Smoothed Estimate H"="green"))+
      labs(y="X(t)",x="t")

  } else if(Raw_Est_H && !(Smooth_Est_H)){
    ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      geom_line(data = H.est, aes(x =.data$x, y =.data$y,col="Raw Estimate H")) +
      scale_color_manual(breaks=c( "Raw Estimate H"),
                         values=c( "Raw Estimate H"="red"))+
      labs(y="X(t)",x="t")

  } else if(!(Raw_Est_H) && !(Smooth_Est_H)){
    ggplot(X.t, aes(x =.data$t1, y =.data$PP))+geom_line() +
      labs(y="X(t)",x="t")
  }
}
}





