#' Sojourn measure
#'
#' @description Provides sojourn measure for X(t) greater or lower than the
#' constant level \code{A} for the provided time sequence (t) interval or a given sub interval of t.
#'
#' @param X Data frame where the first column is a time sequence (t)
#' and the second the data of the time series (X(t)).
#' @param A Constant level as a numeric value. Checks whether X(t) greater or lower than A.
#' @param n Number of points time sequence to be split into. Default set to 10000.
#' @param level A vector of character strings which specifies whether the sojourn
#' measure required for X \code{"greater"} or \code{"lower"} than A. Default set to \code{"greater"}.
#' @param subI Sub interval of the time sequence as a vector where the lower bound
#' the first element and upper bound the second. Optional; if provided area represented by
#' the sojourn measure of the sub interval is returned. Otherwise the whole time sequence is considered.
#' @param plot Logical: if \code{TRUE}, the time series, constant level and the sojourn measure is plotted.
#'
#' @return Sojourn measure. If \code{plot=TRUE}, the time series, constant level and the sojourn measure is plotted.
#' @importFrom ggplot2 geom_line geom_hline geom_ribbon labs aes
#' @importFrom stats approx
#' @importFrom rlang .data
#'
#' @export sojourn
#'
#' @examples
#' t <- seq(0,1,length=1000)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(1000))
#' sojourn(TS,0.8,level='lower',subI=c(0.5,0.8),plot=TRUE)
#'
sojourn<-function(X,A,n=10000,level='greater',subI=NULL,plot=FALSE)
{
  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))) {
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)) {
    stop("A must be numeric")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric")
  } else if (!(n %% 1 == 0) | !(n > 0)) {
    stop("n must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  X <- X[order(X[,1]),]
  colnames(X)<-c("x","y")

  if (is.null(subI)){

    time_points <- seq(X[1,1], X[nrow(X),1], length.out = n)
    diff_time_points<-((X[nrow(X),1]-X[1,1])/n)
    interpolated_X <- approx(X[,1], X[,2], xout = time_points)$y
    data_plot<-data.frame(t=time_points,x_int=interpolated_X)

    if(level=='greater' ){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X > A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X > A,interpolated_X,NA)
        data_segments <- data_plot[!is.na(data_plot$ymax), ]

        p<- ggplot(X, aes(x = .data$x, y = .data$y)) +
          geom_line() +
          geom_hline(yintercept = A,color='blue')+
          geom_point(data = data_segments, aes(x = t, y = 0), color = "red",size=0.1)+
          labs(y="X(t)",x="t")

        print(p)
      }

      return((sum(interpolated_X >= A))*diff_time_points)
    }

    else if(level=='lower'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X < A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X < A,interpolated_X,NA)
        data_segments <- data_plot[!is.na(data_plot$ymax), ]

        p<- ggplot(X, aes(x = .data$x, y = .data$y)) +
          geom_line() +
          geom_hline(yintercept = A,color='blue')+
          geom_point(data = data_segments, aes(x = t, y = 0), color = "red",size=0.1)+
          labs(y="X(t)",x="t")

        print(p)
      }

      return((sum(interpolated_X <= A))*diff_time_points)

    }

    else
    {
      print("Invalid level")
    }

  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }
    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])
    time_points <- seq(X.I[1,1], X.I[nrow(X.I),1], length.out = n)
    diff_time_points<-((X.I[nrow(X.I),1]-X.I[1,1])/n)
    interpolated_X <- approx(X.I[,1], X.I[,2], xout = time_points)$y
    data_plot<-data.frame(t=time_points,x_int=interpolated_X)

    if(level=='greater' ){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X > A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X > A,interpolated_X,NA)
        data_segments <- data_plot[!is.na(data_plot$ymax), ]

        p<- ggplot(X.I, aes(x = .data$x, y = .data$y)) +
          geom_line() +
          geom_hline(yintercept = A,color='blue')+
          geom_point(data = data_segments, aes(x = t, y = 0), color = "red",size=0.1)+
          labs(y="X(t)",x="t")

        print(p)
      }

      return((sum(interpolated_X >= A))*diff_time_points)
    }

    else if(level=='lower'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X < A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X < A,interpolated_X,NA)
        data_segments <- data_plot[!is.na(data_plot$ymax), ]

        p<- ggplot(X.I, aes(x = .data$x, y = .data$y)) +
          geom_line() +
          geom_hline(yintercept = A,color='blue')+
          geom_point(data = data_segments, aes(x = t, y = 0), color = "red",size=0.1)+
          labs(y="X(t)",x="t")

        print(p)
      }

      return((sum(interpolated_X <= A))*diff_time_points)

    }

    else
    {
      print("Invalid level")
    }


  }

}

#' Area represented by the sojourn measure
#'
#' @description
#' Provides the area represented by sojourn measure for X(t) greater or lower than the
#' constant level A for the provided time sequence (t) interval or a given sub interval of t.
#'
#' @param X Data frame where the first column is a time sequence (t)
#' and the second the data of the time series (X(t)).
#' @param A Constant level as a numeric value. Checks whether X(t) greater or lower than A.
#' @param n Number of points time sequence to be split into. Default set to 10000.
#' @param level A vector of character strings which specifies whether the sojourn
#' measure required for X \code{"greater"} or \code{"lower"} than A. Default set to \code{"greater"}.
#' @param subI Sub interval of the time sequence as a vector where the lower bound
#' the first element and upper bound the second. Optional; if provided area represented by
#' the sojourn measure of the sub interval is returned. Otherwise the whole time sequence is considered.
#' @param plot Logical: if \code{TRUE}, the time series, constant level and area represented by the sojourn measure
#' is plotted.
#'
#' @return Area represented by the sojourn measure. If \code{plot=TRUE}, the time series, constant level and Area
#'  represented by the sojourn measure is plotted.
#' @importFrom ggplot2 geom_line geom_hline geom_point labs aes
#' @importFrom stats approx
#' @importFrom rlang .data
#'
#' @export A.sojourn
#'
#' @examples
#' t <- seq(0,1,length=1000)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(1000))
#' A.sojourn(TS,0.8,level='lower',subI=c(0.5,0.8),plot=TRUE)
#'
A.sojourn<-function(X,A,n=10000,level='greater',subI=NULL,plot=FALSE)
{
  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))) {
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)) {
    stop("A must be numeric")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric")
  } else if (!(n %% 1 == 0) | !(n > 0)) {
    stop("n must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  pos<-function(x){
    if(x>=0){return(x)}
    else {return(0)}
  }

  X <- X[order(X[,1]), ]

  colnames(X)<-c("x","y")

  if (is.null(subI)){

    time_points <- seq(X[1,1], X[nrow(X),1], length.out = n)
    diff_time_points<-((X[nrow(X),1]-X[1,1])/n)
    interpolated_X <- approx(X[,1], X[,2], xout = time_points)$y
    data_plot<-data.frame(t=time_points,x_int=interpolated_X)

    if(level=='greater'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X > A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X > A,interpolated_X,NA)

        p<- ggplot(data_plot,aes(x =.data$t, y =.data$x_int)) +
          geom_line(data=X,aes(x =.data$x, y =.data$y))+
          geom_hline(yintercept = A,color='blue')+
          geom_ribbon(data=data_plot, aes(ymin = .data$ymin, ymax = .data$ymax), fill = "lightblue", alpha = 1)+
          labs(y="X(t)",x="t")

        print(p)
      }
      return((sum(sapply(interpolated_X-A,pos)))*diff_time_points)
    }

    else if(level=='lower'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X < A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X < A,interpolated_X,NA)

        p<- ggplot(data_plot,aes(x =.data$t, y =.data$x_int)) +
          geom_line(data=X,aes(x =.data$x, y =.data$y))+
          geom_hline(yintercept = A,color='blue')+
          geom_ribbon(data=data_plot, aes(ymin = .data$ymin, ymax = .data$ymax), fill = "lightblue", alpha = 1)+
          labs(y="X(t)",x="t")

        print(p)
      }
      return((sum(sapply(A-interpolated_X,pos)))*diff_time_points)
    }

    else
    {
      print("Invalid level")
    }

  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }
    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])
    time_points <- seq(X.I[1,1], X.I[nrow(X.I),1], length.out = n)
    diff_time_points<-((X.I[nrow(X.I),1]-X.I[1,1])/n)
    interpolated_X <- approx(X.I[,1], X.I[,2], xout = time_points)$y
    data_plot<-data.frame(t=time_points,x_int=interpolated_X)

    if(level=='greater'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X > A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X > A,interpolated_X,NA)

        p<- ggplot(data_plot,aes(x =.data$t, y =.data$x_int)) +
          geom_line(data=X.I,aes(x =.data$x, y =.data$y))+
          geom_hline(yintercept = A,color='blue')+
          geom_ribbon(data=data_plot, aes(ymin = .data$ymin, ymax = .data$ymax), fill = "lightblue", alpha = 1)+
          labs(y="X(t)",x="t")

        print(p)
      }
      return((sum(sapply(interpolated_X-A,pos)))*diff_time_points)
    }

    else if(level=='lower'){
      if(plot){

        data_plot$ymin<-ifelse(interpolated_X < A,A,NA)
        data_plot$ymax<-ifelse(interpolated_X < A,interpolated_X,NA)

        p<- ggplot(data_plot,aes(x =.data$t, y =.data$x_int)) +
          geom_line(data=X.I,aes(x =.data$x, y =.data$y))+
          geom_hline(yintercept = A,color='blue')+
          geom_ribbon(data=data_plot, aes(ymin = .data$ymin, ymax = .data$ymax), fill = "lightblue", alpha = 1)+
          labs(y="X(t)",x="t")

        print(p)
      }
      return((sum(sapply(A-interpolated_X,pos)))*diff_time_points)
    }

    else
    {
      print("Invalid level")
    }

  }
}

#' Maximum of a time series
#'
#' @description
#' This function finds the local maximum of a time series for the provided
#' time sequence (t) interval or a given sub interval of t ,with corresponding t value(s).
#'
#' @param X  Data frame where the first column is a time sequence (t)
#' and the second the data of the time series (X(t)).
#' @param subI Sub interval of the time sequence as a vector where the lower bound
#' the first element and upper bound the second. Optional; if provided maximum of the
#' sub interval is returned. Otherwise the whole time sequence is considered.
#' @param plot Logical: if \code{TRUE}, the time series and constant level is plotted.
#'
#' @return Print the maximum of the time series for a given sub interval and
#' the corresponding t values. If \code{plot=TRUE}, a plot of the time series with
#' with maximum and corresponding t values are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#' @export X_max
#'
#' @examples
#' #Example 1
#' t <- seq(0,1,length=100)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(100))
#' X_max(TS,subI=c(0.5,0.8),plot=TRUE)
#'
X_max<-function(X,subI=NULL,plot=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- X[order(X[,1]), ]
  colnames(X)<-c("x","y")

  if (is.null(subI)){

    X.maximum<-max(X[,2])
    t.X.maximum<-((X[,1])[which(X[,2] == X.maximum)])
    max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

    if(plot)
    {
      p <- ggplot(X, aes(x=.data$x, y=.data$y)) +
        geom_line() +
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 3) +
        geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }

    cat("Maximum is ",X.maximum,"\n")
    cat("Corresponding t ",t.X.maximum,"\n")
  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }
    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])
    X.maximum<-max(X.I[,2])
    t.X.maximum<-((X.I[,1])[which(X.I[,2] == X.maximum)])
    max_points_df <- data.frame(t = t.X.maximum, x = rep(X.maximum, length(t.X.maximum)))

    if(plot)
    {
      p <- ggplot(X.I, aes(x=.data$x, y=.data$y)) +
        geom_line() +
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 3) +
        geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }

    cat("Maximum is ",X.maximum,"\n")
    cat("Corresponding t ",t.X.maximum,"\n")

  }

}


#' Minimum of a time series
#'
#' @description
#' This function finds the local minimum of a time series for the provided
#' time sequence (t) interval or a given sub interval of t ,with corresponding t value(s).
#'
#' @param X  Data frame where the first column is a time sequence (t)
#' and the second the data of the time series (X(t)).
#' @param subI Sub interval of the time sequence as a vector where the lower bound
#' the first element and upper bound the second. Optional; if provided minimum of the
#' sub interval is returned. Otherwise the whole time sequence is considered.
#' @param plot Logical: if \code{TRUE}, the time series and constant level is plotted.
#'
#' @return Print the minimum of the time series for a given sub interval and
#' the corresponding t values. If \code{plot=TRUE}, a plot of the time series with
#' with minimum and corresponding t values are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#'
#' @export X_min
#'
#' @examples
#' #Example 1
#' t <- seq(0,1,length=100)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(100))
#' X_min(TS,subI=c(0.2,0.8),plot=TRUE)
#'
X_min<-function(X,subI=NULL,plot=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- X[order(X[,1]), ]
  colnames(X)<-c("x","y")

  if (is.null(subI)){

    X.minimum<-min(X[,2])
    t.X.minimum<-((X[,1])[which(X[,2] == X.minimum)])
    min_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

    if(plot)
    {
      p <- ggplot(X, aes(x=.data$x, y=.data$y)) +
        geom_line() +
        geom_point(data = min_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 3) +
        geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }

    cat("Maximum is ",X.minimum,"\n")
    cat("Corresponding t ",t.X.minimum,"\n")
  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }
    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])
    X.minimum<-min(X.I[,2])
    t.X.minimum<-((X.I[,1])[which(X.I[,2] == X.minimum)])
    max_points_df <- data.frame(t = t.X.minimum, x = rep(X.minimum, length(t.X.minimum)))

    if(plot)
    {
      p <- ggplot(X.I, aes(x=.data$x, y=.data$y)) +
        geom_line() +
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 3) +
        geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }

    cat("Minimum is ",X.minimum,"\n")
    cat("Corresponding t ",t.X.minimum,"\n")

  }

}


#' Level crossing times
#'
#' @description
#' Provides the corresponding t values a time series intersects a specific
#' constant level for the provided time sequence (t) interval or a provided sub interval of t.
#'
#' @param X Data frame where the first column is a time sequence (t)
#' and the second the data of the time series (X(t)).
#' @param A Constant level as a numeric value.
#' @param subI Sub interval of the time sequence as a vector where the lower bound
#' the first element and upper bound the second. Optional; if provided level crossing
#' times of the sub interval is provided. Otherwise the whole time sequence is considered.
#' @param plot Logical: if \code{TRUE}, the time series and constant level is plotted.
#'
#' @return The level crossing times. If \code{plot=TRUE}, a plot of the time series with
#' the constant level crossing and level crossing times are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#'
#' @export T.level
#'
#' @examples
#' #Example 1
#' t <- seq(0,1,length=100)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(100))
#' T.level(TS,0.1,subI=c(0.2,0.8),plot=TRUE)
#'
#'
#'
T.level<-function(X,A,subI=NULL,plot=FALSE){

    if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))) {
      stop("X must be a numeric data frame")
    }

    if (!is.numeric(A)) {
      stop("A must be numeric")
    }

    if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
    }

  X <- X[order(X[,1]), ]
  colnames(X)<-c("C1","C2")

  if (is.null(subI)){

    X.I<-X
    x<-X.I[,1]
    y<-X.I[,2]

    crossing_indices <- which(diff(sign(y - A)) != 0)

    crossing_x <- x[crossing_indices] + (A - y[crossing_indices]) /
      (y[crossing_indices + 1] - y[crossing_indices]) *
      (x[crossing_indices + 1] - x[crossing_indices])

    cross_points_df<-data.frame(t = crossing_x, x = rep(A, length(crossing_x)))

    if (plot){

      p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
        geom_line() +
        geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
        geom_point(data = cross_points_df, aes(x = .data$t, y = .data$x), color = "red", size = 2) +
        geom_vline(xintercept = crossing_x, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }
    return(crossing_x )
  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])

    x<-X.I[,1]
    y<-X.I[,2]

    crossing_indices <- which(diff(sign(y - A)) != 0)


    crossing_x <- x[crossing_indices] + (A - y[crossing_indices]) /
      (y[crossing_indices + 1] - y[crossing_indices]) *
      (x[crossing_indices + 1] - x[crossing_indices])

    cross_points_df<-data.frame(t = crossing_x, x = rep(A, length(crossing_x)))
    if (plot){

      p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
        geom_line() +
        geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
        geom_point(data = cross_points_df, aes(x = .data$t, y = .data$x), color = "red", size = 2) +
        geom_vline(xintercept = crossing_x, linetype = "dashed", color = "blue") +
        labs(x = "t",y = "X(t)")

      print(p)
    }
    return(crossing_x )
  }

}


