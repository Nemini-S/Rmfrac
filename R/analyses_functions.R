#' Estimated sojourn measure
#'
#' @description Computes the estimated sojourn measure for a time series \eqn{X(t)} greater or lower than the
#' constant level \code{A} for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param A Constant level as a numeric value.
#' @param N Number of steps the time interval (or time sub-interval) is split into. Default set to 10000.
#' @param level A vector of character strings which specifies which sojourn
#' measure required for \code{X}, \code{"greater"} or \code{"lower"} than \code{A}. Default set to \code{"greater"}.
#' @param subI Time sub-interval is a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided,
#' the estimated sojourn measure for the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, constant level and the sojourn measure are plotted.
#'
#' @return Estimated sojourn measure. If \code{plot=TRUE}, the time series, the constant level (in blue) and the excursion region (in red) are plotted.
#' @export sojourn
#' @importFrom ggplot2 ggplot geom_line geom_hline geom_segment labs aes ggtitle theme element_text
#' @importFrom stats approx aggregate
#' @importFrom rlang .data
#' @seealso \code{\link{exc_Area}}
#' @examples
#' t <- seq(0,1,length=1000)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(1000))
#' sojourn(TS,0.8,level='lower',subI=c(0.5,0.8),plot=TRUE)
sojourn<-function(X,A,N=10000,level='greater',subI=NULL,plot=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)){
    stop("A must be numeric")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!level %in% c("greater", "lower")) {
    stop("level should be 'greater' or 'lower'")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]),]
  colnames(X) <- c("x","y")

  if (is.null(subI)){

    ag_df <- aggregate(X[,2]~X[,1],FUN=mean)
    t <- seq(ag_df[1,1],ag_df[nrow(ag_df),1],length.out=N+1)
    int_X <- approx(x=ag_df[,1],y=ag_df[,2],xout=t)$y
    diff<-((ag_df[nrow(ag_df),1]-ag_df[1,1])/N)

    if(level=='greater'){

      S <- 0
      seg <- data.frame(T_start=rep(NA_real_,N), T_end=rep(NA_real_,N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1>=A) && (x2>=A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1>=A) && (x2<A)){

          S = S + (diff * (x1-A)/(x1-x2))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1-A)/(x1-x2))

        } else if ((x1<A) && (x2>=A)){

          S = S + (diff * (A-x2)/(x1-x2))

          seg$T_start[i] <- t2 - (diff * (A-x2)/(x1-x2))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }
    }

    if(level=='lower'){

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1<=A) && (x2<=A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1<=A) && (x2>A)){

          S = S + (diff * (A-x1)/(x2-x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A-x1)/(x2-x1))

        } else if ((x1>A) && (x2<=A)){

          S = S + (diff * (x2-A)/(x2-x1))

          seg$T_start[i] <- t2 - (diff * (x2-A)/(x2-x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

    }

    seg <- na.omit(seg)
    if (plot){

      p<- ggplot(X, aes(x = .data$x, y = .data$y)) +
        geom_line() +
        geom_hline(yintercept = A,color="blue",linetype = "dashed")+
        labs(y="X(t)",x="t")+
        ggtitle(sprintf("Excursion region where the realisation is over the level %s", A))+
        theme(plot.title = element_text(size = 10))

      if (nrow(seg)>0){

        p <- p + geom_segment(data = seg, aes(x = .data$T_start, xend = .data$T_end, y = 0, yend = 0) ,color="red")
      }

      print(p)
    }

    return(S)

  }
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])

    ag_df <- aggregate(X.I[,2]~X.I[,1],FUN=mean)
    t <- seq(ag_df[1,1], ag_df[nrow(X.I),1], length.out = N+1)
    int_X <- approx(x=ag_df[,1],y=ag_df[,2],xout=t)$y
    diff<-((ag_df[nrow(ag_df),1]-ag_df[1,1])/N)

    if(level=='greater'){

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1>=A) && (x2>=A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1>=A) && (x2<A)){

          S = S + (diff * (x1-A)/(x1-x2))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (x1-A)/(x1-x2))

        } else if ((x1<A) && (x2>=A)){

          S = S + (diff * (A-x2)/(x1-x2))

          seg$T_start[i] <- t2 - (diff * (A-x2)/(x1-x2))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }}

    if(level=='lower'){

      S <- 0
      seg <- data.frame(T_start = rep(NA_real_, N), T_end = rep(NA_real_, N))

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1<=A) && (x2<=A)){

          S = S + diff

          seg$T_start[i] <- t1
          seg$T_end[i] <- t2

        } else if ((x1<=A) && (x2>A)){

          S = S + (diff * (A-x1)/(x2-x1))

          seg$T_start[i] <- t1
          seg$T_end[i] <- t1 + (diff * (A-x1)/(x2-x1))

        } else if ((x1>A) && (x2<=A)){

          S = S + (diff * (x2-A)/(x2-x1))

          seg$T_start[i] <- t2 - (diff * (x2-A)/(x2-x1))
          seg$T_end[i] <- t2

        } else {

          S = S

          seg$T_start[i] <- NA
          seg$T_end[i] <- NA
        }

      }

    }

    seg <- na.omit(seg)
    if (plot){

      p<- ggplot(X.I, aes(x = .data$x, y = .data$y)) +
        geom_line() +
        geom_hline(yintercept=A,color="blue",linetype = "dashed")+
        labs(y="X(t)",x="t")+
        ggtitle(sprintf("Excursion region where the realisation is over the level %s", A))+
        theme(plot.title = element_text(size = 10))

      if (nrow(seg)>0){

        p <- p + geom_segment(data = seg, aes(x = .data$T_start, xend = .data$T_end, y = 0, yend = 0) ,color="red")
      }

      print(p)
    }

    return(S)

  }

}


#' Excursion area
#'
#' @description
#' Computes the excursion area where a time series \eqn{X(t)} is greater or lower than the
#' constant level \code{A} for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param A Constant level as a numeric value.
#' @param N Number of steps the time interval (or time sub-interval) is split into. Default set to 10000.
#' @param level A vector of character strings which specifies whether the excursion
#' area is required for \code{X}, \code{"greater"} or \code{"lower"} than \code{A}. Default set to \code{"greater"}.
#' @param subI Time sub-interval is a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided, the excursion area
#' for the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, constant level and excursion area are plotted.
#'
#' @return Excursion area. If \code{plot=TRUE}, the time series, the constant level and excursion area
#' are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_hline geom_polygon labs aes ggtitle theme element_text
#' @importFrom stats approx aggregate
#' @importFrom rlang .data
#'
#' @export exc_Area
#'
#' @seealso \code{\link{sojourn}}
#'
#' @examples
#' t <- seq(0,1,length=1000)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(1000))
#' exc_Area(TS,0.8,level='lower',subI=c(0.5,0.8),plot=TRUE)
#'
exc_Area <- function(X,A,N=10000,level='greater',subI=NULL,plot=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)){
    stop("A must be numeric")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!level %in% c("greater", "lower")) {
    stop("level should be 'greater' or 'lower'")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]),]
  colnames(X) <- c("x","y")

  if (is.null(subI)){

    ag_df <- aggregate(X[,2]~X[,1],FUN=mean)
    t <- seq(ag_df[1,1],ag_df[nrow(ag_df),1],length.out=N+1)
    int_X <- approx(x=ag_df[,1],y=ag_df[,2],xout=t)$y
    diff<-((ag_df[nrow(ag_df),1]-ag_df[1,1])/N)


    if(level=='greater'){

      Area <- 0
      DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1>=A) && (x2>=A)){

          Area = Area + (diff * (((x1-A)+(x2-A))/2))

          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1))
          if (i==N) {
            DF_Area <- rbind(DF_Area,data.frame(t=t2,X_t=x2))}

        } else if ((x1>=A) && (x2<A)){

          Area = Area + ((diff * (x1-A)/(x1-x2)) * ((x1-A)/2))

          x_cross <- t1+(diff * (x1-A)/(x1-x2))
          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1),
                           data.frame(t=x_cross,X_t=A),
                           data.frame(t=x_cross,X_t=A))

          DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A,nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))

        } else if ((x1<A) && (x2>=A)){

          Area = Area + ((diff * (A-x2)/(x1-x2)) * ((x2-A)/2))

          x_cross <- t1+(diff*(A-x2)/(x1-x2))
          DF_Area <- rbind(DF_Area,data.frame(t=x_cross,X_t=A))

        } else {

          Area = Area

        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area,data.frame(t=ag_df[nrow(ag_df),1],X_t=ag_df[nrow(ag_df),2]))
        DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
    }

    if(level=='lower'){

      Area <- 0
      DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1<=A) && (x2<=A)){

          Area = Area + (diff * (((A-x1)+(A-x2))/2))

          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1))
          if (i==N) {
            DF_Area <- rbind(DF_Area,data.frame(t=t2,X_t=x2))}

        } else if ((x1<=A) && (x2>A)){

          Area = Area + ((diff * (A-x1)/(x2-x1)) * ((A-x1)/2))

          x_cross <- t1+(diff * (A-x1)/(x2-x1))
          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1),
                           data.frame(t=x_cross,X_t=A),
                           data.frame(t=x_cross,X_t=A))

          DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A,nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))

        } else if ((x1>A) && (x2<=A)){

          Area = Area + ((diff * (x2-A)/(x2-x1)) * ((A-x2)/2))

          x_cross <- t1+(diff * (x2-A)/(x2-x1))
          DF_Area <- rbind(DF_Area,data.frame(t=x_cross,X_t=A))

        } else {

          Area = Area
        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area,data.frame(t=ag_df[nrow(ag_df),1],X_t=ag_df[nrow(ag_df),2]))
        DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

    }

    DF_Area <- na.omit(DF_Area)

    if (plot){
      p<- ggplot(X, aes(x = .data$x, y = .data$y)) +
        geom_line() +
        geom_hline(yintercept = A,color="blue",linetype = "dashed")+
        labs(y="X(t)",x="t")+
        ggtitle(sprintf("Excursion area of the realisation over the level %s", A))+
        theme(plot.title = element_text(size = 10))


      if (!is.null(DF_Area) && nrow(DF_Area)>0){

        p <- p + geom_polygon(data=DF_Area,aes(x=.data$t,y=.data$X_t,group=.data$G),fill="lightblue")
      }

      print(p)
    }

    return(Area)

  }
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time<-X[,1]
    X.I<-subset(X, Time >= subI[1] & Time <= subI[2])

    ag_df <- aggregate(X.I[,2]~X.I[,1],FUN=mean)
    t <- seq(ag_df[1,1], ag_df[nrow(X.I),1], length.out = N+1)
    int_X <- approx(x=ag_df[,1],y=ag_df[,2],xout=t)$y
    diff<-((ag_df[nrow(ag_df),1]-ag_df[1,1])/N)

    if(level=='greater'){
      Area <- 0
      DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1>=A) && (x2>=A)){

          Area = Area + (diff * (((x1-A)+(x2-A))/2))

          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1))
          if (i==N) {
            DF_Area <- rbind(DF_Area,data.frame(t=t2,X_t=x2))}

        } else if ((x1>=A) && (x2<A)){

          Area = Area + ((diff * (x1-A)/(x1-x2)) * ((x1-A)/2))

          x_cross <- t1+(diff * (x1-A)/(x1-x2))
          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1),
                           data.frame(t=x_cross,X_t=A),
                           data.frame(t=x_cross,X_t=A))

          DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A,nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))

        } else if ((x1<A) && (x2>=A)){

          Area = Area + ((diff * (A-x2)/(x1-x2)) * ((x2-A)/2))

          x_cross <- t1+(diff*(A-x2)/(x1-x2))
          DF_Area <- rbind(DF_Area,data.frame(t=x_cross,X_t=A))

        } else {

          Area = Area

        }

      }

      if (!is.null(DF_Area) && (nrow(DF_Area) > 0)) {
        DF_Area <- rbind(DF_Area,data.frame(t=ag_df[nrow(ag_df),1],X_t=ag_df[nrow(ag_df),2]))
        DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)
    }

    if(level=='lower'){

      Area <- 0
      DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))
      G <- 1
      polygon <- list()

      for(i in 1:(N)){

        x1 <- int_X[i]
        x2 <- int_X[i+1]

        t1 <- t[i]
        t2 <- t[i+1]

        if((x1<=A) && (x2<=A)){

          Area = Area + (diff * (((A-x1)+(A-x2))/2))

          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1))
          if (i==N) {
            DF_Area <- rbind(DF_Area,data.frame(t=t2,X_t=x2))}

        } else if ((x1<=A) && (x2>A)){

          Area = Area + ((diff * (A-x1)/(x2-x1)) * ((A-x1)/2))

          x_cross <- t1+(diff * (A-x1)/(x2-x1))
          DF_Area <- rbind(DF_Area,data.frame(t=t1,X_t=x1),
                           data.frame(t=x_cross,X_t=A),
                           data.frame(t=x_cross,X_t=A))

          DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A,nrow(DF_Area))))
          DF_Area$G <- G
          polygon[[G]] <- DF_Area
          G <- G + 1
          DF_Area <- data.frame(t=numeric(0),X_t=numeric(0))

        } else if ((x1>A) && (x2<=A)){

          Area = Area + ((diff * (x2-A)/(x2-x1)) * ((A-x2)/2))

          x_cross <- t1+(diff * (x2-A)/(x2-x1))
          DF_Area <- rbind(DF_Area,data.frame(t=x_cross,X_t=A))

        } else {

          Area = Area
        }

      }

      if (!is.null(DF_Area) && nrow(DF_Area) > 0) {
        DF_Area <- rbind(DF_Area,data.frame(t=ag_df[nrow(ag_df),1],X_t=ag_df[nrow(ag_df),2]))
        DF_Area <- rbind(DF_Area,data.frame(t=rev(DF_Area$t),X_t=rep(A, nrow(DF_Area))))
        DF_Area$G <- G
        polygon[[G]] <- DF_Area
      }

      DF_Area <- do.call(rbind, polygon)

    }

    DF_Area <- na.omit(DF_Area)

    if (plot){

      p<- ggplot(X.I, aes(x = .data$x, y = .data$y)) +
        geom_line() +
        geom_hline(yintercept = A,color="blue",linetype = "dashed")+
        labs(y="X(t)",x="t")+
        ggtitle(sprintf("Excursion area of the realisation over the level %s", A))+
        theme(plot.title = element_text(size = 10))


      if (!is.null(DF_Area) && nrow(DF_Area)>0){

        p <- p + geom_polygon(data=DF_Area,aes(x=.data$t,y=.data$X_t,group=.data$G),fill="lightblue")
      }

      print(p)

    }

    return(Area)

  }
}


#' Estimated maximum of a time series
#'
#' @description
#' This function computes the maximum of a time series for the provided
#' time interval or its sub-interval.
#'
#' @param X  Data frame where the first column is a numeric time sequence \eqn{(t)}
#' and the second the values of the time series \eqn{(X(t))}.
#' @param subI Time sub-interval is a vector where the lower bound is
#' the first element and upper bound is the second. Optional: If provided maximum of the
#' sub-interval is returned, otherwise the whole time sequence is considered.
#' @param plot Logical: If \code{TRUE}, the time series, the maximum and corresponding \eqn{t} values are plotted.
#' @param hline Logical: If \code{TRUE}, a horizontal line is plotted across the maximum.
#' @param vline Logical: If \code{TRUE}, a vertical line is plotted across the maximum.
#'
#' @return Print the maximum of the time series and
#' the corresponding \eqn{t} values. If \code{plot=TRUE}, the time series with
#' with maximum and corresponding \eqn{t} values are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#' @export X_max
#'
#' @seealso \code{\link{X_min}}
#'
#' @examples
#' t <- seq(0,1,length=100)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(100))
#' X_max(TS,subI=c(0.5,0.8),plot=TRUE)
#'
X_max<-function(X,subI=NULL,plot=FALSE,vline=FALSE,hline=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!is.logical(vline))
  {
    stop("vline should have logical inputs either TRUE or FALSE")
  }

  if (!is.logical(hline))
  {
    stop("hline should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
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
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      if(vline){
        p <- p + geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue")
      }

      if(hline){
        p <- p + geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue")
      }

      if (hline && vline){
        p <- p+
          geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue") +
          geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue")
      }

      print(p)
    }

    max_return <- c(t.X.maximum,X.maximum)
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
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      if(vline){
        p <- p + geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue")
      }

      if(hline){
        p <- p + geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue")
      }

      if (hline && vline){
        p <- p+
          geom_hline(yintercept = X.maximum, linetype = "dashed", color = "blue") +
          geom_vline(xintercept = t.X.maximum, linetype = "dashed", color = "blue")
      }

      print(p)
    }

    max_return <- c(t.X.maximum,X.maximum)

  }

  return(max_return)
}


#' Estimated minimum of a time series
#'
#' @description
#' This function computes the minimum of a time series for the provided
#' time interval or its sub-interval.
#'
#' @param X  Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second the values of the time series \eqn{X(t)}.
#' @param subI Time sub-interval is a vector where the lower bound is
#' the first element and upper bound is the second. Optional: If provided minimum of the
#' sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, the minimum and corresponding \eqn{t} values are plotted.
#' @param hline Logical: If \code{TRUE}, a horizontal line is plotted across the minimum.
#' @param vline Logical: If \code{TRUE}, a vertical line is plotted across the minimum.
#'
#' @return Print the minimum of the time series and
#' the corresponding \eqn{t} values. If \code{plot=TRUE}, the time series with
#' with minimum and corresponding \eqn{t} values are plotted.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#'
#' @export X_min
#'
#' @seealso \code{\link{X_max}}
#'
#' @examples
#' t <- seq(0,1,length=100)
#' TS <- data.frame("t"=t,"X(t)"=rnorm(100))
#' X_min(TS,subI=c(0.2,0.8),plot=TRUE)
#'
X_min<-function(X,subI=NULL,plot=FALSE,vline= FALSE,hline=FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric))))
  {
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot)) {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!is.logical(vline))
  {
    stop("vline should have logical inputs either TRUE or FALSE")
  }

  if (!is.logical(hline))
  {
    stop("hline should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
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
        geom_point(data = min_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      if(vline){
       p <- p + geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue")
      }

      if(hline){
        p <- p + geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue")
      }

      if (hline && vline){
        p <- p+
          geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue") +
          geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue")
      }

      print(p)
    }

    min_return <- c(t.X.minimum,X.minimum)
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
        geom_point(data = max_points_df, aes(x=.data$t, y=.data$x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      if(vline){
        p <- p + geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue")
      }

      if(hline){
        p <- p + geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue")
      }

      if (hline && vline){
        p <- p+
          geom_hline(yintercept = X.minimum, linetype = "dashed", color = "blue") +
          geom_vline(xintercept = t.X.minimum, linetype = "dashed", color = "blue")
      }

      print(p)
    }

    min_return <- c(t.X.minimum,X.minimum)

  }

  return(min_return)
}
