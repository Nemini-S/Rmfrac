#' Estimated crossing times
#'
#' @description
#' Computes the estimated \eqn{t} value(s), in which a time series crosses a specific
#' constant level for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param A Constant level as a numeric value.
#' @param subI Time sub-interval as a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided estimated crossing
#' times of the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, the constant level and corresponding \eqn{t} values are plotted
#' in interactive sessions.
#' @param vline Logical: If \code{TRUE}, a vertical line is plotted at the crossing point(s).
#'
#' @return The estimated crossing times at a given level.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_vline geom_hline labs aes
#' @importFrom rlang .data
#' @seealso \code{\link{cross_rate}}, \code{\link{cross_mean}}
#' @export cross_T
#'
#' @examples
#' t <- seq(0, 1, length = 100)
#' TS <- data.frame("t" = t, "X(t)" = rnorm(100))
#' cross_T(TS, 0.1, subI = c(0.2, 0.8), plot = TRUE, vline = TRUE)
#'
cross_T <- function(X, A, subI = NULL, plot = FALSE, vline = FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))){
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)){
    stop("A must be numeric")
  }

  if (!is.logical(plot))
  {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!is.logical(vline))
  {
    stop("vline should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]),]
  colnames(X) <- c("C1", "C2")

  if (is.null(subI)){

    X.I <- X
    t <- X.I[,1]
    x <- X.I[,2]

    cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))

    for (i in 1:(nrow(X.I) - 1)) {

      if ((x[i] < A && x[i+1] > A) || (x[i] > A && x[i+1] < A)) {
        cross_t <- t[i] + (A - x[i])*(t[i + 1 ]- t[i]) / (x[i + 1] - x[i])
        cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
      }

      if (x[i + 1] == A) {
        if (i>1 && (i + 2) <= length(x)) {
          if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
            cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
          }
        }
      }
    }

    if (isTRUE(plot) && interactive()){
      p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
        geom_line() +
        geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
        geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      if (vline){
        p <- p + geom_vline(xintercept = cross_df$cross_t, linetype = "dashed", color = "blue")
      }

      print(p)
    }
    return(cross_df$cross_t)
  }
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time <- X[,1]
    X.I <- subset(X, Time >= subI[1] & Time <= subI[2])

    t <- X.I[,1]
    x <- X.I[,2]

    cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))

    for (i in 1:(nrow(X.I) - 1)) {

      if ((x[i] < A && x[i + 1] > A) || (x[i] > A && x[i + 1] < A)) {
        cross_t <- t[i] + (A - x[i]) * (t[i + 1] - t[i]) / (x[i + 1] - x[i])
        cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
      }

      if (x[i + 1] == A) {
        if (i > 1 && (i + 2) <= length(x)) {
          if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
            cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
          }
        }
      }
    }

    if (isTRUE(plot) && interactive()){
      p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
        geom_line() +
        geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
        geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 2) +
        labs(x = "t",y = "X(t)")

      if (vline){
        p <- p + geom_vline(xintercept = cross_df$cross_t, linetype = "dashed", color = "blue")
      }

      print(p)
    }
    return(cross_df$cross_t)
  }

}


#' Crossing rate
#' @description
#' Computes the rate at which a time series crosses a specific
#' constant level for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param A Constant level as a numeric value.
#' @param subI Time sub-interval as a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided crossing rate
#' for the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, the constant level and crossing
#' points are plotted in interactive sessions.
#'
#' @return The crossing rate, which gives average number of crossings per time unit.
#' @export cross_rate
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline labs aes
#' @importFrom rlang .data
#' @seealso \code{\link{cross_T}}, \code{\link{cross_mean}}
#' @examples
#' t <- seq(0, 1, length = 100)
#' TS <- data.frame("t" = t, "X(t)" = rnorm(100))
#' cross_rate(TS, 0.1, subI = c(0.2, 0.8), plot = TRUE)
cross_rate <- function(X, A, subI = NULL, plot = FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))){
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)){
    stop("A must be numeric")
  }

  if (!is.logical(plot))
  {
  stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]), ]
  colnames(X) <- c("C1", "C2")

if (is.null(subI)){

  X.I <- X
  t <- X.I[,1]
  x <- X.I[,2]

  cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))

  for (i in 1:(nrow(X.I) - 1)) {

    if ((x[i] < A && x[i + 1] > A) || (x[i] > A && x[i + 1] < A)) {
      cross_t <- t[i] + (A - x[i]) * (t[i + 1] - t[i]) / (x[i + 1] - x[i])
      cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
    }

    if (x[i+1] == A) {
      if (i > 1 && (i + 2) <= length(x)) {
        if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
          cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
        }
      }
    }
  }

  total_t <- X.I[nrow(X.I), 1] - X.I[1, 1]

  crossing_rate <- nrow(cross_df) / total_t

  if (isTRUE(plot) && interactive()){
    p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
      geom_line() +
      geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
      geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 1.5) +
      labs(x = "t",y = "X(t)")

  print(p)
}
  return(crossing_rate)
}
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time <- X[,1]
    X.I <- subset(X, Time >= subI[1] & Time <= subI[2])

    t <- X.I[,1]
    x <- X.I[,2]

    cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))

    for (i in 1:(nrow(X.I) - 1)) {

      if ((x[i] < A && x[i + 1] > A) || (x[i] > A && x[i + 1] < A)) {
        cross_t <- t[i] + (A - x[i]) * (t[i + 1] - t[i]) / (x[i + 1] - x[i])
        cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
      }

      if (x[i+1] == A) {
        if (i > 1 && (i + 2) <= length(x)) {
          if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
            cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
          }
        }
      }
    }

    total_t <- X.I[nrow(X.I), 1] - X.I[1, 1]

    crossing_rate <- nrow(cross_df) / total_t

    if (isTRUE(plot) && interactive()){
      p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
        geom_line() +
        geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
        geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 1.5) +
        labs(x = "t",y = "X(t)")

      print(p)
    }
    return(crossing_rate)
  }

}


#' Mean time between crossings
#' @description
#' Computes the mean duration between crossings of a time series
#' at a specified constant level for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param A Constant level as a numeric value.
#' @param subI Time sub-interval is a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided mean crossing
#' times for the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series, the constant level and crossing
#' points are plotted in interactive sessions.
#'
#' @return The estimated mean time between crossings.
#' @export cross_mean
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline labs aes
#' @importFrom rlang .data
#' @seealso \code{\link{cross_T}}, \code{\link{cross_rate}}
#' @examples
#' t <- seq(0, 1, length = 100)
#' TS <- data.frame("t" = t, "X(t)" = rnorm(100))
#' cross_mean(TS, 0.1, subI = c(0.2, 0.8), plot = TRUE)
cross_mean <- function(X, A, subI = NULL, plot = FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))){
    stop("X must be a numeric data frame")
  }

  if (!is.numeric(A)){
    stop("A must be numeric")
  }

  if (!is.logical(plot))
  {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]), ]
  colnames(X) <- c("C1", "C2")

  if (is.null(subI)){

    X.I <- X
    t <- X.I[,1]
    x <- X.I[,2]

    cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))
    for (i in 1:(nrow(X.I) - 1)) {

      if ((x[i] < A && x[i + 1] > A) || (x[i] > A && x[i + 1] < A)) {
        cross_t <- t[i] + (A - x[i]) * (t[i+1] - t[i]) / (x[i+1] - x[i])
        cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
      }

      if (x[i + 1] == A && i > 1 && i + 1 <= nrow(X.I)) {
        if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
          cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
        }
      }
    }

    if (nrow(cross_df) < 2) {

      message("Insufficient number of crossings to compute the mean time between crossings.\n")

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
          geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 2) +
          labs(x = "t",y = "X(t)")

        print(p)
      }

    } else {

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
          geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 2) +
          labs(x = "t",y = "X(t)")

        print(p)
      }

      time_diff <- diff(cross_df$cross_t)
      mean_t <- mean(time_diff)
      return(mean_t)
    }

  }
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1,1] & subI[2] <= X[nrow(X),1]))){
      stop("subI must be a numeric vector")
    }

    Time <- X[,1]
    X.I <- subset(X, Time >= subI[1] & Time <= subI[2])

    t <- X.I[,1]
    x <- X.I[,2]

    cross_df <- data.frame(cross_t = numeric(0), cross_x = numeric(0))
    for (i in 1:(nrow(X.I) - 1)) {

      if ((x[i] < A && x[i + 1] > A) || (x[i] > A && x[i + 1] < A)) {
        cross_t <- t[i] + (A - x[i]) * (t[i + 1] - t[i]) / (x[i + 1] - x[i])
        cross_df <- rbind(cross_df, data.frame(cross_t = cross_t, cross_x = A))
      }

      if (x[i+1] == A && i > 1 && i + 1 <= nrow(X.I)) {
        if ((x[i] < A && x[i + 2] > A) || (x[i] > A && x[i + 2] < A)) {
          cross_df <- rbind(cross_df, data.frame(cross_t = t[i + 1], cross_x = A))
        }
      }
    }

    if (nrow(cross_df) < 2) {

      message("Insufficient number of crossings to compute the mean time between crossings.\n")

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
          geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 2) +
          labs(x = "t",y = "X(t)")

        print(p)
      }

      } else {

        if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_hline(yintercept = A, linetype = "dashed", color = "blue") +
          geom_point(data = cross_df, aes(x = .data$cross_t, y = .data$cross_x), color = "red", size = 2) +
          labs(x = "t",y = "X(t)")

        print(p)
      }

      time_diff <- diff(cross_df$cross_t)
      mean_t <- mean(time_diff)
      return(mean_t)

    }

  }

}



#' Longest increasing/decreasing streak
#' @description
#' Computes the time span of the longest increasing or decreasing streak(s)
#' of a time series for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param direction A character string which specifies the direction of
#' the streak: \code{"increasing"} or \code{"decreasing"}.
#' @param subI Time sub-interval is a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided
#' longest streak(s) time span of the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series and the longest streaks(s) of
#' increasing/decreasing is plotted in interactive sessions.
#'
#' @return A data frame with one row for each longest streak, containing the time span
#' and the corresponding values of \eqn{X(t)} at the streak endpoints.
#'
#' @export long_streak
#' @importFrom ggplot2 ggplot geom_line labs aes
#' @importFrom rlang .data
#' @importFrom stats aggregate
#' @importFrom utils head tail
#' @seealso \code{\link{mean_streak}}
#' @examples
#' t <- seq(0, 1, length = 100)
#' TS <- data.frame("t" = t,"X(t)" = rnorm(100))
#' long_streak(TS, direction = 'decreasing', subI = c(0.2, 0.8), plot = TRUE)
long_streak <- function(X, direction = 'increasing', subI = NULL, plot = FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))){
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot))
  {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!direction %in% c("increasing", "decreasing")) {
    stop("streak should be 'increasing' or 'decreasing' ")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]), ]
  colnames(X) <- c("C1", "C2")

  if (is.null(subI)){

    X.I <- X
    t <- X.I[,1]
    x <- X.I[,2]

    if(direction == 'increasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (direction == 'decreasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (length(streak) > 0) {

      time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

      max_int <- max(time_int)

      longest_streak <- streak[time_int == max_int]

      long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

      for (i in 1:length(longest_streak)) {
        id <- longest_streak[[i]][1]:longest_streak[[i]][2]
        df1 <- data.frame(t = t[id], x = x[id], group = i)
        long_streak_df <- rbind(long_streak_df, df1)
      }

      t_start <- aggregate(t ~ group, long_streak_df, head, 1)
      t_end <- aggregate(t ~ group, long_streak_df, tail, 1)
      X_start <- aggregate(x ~ group, long_streak_df, head, 1)
      X_end <- aggregate(x ~ group, long_streak_df, tail, 1)

      Return_DF <- data.frame(streak_no = t_start$group, t_start = t_start$t, t_end = t_end$t, X_start = X_start$x, X_end = X_end$x)
      row.names(Return_DF) <- NULL

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
             geom_line() +
             geom_line(data = long_streak_df, aes(x = .data$t, y = .data$x, group = .data$group), color = "blue",linewidth = 1) +
             labs(x = "t",y = "X(t)")

        print(p)
      }

      return(Return_DF)

    }

    else{

      if (isTRUE(plot) && interactive()){

          p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
               geom_line() +
               labs(x = "t",y = "X(t)")

        print(p)
      }

      message(sprintf("No %s streaks", direction))

    }


  }
  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1, 1] & subI[2] <= X[nrow(X), 1]))){
      stop("subI must be a numeric vector")
    }

    Time <- X[,1]
    X.I <- subset(X, Time >= subI[1] & Time <= subI[2])

    t <- X.I[,1]
    x <- X.I[,2]

    if(direction == 'increasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (direction == 'decreasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (length(streak) > 0) {

      time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

      max_int <- max(time_int)

      longest_streak <- streak[time_int == max_int]

      long_streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

      for (i in 1:length(longest_streak)) {
        id <- longest_streak[[i]][1]:longest_streak[[i]][2]
        df1 <- data.frame(t = t[id], x = x[id], group = i)
        long_streak_df <- rbind(long_streak_df, df1)
      }

      t_start <- aggregate(t ~ group, long_streak_df, head, 1)
      t_end <- aggregate(t ~ group, long_streak_df, tail, 1)
      X_start <- aggregate(x ~ group, long_streak_df, head, 1)
      X_end <- aggregate(x ~ group, long_streak_df, tail, 1)

      Return_DF <- data.frame(streak_no = t_start$group, t_start = t_start$t, t_end = t_end$t, X_start = X_start$x, X_end = X_end$x)
      row.names(Return_DF) <- NULL

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_line(data = long_streak_df, aes(x = .data$t, y = .data$x, group = .data$group), color = "blue",linewidth = 1) +
          labs(x = "t", y = "X(t)")

        print(p)
      }

      return(Return_DF)

    }

    else{

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          labs(x = "t", y = "X(t)")

        print(p)
      }

      message(sprintf("No %s streaks", direction))

    }

  }

}


#' Mean time span of increasing/decreasing streaks
#' @description
#' Computes the mean time span of the increasing/decreasing streaks
#' for the provided time interval or its sub-interval.
#'
#' @param X Data frame where the first column is a numeric time sequence \eqn{t}
#' and the second one is the values of the time series \eqn{X(t)}.
#' @param direction A character string which specifies the direction of
#' the streak: \code{"increasing"} or \code{"decreasing"}.
#' @param subI Time sub-interval is a vector, where the lower bound is
#' the first element and the upper bound is the second. Optional: If provided mean time span
#' of the sub-interval is returned, otherwise the whole time interval is considered.
#' @param plot Logical: If \code{TRUE}, the time series and the increasing/decreasing
#' streaks are plotted in interactive sessions.
#'
#' @return Mean time span of the increasing/decreasing streaks.
#' @export mean_streak
#' @importFrom ggplot2 ggplot geom_line labs aes
#' @importFrom rlang .data
#' @seealso \code{\link{long_streak}}
#' @examples
#' t <- seq(0, 1 ,length = 100)
#' TS <- data.frame("t" = t,"X(t)" = rnorm(100))
#' mean_streak(TS, direction = 'decreasing', subI = c(0.2,0.8), plot = TRUE)
#'
mean_streak <- function(X, direction = 'increasing', subI = NULL, plot = FALSE){

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(sapply(X[,1], is.numeric)))){
    stop("X must be a numeric data frame")
  }

  if (!is.logical(plot))
  {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!direction %in% c("increasing", "decreasing")) {
    stop("streak should be 'increasing' or 'decreasing' ")
  }

  X <- na.omit(X)
  X <- X[order(X[,1]), ]
  colnames(X) <- c("C1", "C2")

  if (is.null(subI)){

    X.I <- X
    t <- X.I[,1]
    x <- X.I[,2]

    if(direction == 'increasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (direction == 'decreasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (length(streak) > 0) {

      time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

      mean_streak <- mean(time_int)

      streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

      for (i in 1:length(streak)) {
        id <- streak[[i]][1]:streak[[i]][2]
        df1 <- data.frame(t = t[id], x = x[id], group = i)
        streak_df <- rbind(streak_df, df1)
      }

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_line(data = streak_df, aes(x = .data$t, y = .data$x, group = .data$group), color = "blue",linewidth = 1)+
          labs(x = "t", y = "X(t)")

        print(p)
      }

      return(mean_streak)

    }
    else{
      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          labs(x = "t", y = "X(t)")

        print(p)
      }

      message(sprintf("No %s streaks", direction))
    }

  }

  else{

    if (!is.numeric(subI) | !is.vector(subI) | !length(subI) == 2 | !(all(subI[1] >= X[1, 1] & subI[2] <= X[nrow(X), 1]))){
      stop("subI must be a numeric vector")
    }

    Time <- X[,1]
    X.I <- subset(X, Time >= subI[1] & Time <= subI[2])

    t <- X.I[,1]
    x <- X.I[,2]

    if(direction == 'increasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] > x[i-1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] > x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (direction == 'decreasing'){

      streak <- list()
      start <- 1

      for (i in 2:length(x)) {
        if (x[i] < x[i - 1]) {
          next
        } else {
          if (i - 1 > start) {
            streak[[length(streak) + 1]] <- c(start, i - 1)
          }
          start <- i
        }
      }

      if (length(x) > 1 && x[length(x)] < x[length(x) - 1]) {
        streak[[length(streak) + 1]] <- c(start, length(x))
      }

    }

    if (length(streak) > 0) {

      time_int <- sapply(streak, function(n) t[n[2]] - t[n[1]])

      mean_streak <- mean(time_int)

      streak_df <- data.frame(t = numeric(0), x = numeric(0), group = integer(0))

      for (i in 1:length(streak)) {
        id <- streak[[i]][1]:streak[[i]][2]
        df1 <- data.frame(t = t[id], x = x[id], group = i)
        streak_df <- rbind(streak_df, df1)
      }

      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          geom_line(data = streak_df, aes(x = .data$t, y = .data$x, group = .data$group), color = "blue",linewidth = 1)+
          labs(x = "t", y = "X(t)")

        print(p)
      }

      return(mean_streak)

    }
    else{
      if (isTRUE(plot) && interactive()){
        p <- ggplot(X.I, aes(x = .data$C1, y = .data$C2)) +
          geom_line() +
          labs(x = "t", y = "X(t)")

        print(p)
      }

      message(sprintf("No %s streaks", direction))
    }


  }
}


#' Relative strength index
#' @description
#' This function computes the Relative Strength Index (RSI) for a time series.
#'
#' @param X A vector.
#' @param period Period length used for smoothing. Default is set to 14.
#' @param plot Logical: If \code{TRUE}, the time series and the
#' RSI are plotted (with \code{overbought} and
#' \code{oversold} levels) in the same window in interactive sessions.
#' @param overbought Horizontal line which indicates an overbought level in the RSI plot. Default is set to 70.
#' @param oversold Horizontal line which indicates an oversold level in the RSI plot. Default is set to 30.
#'
#' @return A list or vector of the RSI values.
#' @details To compute the RSI,
#'
#' \deqn{\text{RSI} = 100 \dfrac{\text{Average\_gain}}{\text{Average\_gain}+\text{Average\_loss}}}
#'
#' formula is used. Average gain and average loss are computed using the Wilders's smoothing method.
#' @references Wilder, J. W. (1978). New concepts in technical trading systems. Greensboro, NC.
#' @importFrom ggplot2 ggplot geom_line geom_hline labs aes facet_grid vars
#' @importFrom rlang .data
#' @importFrom zoo index
#'
#' @export RS_Index
#'
#' @examples
#' X <- c(74.44,74.19,74.25,73.65,74.37,74.73,75.15,75.46,75.88,76.78,
#'             75.81,76.53,75.11,76.28,76.68,76.08,76.53,76.11,76.42,75.58,
#'             75.44,75.46,74.98)
#' RS_Index(X, plot = TRUE)
#'
RS_Index <- function(X, period = 14, plot = FALSE, overbought = 70, oversold = 30)
{

  if(!is.vector(X)){
    stop("X must be a vector")
  } else if (!(length(X) >= (period + 1))){
    stop("Not enough data to compute RSI for the given period")
  }

  if (!is.numeric(period)) {
    stop("period must be numeric")
  } else if (!(period %% 1 == 0) | !(period > 0)) {
    stop("period must be a positive integer")
  }

  if (!is.logical(plot))
  {
    stop("plot should have logical inputs either TRUE or FALSE")
  }

  if (!is.numeric(overbought) | !is.numeric(oversold)) {
    stop("overbought and oversold must be numeric")
  } else if ((overbought < 0 | overbought > 100) | (oversold < 0 | oversold > 100)) {
    stop("overbought and oversold should range from 0 to 100")
  }

  X <- na.omit(X)
  t <- 1:length(X)

  N <- length(X)
  diff <- diff(X)

  gain <- ifelse(diff > 0, diff, 0)
  loss <- ifelse(diff < 0, -diff, 0)

  gain_mean <- rep(NA, length(X))
  loss_mean <- rep(NA, length(X))

  gain_mean[period + 1] <- mean(gain[1:period], na.rm = TRUE)
  loss_mean[period + 1] <- mean(loss[1:period], na.rm = TRUE)

  for (i in (period + 2):N) {
    gain_mean[i] <- (gain_mean[i - 1] * (period - 1) + gain[i - 1]) / period
    loss_mean[i] <- (loss_mean[i - 1] * (period - 1) + loss[i - 1]) / period
  }

  RSI <- 100 * (gain_mean) / (gain_mean + loss_mean)

  RSI_df <- data.frame(t = rep(t, 2), value = c(X, RSI),group = rep(c("X", "RSI"), each = N))

  if (isTRUE(plot) && interactive())
  {
    df1 <- data.frame(group = "RSI",hline = c(overbought, oversold))

    p <- ggplot(RSI_df, aes(x = .data$t, y = .data$value)) +
      geom_line(na.rm = TRUE) +
      geom_hline(data = df1, aes(yintercept = .data$hline),color = c("blue","blue"), linetype = "dashed") +
      facet_wrap(~group, ncol = 1, scales = "free_y") +
      labs(title = "X and Relative Strength Index", y = "X(t)", x = "t")

    print(p)

  }

  return(RSI)

}

