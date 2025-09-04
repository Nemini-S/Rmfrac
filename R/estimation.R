#' Statistical estimation of the Hurst function
#'
#' @description
#' This function computes statistical estimates for the Hurst function.
#'
#' @param X Data frame where the first column is a numeric time sequence and the second the values of the process or time series.
#' @param N Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Fixed integer greater than or equal to 2. Default is set to 2.
#'
#' @return A data frame of where the first column is a time sequence and second column is estimated values of the Hurst function.
#' @note
#' Since these are estimators of local characteristics, reliable results can only be obtained when a sufficiently large number of points is used.
#' @details
#' Statistical estimation of the Hurst function is done based on the results of Ayache, A.,
#' & Bouly, F. (2023). The estimator is built through generalized quadratic variations
#' of the process associated with its increments.
#'
#'
#' @export Hurst
#'
#' @importFrom zoo rollapply
#' @importFrom matrixStats sum2
#' @importFrom stats na.omit
#'
#' @references Ayache, A. and Bouly, F. (2023). Uniformly and strongly consistent estimation for
#' the random Hurst function of a multifractional process. Latin American Journal of
#' Probability and Mathematical Statistics, 20(2):1587–1614. \doi{doi:10.30757/alea.v20-60}.
#'
#' @seealso \code{\link{LFD}}, \code{H_LFD}, \code{\link{plot.mp}}, \code{\link{plot_tsest}}, \code{\link{plot.H_LFD}}
#'
#' @examples
#' \donttest{
#' #Hurst function of a multifractional process simulated using GHBMP function
#' T <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.5 - 0.4 * sin(6 * 3.14 * t))}
#' X <- GHBMP(T, H)
#' Hurst(X)
#' }
#'
#' #Hurst function of a fractional Browian motion simulated using FBm
#' X <- FBm(H = 0.5, x_start = 0, t_start = 0, t_end = 2, N = 1000)
#' Hurst(X)
#'
Hurst <- function(X, N = 100, Q = 2, L = 2)
{
  tmin1 <- min(na.omit(X[,1]))
  tmax1 <- max(na.omit(X[,1]))

  X<-na.omit(X)

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0)) ) {
    stop("X must be a numeric data frame with time sequence given as the first column")
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

  X <- X[order(X[,1]),]

  X_scaled <- X
  X_scaled[,1] <- (X_scaled[,1] - tmin1) / (tmax1 - tmin1)

  tQ <- X_scaled[,1]
  t <- tQ[seq(1, length(tQ), by = Q)]

  l <- 0:L

  al <- (-1)^(L - l) * ((factorial(L)) / ((factorial(l)) * (factorial(L - l))))

  ms <- function(b)
  {
    sum(al * b)
  }

  log_Q <- function(x, y, q)
  {
    (log(x / y)) / log(q^2)
  }

  #val <- ifelse(L<N, "Continue", "Stop")
  #print(val)

  XNQ <- X[,2]
  XN <- XNQ[seq(1, length(XNQ), by = Q)]

  dNk <- rollapply(XN, width = (L + 1), ms)
  dNk2 <- dNk^2
  dNQk <- rollapply(XNQ, width = (L + 1), ms)
  dNQk2 <- dNQk^2

  sub_intervals <- seq(0, 1, 1/N)
  start_points <- sub_intervals[-length(sub_intervals)]
  end_points <- sub_intervals[-1]

  H_est<-function(np)
  {
    I_L <- start_points[np]
    I_U <- end_points[np]
    cx <- floor(I_U * (length(t) - 1)) - ceiling(I_L * (length(t) - 1)) + 1
    cy <- floor(I_U * ((length(tQ) - 1))) - ceiling(I_L * ((length(tQ) - 1))) + 1
    v1 <- ((I_L * (length(t) - 1)) + 1) : (I_U * (length(t) - 1) + 1)
    vx <- (1/cx) * sum2(dNk2, v1)
    v2 <- ((I_L * ((length(tQ) - 1))) + 1) :(I_U*((length(tQ) - 1)) + 1)
    vy <- (1/cy) * sum2(dNQk2, v2)
    a <- min(1, max(log_Q(vx, vy, Q), 0))
    matrix(c(I_L, a))
  }

  H_est_v <- Vectorize(H_est)
  p1 <- H_est_v(1:N)
  est_data <- as.data.frame(t(p1)) #Estimated data for the Hurst function

  est_data[,1] <- ((tmax1 - tmin1) * est_data[,1]) + tmin1

  est_data <- na.omit(est_data)
  colnames(est_data) <- c("Time", "Hurst_estimate")

  return(est_data)

}

#' Estimation of the local fractal dimension
#'
#' @description
#' This function computes the estimates for the local fractal dimension.
#'
#' @param X Data frame where the first column is a numeric time sequence and the second is the values of the process or time series.
#' @param N The same argument that is used for the estimation of Hurst function. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q The same argument that is used for the estimation of Hurst function. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L The same argument that is used for the estimation of Hurst function. Fixed integer greater than or equal to 2. Default is set to 2.
#'
#' @return A data frame where the first column is a time sequence and the second column is estimated values of the local fractal dimension.
#' @export LFD
#' @seealso \code{\link{Hurst}}, \code{\link{H_LFD}}, \code{\link{plot.mp}}, \code{\link{plot_tsest}}, \code{\link{plot.H_LFD }}
#' @note
#' Since these are estimators of local characteristics, reliable results can only be obtained when a sufficiently large number of points is used.
#' @details
#' The formula \eqn{\widehat{LFD} = 2-\widehat{H}(t)} is used to compute the estimated local fractal dimension,
#' where \eqn{\widehat{H}(t)} is the estimated Hurst function.
#'
#' @references Gneiting, T., and Schlather, M. (2004). Stochastic models
#' that separate fractal dimension and the Hurst effect. SIAM Review, 46(2):269-282.
#' \doi{doi:10.1137/S0036144501394387}.
#' @examples
#' \donttest{
#' #LFD of a multifractional process simulated using GHBMP function
#' T <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.5 - 0.4 * sin(6 * 3.14 * t))}
#' X <- GHBMP(T, H)
#' LFD(X)
#' }
#'
#' #LFD of a fractional Browian motion simulated using FBm
#' X <- FBm(H = 0.5, x_start = 0, t_start = 0, t_end = 2, N = 1000)
#' LFD(X)
#'
LFD <- function(X, N = 100, Q = 2, L = 2)
{
  X <- na.omit(X)

  if (!is.data.frame(X) | !ncol(X) == 2 | !(all(sapply(X, is.numeric))) | !(all(X[[1]] >= 0))) {
    stop("X must be a numeric data frame with time sequence from 0 to 1 given as the first column")
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

  Hurst_est <- Hurst(X, N, Q, L)

  D <- data.frame(Time = Hurst_est[,1], LFD_estimate = 2 - (Hurst_est[,2]))

  return(D)
}
