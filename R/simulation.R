#' Simulation of Gaussian Haar-based multifractional processes
#'
#' @description
#' This function simulates a realisation of a Gaussian Haar-based multifractional process at any
#' time point or time sequence on the interval \eqn{[0,1]}.
#'
#' @param t Time point or time sequence on the interval \eqn{[0,1]}.
#' @param H Hurst function which depends on \code{t} \eqn{(H(t))}. See Examples for usage.
#' @param J Positive integer. \code{J} is recommended to be greater than \eqn{\log_2(length(t))}. For large \code{J} values could
#' be rather time consuming. Default is set to 15.
#' @param num.cores Number of cores to set up the clusters for parallel computing.
#'
#' @return A data frame of class \code{"mp"} where the first column is time moments \code{t} and second column is simulated values of \eqn{X(t)}.
#'
#' @details
#' The following formula defined in Ayache, A., Olenko, A. & Samarakoon, N. (2025) was used in simulating Gaussian Haar-based multifractional process.
#'
#' \eqn{X(t) := \sum_{j=0}^{+\infty}  \sum_{k=0}^{2^{j}-1}\left(\int_{0}^{1} (t-s)_{+}^{H_{j}(k/{2^j})-{1}/{2}} h_{j,k}(s)ds \right)\varepsilon_{j,k},}
#'
#' where
#'
#' \eqn{  \int_{0}^{1} (t-s)_{+}^{H_{j,k}-\frac{1}{2}} h_{j,k} (s) ds = 2^{-j H_{j,k}} h^{[H_{j,k}]} (2^jt-k)}
#'
#' with \eqn{h^{[\lambda]} (x) =  \int_{\mathbb{R}} (x-s)_{+}^{\lambda-\frac{1}{2}} h(s) ds}.
#' \eqn{h} is the Haar mother wavelet, \eqn{j} and \eqn{k} are positive integers, \eqn{t} is time, \eqn{H} is the Hurst function and
#' \eqn{\varepsilon_{j,k}} is a sequence of independent \eqn{\mathcal{N}(0,1)} Gaussian random variables.
#' For simulations, the truncated version of this formula with first summation up to J is used.
#'
#' @note
#' See Examples for the usage of constant, time-varying, piecewise or step Hurst functions.
#'
#' @export GHBMP
#'
#' @importFrom parallel clusterExport parLapply stopCluster
#' @importFrom parallelly availableCores makeClusterPSOCK
#' @importFrom stats rnorm
#'
#' @references Ayache, A., Olenko, A. and Samarakoon, N. (2025).
#' On Construction, Properties and Simulation of Haar-Based
#' Multifractional Processes. \doi{doi:10.48550/arXiv.2503.07286}. (submitted).
#'
#' @seealso \code{\link{Hurst}}, \code{\link{plot.mp}}, \code{\link{Bm}}, \code{\link{FBm}},
#' \code{\link{FGn}}, \code{\link{Bbridge }}, \code{\link{FBbridge }}
#' @examples
#' \dontrun{
#' #Constant Hurst function
#' t <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.4 + 0*t)}
#' GHBMP(t, H)
#'
#' #Linear Hurst function
#' t <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.2 + 0.45*t)}
#' GHBMP(t, H)
#'
#' #Oscillating Hurst function
#' t <- seq(0, 1, by = (1/2)^10)
#' H <- function(t) {return(0.5 - 0.4 * sin(6 * 3.14 * t))}
#' GHBMP(t, H)
#'
#' #Piecewise Hurst function
#' t <- seq(0, 1, by = (1/2)^10)
#' H <- function(x) {
#' ifelse(x >= 0 & x <= 0.8, 0.375 * x + 0.2,
#'       ifelse(x > 0.8 & x <= 1,-1.5 * x + 1.7, NA))
#' }
#' GHBMP(t, H)
#' }
GHBMP<-function(t, H, J = 15, num.cores = availableCores(omit = 1))
{
  if (!is.numeric(t)|!all(t >= 0 & t<= 1)) {
    stop("t must be a numeric sequence between 0 and 1")
  }

  H.t<-sapply(t, H)
  if (!is.numeric(H.t) | !all(H.t > 0 & H.t< 1)) {
    stop("H must be a function which returns a numeric list between 0 and 1")
  }

  if (!is.numeric(J)) {
    stop("J must be numeric")
   } else if (!(J > 0) | !(J %% 1 == 0)){
      stop("J must be a positive integer")
    }

  if (!is.numeric(num.cores)) {
    stop("num.cores must be numeric")
   } else if (!(num.cores %% 1 == 0) | !(num.cores > 0)) {
      stop("num.cores must be a positive integer")
   }

  options(warn = -1)
  cl <- makeClusterPSOCK(num.cores) #Creation of a cluster using PSOCK connections for parallel computing

  t <- sort(t)

  x1 <- 0:J
  x2 <- 0:(2^(J) - 1)

  Aind <- outer(x1, x2, function(x,y) as.integer(as.logical(y < 2^x)))
  Aindv <- unlist(asplit(Aind, 2))
  ind0 <- which(Aindv > 0)

  A1 <- outer(x1, x2, function(x, y) 2^x)
  A2 <- outer(x1, x2, function(x, y) y)

  m <- rnorm((J + 1) * 2^(J))
  dim(m) <- c(J + 1, 2^(J))

  H1 <- outer(x1, x2, function(x, y) H(y / 2^x))

  A3 <- outer(x1, x2, function(x, y) 2^(-x * H(y / 2^x)))

  m <- ((H1 + 0.5)^(-1)) * m * A3

  A1v <- unlist(asplit(A1, 2))[ind0]
  A2v <- unlist(asplit(A2, 2))[ind0]
  mv <- unlist(asplit(m, 2))[ind0]
  Hv <- unlist(asplit(H1, 2))[ind0]

  Xt<-function(t)
  {
    tv <- A1v * t - A2v
    ind1 <- which(tv > 0)
    ind2 <- which(tv > 0.5)
    ind3 <- which(tv > 1)
    return(sum(mv[ind1] * (tv[ind1]^(Hv[ind1] + 0.5))) - 2 * sum(mv[ind2] * ((tv[ind2] - 0.5)^(Hv[ind2] + 0.5))) + sum(mv[ind3] * ((tv[ind3] - 1)^(Hv[ind3] + 0.5))))
  }

  clusterExport(cl, c("t", "A1v", "A2v", "Hv", "mv"),envir = environment())
  XN <- do.call(c, parLapply(cl, t, Xt))

  sim_data <- data.frame("t" = t, "X" = XN)
  class(sim_data) <- c("mp", class(sim_data))
  return(sim_data)

  stopCluster(cl)
  options(warn = 0)
}




#' Simulation of Brownian motion
#'
#' @description
#' This function simulates a realisation of the Brownian motion over the
#' time interval \code{[t_start,t_end]} with \code{N} time steps and initial value \code{x_start}.
#'
#' @param x_start Value of the process at the initial time point (additive constant mean).
#' @param t_start Initial time point.
#' @param t_end Terminal time point.
#' @param N Number of sub-intervals the interval \code{[t_start,t_end]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the Brownian
#' motion is plotted.
#'
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of Brownian motion with added constant mean.
#'
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @importFrom stats rnorm
#'
#' @export Bm
#' @seealso \code{\link{GHBMP}}, \code{\link{FBm}}, \code{\link{FGn}}, \code{\link{Bbridge}}, \code{\link{FBbridge}}
#'
#' @examples
#' Bm(t_end = 2, plot = TRUE)
Bm<-function(x_start = 0, t_start = 0, t_end = 1, N = 1000, plot = FALSE)
{

  if (!is.numeric(x_start)) {
    stop("x_start must be numeric")
  }

  if (!is.numeric(t_start)) {
    stop("t_start must be numeric")
  } else if ( !(t_start >=0)) {
    stop("Incorrect input for t_start")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  }

  if (!(t_start < t_end)) {
    stop("Incorrect inputs for t_start and t_end")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  diff <- (t_end - t_start) / N
  increments <- rnorm(N, mean = 0, sd = sqrt(diff))
  X <- c(x_start, x_start + cumsum(increments))
  t <- seq(t_start, t_end, length.out = N + 1)
  sim_data <- data.frame(t = t, X = X)

  if(plot){
    p<- ggplot(sim_data, aes(x = .data$t, y = .data$X)) +
      geom_line() +
      labs(y="X(t)",x="t") +
      ggtitle("Realisation of Brownian motion")

    print(p)
  }

  return(sim_data)
}


#' Simulation of fractional Brownian motion
#'
#' @description
#' This function simulates a realisation of the fractional Brownian motion over
#' the time interval \code{[t_start,t_end]} for a provided Hurst parameter.
#'
#' @param H Hurst parameter which lies between 0 and 1.
#' @param x_start Value of the process at the initial time point (additive constant mean).
#' @param t_start Initial time point.
#' @param t_end Terminal time point.
#' @param N Number of sub-intervals the interval \code{[t_start,t_end]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the fractional Brownian
#' motion is plotted.
#'
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of fractional Brownian motion
#' with added constant mean.
#'
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @importFrom stats toeplitz rnorm
#'
#' @export FBm
#' @references Banna, O., Mishura, Y., Ralchenko, K., & Shklyar, S. (2019). Fractional Brownian motion:
#' Approximations and Projections. John Wiley & Sons. \doi{doi:10.1002/9781119476771.app3}.
#' @seealso \code{\link{FGn}}, \code{\link{Bm}}, \code{\link{GHBMP}}, \code{\link{Bbridge}}, \code{\link{FBbridge}}
#'
#' @examples
#' FBm(H = 0.5, plot = TRUE)
FBm <- function(H, x_start = 0, t_start = 0, t_end = 1, N = 1000, plot = FALSE){

  if (!is.numeric(H) | !(H > 0 & H< 1)) {
    stop("H must be a number between 0 and 1")
  }

  if (!is.numeric(x_start)) {
    stop("x_start must be numeric")
  }

  if (!is.numeric(t_start)) {
    stop("t_start must be numeric")
  } else if ( !(t_start >=0)) {
    stop("Incorrect input for t_start")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  } else if ( !(t_end > 0)) {
    stop("Incorrect input for t_end")
  }

  if (!(t_start < t_end)) {
    stop("Incorrect inputs for t_start and t_end")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  diff <- (t_end - t_start) / N
  t <- seq(t_start, t_end, length.out = N + 1)

  c<- function(k, H){
    0.5 * ((k + 1)^(2*H) + (abs(k - 1))^(2*H) - 2*(k)^(2*H))
  }

  cov_mat <- toeplitz(sapply(0:(N-1), c, H = H))

  L <- t(chol(cov_mat))

  z <- rnorm(N, 0, 1)

  x <- L%*%z

  B <- cumsum(x)
  B <- B * diff^H
  B <- x_start + B
  B <- append(B, x_start, after = 0)

  sim_data <- data.frame(t = t, X = B)

  if(plot){
    p<- ggplot(sim_data, aes(x = .data$t, y = .data$X)) +
      geom_line() +
      labs(y = "X(t)",x = "t") +
      ggtitle("Realisation of fractional Brownian motion")

    print(p)
  }

  return(sim_data)

}

#' Simulation of fractional Gaussian noise
#'
#' @description
#' This function simulates a realisation of the fractional Gaussian noise over the
#' time interval \code{[t_start,t_end]} for a provided Hurst parameter.
#'
#' @param H Hurst parameter which lies between 0 and 1.
#' @param t_start Initial time point.
#' @param t_end Terminal time point.
#' @param n Number of time points where the simulation is performed on the interval \code{[t_start,t_end]}.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the fractional Gaussian noise
#' is plotted.
#'
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of fractional Gaussian noise.
#'
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @importFrom stats toeplitz rnorm
#'
#' @export FGn
#' @references Banna, O., Mishura, Y., Ralchenko, K., & Shklyar, S. (2019). Fractional Brownian motion:
#' Approximations and Projections. John Wiley & Sons. \doi{doi:10.1002/9781119476771.app3}.
#' @seealso \code{\link{FBm}}, \code{\link{Bm}}, \code{\link{GHBMP}}, \code{\link{Bbridge }}, \code{\link{FBbridge }}
#' @examples
#' FGn(H=0.5,plot=TRUE)
FGn <- function(H, t_start = 0, t_end = 1, n = 1000, plot = FALSE){

  if (!is.numeric(H) | !(H > 0 & H< 1)) {
    stop("H must be a number between 0 and 1")
  }

  if (!is.numeric(t_start)) {
    stop("t_start must be numeric")
  } else if ( !(t_start >= 0)) {
    stop("Incorrect input for t_start")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  } else if ( !(t_end > 0)) {
    stop("Incorrect input for t_end")
  }

  if (!(t_start<t_end)) {
    stop("Incorrect inputs for t_start and t_end")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric")
  } else if (!(n %% 1 == 0) | !(n > 0)) {
    stop("n must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  t <- seq(t_start, t_end, length.out = n)

  c<- function(k, H){
    0.5 * ((k + 1)^(2*H) + (abs(k - 1))^(2*H) - 2*(k)^(2*H))
  }

  cov_mat <- toeplitz(sapply(0:(n - 1), c, H = H))

  L <- t(chol(cov_mat))

  z <- rnorm(n, 0, 1)

  x <- L%*%z

  sim_data <- data.frame(t = t, X = x)

  if(plot){
    p<- ggplot(sim_data,aes(x = .data$t,y = .data$X)) +
      geom_line() +
      labs(y = "X(t)",x = "t") +
      ggtitle("Realisation of fractional Guassian noise")

    print(p)
  }

  return(sim_data)

}

#' Simulation of Brownian bridge
#'
#' @description
#' This function simulates a realisation of the Brownian bridge over the
#' time interval \code{[0,t_end]} which terminates at \code{x_end} with \code{N} time steps.
#'
#' @param x_end Value of the process at the terminating time point.
#' @param t_end Terminal time point.
#' @param N Number of sub-intervals the interval \code{[0,t_end]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the Brownian bridge
#' is plotted.
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of Brownian bridge.
#' @seealso \code{\link{FBm}}, \code{\link{Bm}}, \code{\link{GHBMP}}, \code{\link{FBbridge }}
#' @export Bbridge
#' @references Bianchi, S., Frezza, M., Pianese, A., Palazzo, A.M. (2022). Modelling
#' H-Volatility with Fractional Brownian Bridge. In: Corazza, M., Perna, C., Pizzi, C.,
#' Sibillo, M. (eds) Mathematical and Statistical Methods for Actuarial Sciences and Finance.
#' MAF 2022. Springer, Cham. \doi{doi:10.1007/978-3-030-99638-3_16}.
#' @examples
#' Bbridge(x_end=2,t_end=1,plot=TRUE)
Bbridge <- function(x_end, t_end, N = 1000, plot = FALSE){

  if (!is.numeric(x_end)) {
    stop("x_end must be numeric")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  } else if ( !(t_end > 0)) {
    stop("Incorrect input for t_end")
  }

  if (!(0 < t_end)) {
    stop("Incorrect inputs for t_start and t_end")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  Bm_sim <- Bm(x_start = 0, t_start = 0, t_end = t_end, N = N)
  X <- Bm_sim[,2] - ((Bm_sim[,1] / t_end) * (Bm_sim[N + 1, 2] - x_end))
  sim_data <- data.frame(t = Bm_sim[,1], X = X)

  if(plot){
    p<- ggplot(sim_data, aes(x = .data$t, y = .data$X)) +
      geom_line() +
      labs(y = "X(t)",x = "t") +
      ggtitle("Realisation of Brownian bridge")

    print(p)
  }

  return(sim_data)
}




#' Simulation of fractional Brownian bridge
#'
#' @description
#' This function simulates a realisation of the fractional Brownian bridge
#' for a provided Hurst parameter over the time interval \code{[0,t_end]}
#' which  terminates at \code{x_end} with \code{N} time steps.
#'
#' @param H Hurst parameter which lies between 0 and 1.
#' @param x_end Value of the process at the terminating time point.
#' @param t_end Terminal time point.
#' @param N Number of sub-intervals the interval \code{[0,t_end]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the fractional Brownian bridge
#' is plotted.
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of fractional Brownian bridge.
#' @seealso \code{\link{FBm}}, \code{\link{Bm}}, \code{\link{GHBMP}}, \code{\link{Bbridge}}
#' @export FBbridge
#' @references Bianchi, S., Frezza, M., Pianese, A., Palazzo, A.M. (2022). Modelling
#' H-Volatility with Fractional Brownian Bridge. In: Corazza, M., Perna, C., Pizzi, C.,
#' Sibillo, M. (eds) Mathematical and Statistical Methods for Actuarial Sciences and Finance.
#' MAF 2022. Springer, Cham. \doi{doi:10.1007/978-3-030-99638-3_16}.
#' @examples
#' FBbridge(H = 0.5, x_end = 2, t_end = 1,plot = TRUE)
FBbridge <- function(H, x_end, t_end, N = 1000, plot = FALSE){

  if (!is.numeric(H) | !(H > 0 & H< 1)) {
    stop("H must be a number between 0 and 1")
  }

  if (!is.numeric(x_end)) {
    stop("x_end must be numeric")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  } else if ( !(t_end > 0)) {
    stop("Incorrect input for t_end")
  }

  if (!(0 < t_end)) {
    stop("Incorrect inputs for t_start and t_end")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  FBm_sim <- FBm(H = H, x_start = 0, t_start = 0, t_end = t_end, N = N)
  X <- FBm_sim[,2] - (0.5 * (FBm_sim[N + 1, 2] - x_end) * (1 + (FBm_sim[,1] / t_end)^(2*H) - (1 - (FBm_sim[,1] / t_end))^(2*H)))
  sim_data <- data.frame(t = FBm_sim[,1], X = X)

  if(plot){
    p<- ggplot(sim_data, aes(x = .data$t, y = .data$X)) +
      geom_line() +
      labs(y = "X(t)", x = "t") +
      ggtitle("Realisation of fractional Brownian bridge")

    print(p)
  }

  return(sim_data)
}


