#' Simulation of Gaussian Haar-based multifractional processes
#'
#' @description
#' This function simulates a Gaussian Haar-based multifractional process at any
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
#' For simulations, the truncated version of this formula with first summation up to J is considered.
#'
#' @note
#' See Examples for the usage of Hurst functions, for example, constant, time-varying, piecewise or step functions.
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
#'
#' @seealso \code{\link{Hurst}}, \code{\link{plot.mp}}, \code{\link{Bm}}, \code{\link{FBM}}, \code{\link{sim_FGN}}
#' @examples
#' #Constant Hurst function
#' t <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.4 +0*t)}
#' GHBMP(t,H)
#'
#' #Linear Hurst function
#' t <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.2+0.45*t)}
#' GHBMP(t,H)
#'
#' #Oscillating Hurst function
#' t <- seq(0,1,by=(1/2)^10)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' GHBMP(t,H)
#'
#' #Piecewise Hurst function
#' t <- seq(0,1,by=(1/2)^10)
#' H <- function(x) {
#' ifelse(x >= 0 & x <= 0.8, 0.375 * x + 0.2,
#'       ifelse(x > 0.8 & x <= 1,-1.5 * x + 1.7, NA))
#' }
#' GHBMP(t,H)
#'
GHBMP<-function(t,H,J=15,num.cores=availableCores(omit = 1))
{
  if (!is.numeric(t)|!all(t >= 0 & t<= 1)) {
    stop("t must be a numeric sequence between 0 and 1")
  }

  H.t<-sapply(t, H)
  if (!is.numeric(H.t) | !all(H.t >= 0 & H.t<= 1)) {
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

  t<-sort(t)

  x1 <- 0:J
  x2 <- 0:(2^(J)-1)

  Aind <- outer(x1,x2, function(x,y) as.integer(as.logical(y<2^x)))
  Aindv <- unlist(asplit(Aind, 2))
  ind0 <- which(Aindv >0)

  A1 <- outer(x1,x2, function(x,y) 2^x)
  A2 <- outer(x1,x2, function(x,y) y)

  m <- rnorm((J+1) * 2^(J))
  dim(m) <- c(J+1, 2^(J))

  H1 <- outer(x1,x2, function(x,y) H(y/2^x))

  A3 <- outer(x1,x2, function(x,y) 2^(-x*H(y/2^x)))

  m <- ((H1+0.5)^(-1))*m*A3

  A1v <- unlist(asplit(A1, 2))[ind0]
  A2v <- unlist(asplit(A2, 2))[ind0]
  mv <- unlist(asplit(m, 2))[ind0]
  Hv <- unlist(asplit(H1, 2))[ind0]

  Xt<-function(t)
  {
    tv <- A1v*t-A2v
    ind1 <- which(tv >0)
    ind2 <- which(tv >0.5)
    ind3 <- which(tv >1)
    return(sum(mv[ind1]*(tv[ind1]^(Hv[ind1]+0.5)))-2*sum(mv[ind2]*((tv[ind2]-0.5)^(Hv[ind2]+0.5)))+sum(mv[ind3]*((tv[ind3]-1)^(Hv[ind3]+0.5))))
  }

  clusterExport(cl,c("t","A1v","A2v","Hv","mv"),envir = environment())
  XN <- do.call(c, parLapply(cl,t,Xt))

  sim_data<-data.frame("t"=t,"X"=XN)
  class(sim_data)<-c("mp",class(sim_data))
  return(sim_data)

  stopCluster(cl)
  options(warn = 0)
}




#' Simulation of Brownian motion
#'
#' @description
#' This function simulates a realisation of the Brownian motion over
#' time interval \eqn{[t_{start},t_{end}]} with \code{N} increments and initial value \code{x1}.
#'
#' @param x1 Value of the process at the initial time point.
#' @param t_start Initial time point.
#' @param t_end Terminal time point.
#' @param N Number of time steps the interval \eqn{[t_{start},t_{end}]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the Brownian
#' motion is plotted.
#'
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of Brownian motion.
#'
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @importFrom stats rnorm
#'
#' @export Bm
#' @seealso \code{\link{GHBMP}}, \code{\link{FBM}}, \code{\link{sim_FGN}}
#'
#' @examples
#' Bm(x1=0,t_start=0,t_end=2,plot=TRUE)
Bm<-function(x1=0,t_start=0,t_end=1,N=1000,plot=FALSE)
{

  if (!is.numeric(x1)) {
    stop("x1 must be numeric")
  }

  if (!is.numeric(t_start)) {
    stop("t_start must be numeric")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  }

  if (!(t_start<t_end)) {
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

  diff <- (t_end-t_start)/N
  increments <- rnorm(N,mean=0,sd=sqrt(diff))
  X <- c(x1,x1+cumsum(increments))
  t <- seq(t_start,t_end,length.out=N+1)
  sim_data <- data.frame(t=t,X=X)

  if(plot){
    p<- ggplot(sim_data,aes(x=.data$t,y=.data$X)) +
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
#' time interval \eqn{[0,t_{end}]} for a provided Hurst parameter.
#'
#' @param H Hurst parameter which lies between 0 and 1.
#' @param t_end Terminal time point.
#' @param N Number of time steps the interval \eqn{[0,t_{end}]} is split into.
#' Default set to 1000.
#' @param plot Logical: If \code{TRUE}, the realisation of the fractional Brownian
#' motion is plotted.
#'
#' @return A data frame where the first column is \code{t} and second
#' column is simulated values of the realisation of fractional Brownian motion.
#'
#' @importFrom ggplot2 ggplot geom_line labs ggtitle aes
#' @importFrom stats toeplitz rnorm
#'
#' @export FBM
#' @seealso \code{\link{sim_FGN}}, \code{\link{Bm}}, \code{\link{GHBMP}}
#'
#' @examples
#' FBM(H=0.3,t_end=1,plot=TRUE)
FBM <- function(H,t_end,N=1000,plot=FALSE){

  if (!is.numeric(H) | !(H > 0 & H< 1)) {
    stop("H must be a number between 0 and 1")
  }

  if (!is.numeric(t_end)) {
    stop("t_end must be numeric")
  } else if ( !(t_end > 0)) {
    stop("Incorrect input for t_end")
  }

  if (!is.numeric(N)) {
    stop("N must be numeric")
  } else if (!(N %% 1 == 0) | !(N > 0)) {
    stop("N must be a positive integer")
  }

  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  diff <- t_end/N
  t <- seq(0,t_end,by=diff)

  c<- function(k,H){
    0.5 * ((k+1)^(2*H)+(abs(k-1))^(2*H)-2*(k)^(2*H))
  }

  cov_mat <- toeplitz(sapply(0:(N-1),c,H=H))

  L <- t(chol(cov_mat))

  z <- rnorm(N,0,1)

  x <- L%*%z

  B <- cumsum(x)
  B <- append(B, 0, after = 0)

  sim_data <- data.frame(t=t,X=B*diff^H)

  if(plot){
    p<- ggplot(sim_data,aes(x=.data$t,y=.data$X)) +
      geom_line() +
      labs(y="X(t)",x="t") +
      ggtitle("Realisation of fractional Brownian motion")

    print(p)
  }

  return(sim_data)

}

#' Simulation of fractional Gaussian noise
#'
#' @description
#' This function simulates a realisation of the fractional Gaussian noise over
#' time interval \eqn{[t_{start},t_{end}]} for a provided Hurst parameter.
#'
#' @param H Hurst parameter which lies between 0 and 1.
#' @param t_start Initial time point.
#' @param t_end Terminal time point.
#' @param N Number of time steps the interval \eqn{[t_{start},t_{end}]} is split into.
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
#' @export
#' @seealso \code{\link{FBM}}, \code{\link{Bm}}, \code{\link{GHBMP}}
#' @examples
#' sim_FGN(H=0.3,t_start=0,t_end=1,plot=TRUE)
sim_FGN <- function(H,t_start,t_end,N=1000,plot=FALSE){

  diff <- t_end/N
  t <- seq(t_start,t_end,length.out=N)

  c<- function(k,H){
    0.5 * ((k+1)^(2*H)+(abs(k-1))^(2*H)-2*(k)^(2*H))
  }

  cov_mat <- toeplitz(sapply(0:(N-1),c,H=H))

  L <- t(chol(cov_mat))

  z <- rnorm(N,0,1)

  x <- L%*%z

  sim_data <- data.frame(t=t,X=x)

  if(plot){
    p<- ggplot(sim_data,aes(x=.data$t,y=.data$X))+
      geom_line() +
      labs(y="X(t)",x="t") +
      ggtitle("Realisation of fractional Guassian noise")

    print(p)
  }

  return(sim_data)

}
