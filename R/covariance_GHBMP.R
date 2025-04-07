#' Empirical covariance estimates of a process
#'
#' @description
#' Computes the empirical covariance estimates of a process, for each pair of time points in the
#' time sequence using N realizations of the process.
#'
#' @param X A data frame where the first column is the time sequence and the remaining
#' columns are the values of each realization of the process.
#' Each row represents one out of the N realizations.
#' @param plot Logical: If TRUE, a 3D surface plot of the covariance function is plotted.
#'
#' @return
#' An \eqn{m \times m} matrix, where \eqn{m} is the number of time points.
#' Each element represents the empirical covariance estimate for the
#' corresponding time points which are arranged in ascending order.
#'
#'
#' @importFrom plotly plot_ly
#'
#' @export est_cov
#'
#' @examples
#' #Matrix of empirical covariance estimates of the GHBMP with Hurst function H.
#' t <- seq(0,1,by=(1/2)^8)
#' H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X.t <- replicate(5, GHBMP(t,H), simplify = FALSE)
#' X <- do.call(rbind, lapply(X.t, function(df) df[, 2]))
#' Data <- data.frame(t,t(X))
#' cov.mat <- est_cov(Data,plot=TRUE)
#' cov.mat
#'
est_cov<-function(X,plot=FALSE)
{

    if (!is.data.frame(X) | !(all(sapply(X, is.numeric)))) {
      stop("X must be a numeric data frame")
    }

    if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
    }

    X <- X[order(X[[1]]), ]

    X.data <- t(X[, -1])
    t <- X[,1]

    m <- ncol(X.data)

    X_mean <- colMeans(X.data)

    C <- matrix(0, m, m)

    for (i in 1:m) {
      for (j in 1:i) {  # Compute only for j ≤ i (lower)
        C[i, j] <- mean(X.data[, i] * X.data[, j]) - X_mean[i] * X_mean[j]
      }
    }

    C[upper.tri(C)] <- t(C)[upper.tri(C)]

    if(plot)
    {
      cov.fig <- plot_ly(
        x = ~t, y = ~t, z = ~C,
        type = 'surface',
        colorbar = list(title = "Covariance"))

      cov.fig <- layout(cov.fig, scene = list (xaxis = list(title = "t"),
                                           yaxis = list(title = "s"),
                                           zaxis = list(title = "Covariance")))
      print(cov.fig)
    }

    return(C)

}


#' Covariance of Gaussian Haar-based multifractional processes
#'
#' @description
#' Computes the covariance matrix of a Gaussian Haar-based multifractional process.
#'
#' @param t Time point or time sequence on the interval [0,1].
#' @param H Hurst function which depends on \code{t} \eqn{(H(t))}.
#' @param J Positive integer. For large J values could be rather time consuming. Default is set to 8.
#' @param plot Logical: If TRUE, a 3D surface plot of the covariance function is plotted.
#' @param num.cores Number of cores to set up the clusters for parallel computing.
#'
#' @return An \eqn{m \times m} matrix, where \eqn{m} is the length of \code{t}.
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom parallelly availableCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom plotly plot_ly layout
#'
#' @references Ayache, A., Olenko, A. and Samarakoon, N. (2025).
#' On Construction, Properties and Simulation of Haar-Based Multifractional Processes. (submitted).
#'
#' @export cov_GHBMP
#'
#' @seealso \code{\link{GHBMP}}
#'
#' @examples
#' \dontrun{
#' #Covariance of a GHBMP with H=0.5-0.4* sin(6*3.14*t)
#' t <- seq(0,1,by=0.01)
#' H <- function(t) {return(0.5-0.4* sin(6*3.14*t))}
#' cov_GHBMP(t,H,plot=TRUE)
#' }
cov_GHBMP<-function(t,H,J=8,plot=FALSE,num.cores=availableCores(omit = 1))
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


  if (!is.logical(plot)) {
    stop("Plot must have logical inputs either TRUE or FALSE")
  }

  if (!is.numeric(num.cores)) {
    stop("num.cores must be numeric")
  } else if (!(num.cores %% 1 == 0) | !(num.cores > 0)) {
    stop("num.cores must be a positive integer")
  }

  h<-function(x,lambda)
  {
    ((lambda+0.5)^(-1))*((x*(x>0))^(lambda+0.5)-(2*(((x-0.5)*((x-0.5)>0))^(lambda+0.5)))+(((x-1)*((x-1)>0))^(lambda+0.5)))
  }

  int<-function(j,k,t,H1){
    H11 <- H1(t)
    return((2^(-j*H11))*h(((2^j)*t)-k,H11))
  }

  x1 <- 0:J
  x2 <- 0:(2^(J)-1)

  X<-function(t,s){
    A <- outer(x1,x2, function(j,k){int(j,k,t,H)*int(j,k,s,H)})
    return(sum(A))
  }

  cl <- makeCluster(num.cores)
  registerDoParallel(cl)

  i <- NULL
  cov.mat <- foreach(i = 1:length(t), .combine = rbind) %dopar% {
    sapply(t, function(j) X(t[i], j))}

  if(plot)
  {
    cov.fig <- plot_ly(
      x = ~t, y = ~t, z = ~cov.mat,
      type = 'surface',
      colorbar = list(title = "Covariance"))

    cov.fig <- layout(cov.fig, scene = list (xaxis = list(title = "t"),
                                             yaxis = list(title = "s"),
                                             zaxis = list(title = "Covariance")))
    print(cov.fig)
  }

  return(cov.mat)

  stopCluster(cl)
}

