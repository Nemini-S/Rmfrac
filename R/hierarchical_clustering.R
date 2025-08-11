#' Hierarchical clustering
#'
#' @description
#' This function performs hierarchical clustering of realisations
#' based on the estimated Hurst functions.
#'
#' @param X.t A list of data frames. In each data frame, the first column is a numeric time sequence
#' and the second gives the values of the multifractional process. To get reliable results, it is
#' recommended to use at least 500 time points. See Examples for usage.
#'
#' @param k The desired number of clusters.
#' @param h The height where the dendrogram should be cut into. Either \code{k} or \code{h} must be specified. If both are provided k is used.
#' @param dist.method A string which specifies a registered distance from [`proxy::dist()`][proxy::dist]. The default
#' is \code{"euclidean"}.
#' @param method A string which specifies the hierarchical method used. Available methods
#' are \code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} and \code{"centroid"}.
#' The default method is \code{"complete"}.
#' @param dendrogram Logical: If \code{TRUE} the dendrogram is plotted indicating
#' the clusters.
#' @param N Argument used for the estimation of Hurst functions. Number of sub-intervals on which the estimation is performed on. Default is set to 100 sub-intervals.
#' @param Q Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#' @param L Argument used for the estimation of Hurst functions. Fixed integer greater than or equal to 2. Default is set to 2.
#'
#' @return An object list of class \code{"hc_hurst"} with print and plot methods. The list has following components:
#' \describe{
#'   \item{\code{cluster_info}}{A data frame indicating the cluster number and distance to cluster center from
#'   each smoothed estimated Hurst function (item). Distance is obtained from the \code{dist.method}.}
#'   \item{\code{cluster}}{A vector with cluster number of each item.}
#'   \item{\code{cluster_sizes}}{Number of items in each cluster.}
#'   \item{\code{centers}}{A data frame of cluster centers. Center obtained as the average of each smoothed estimated Hurst function in the cluster.
#'   Columns denote time points in which estimates were obtained. Row names denote cluster numbers.}
#'   \item{\code{smoothed_Hurst_estimates}}{A data frame of smoothed Hurst estimates. Columns denote time points in which estimates were obtained.
#'   Rows denote estimates for each realisation.}
#'   \item{\code{raw_Hurst_estimates}}{A list of data frames of raw Hurst estimates.}
#'   \item{\code{call}}{Information about the input parameters used.}
#' }
#'
#'
#' @details
#' The Hurst function of each realisation is estimated using the function \code{\link{Hurst}} and the smoothed Hurst estimates are used
#' for the cluster analysis. The distances between smoothed Hurst estimates are  computed by the dist.method provided and passed into
#' the \code{\link[stats]{hclust}} for hierarchical clustering.
#'
#' @importFrom proxy dist
#' @importFrom stats loess hclust cutree rect.hclust as.dendrogram
#' @export hclust_hurst
#'
#' @seealso \code{\link{print.hc_hurst}}, \code{\link{plot.hc_hurst}}, \code{\link{kmeans_hurst}}
#'
#' @examples
#' #Simulation of multifractional processes
#' t <- seq(0,1,by=(1/2)^10)
#' H1 <- function(t) {return(0.1+0*t)}
#' H2 <- function(t) {return(0.2+0.45*t)}
#' H3 <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X.list.1 <- replicate(3, GHBMP(t,H1),simplify = FALSE)
#' X.list.2 <- replicate(3, GHBMP(t,H2),simplify = FALSE)
#' X.list.3 <- replicate(3, GHBMP(t,H3),simplify = FALSE)
#' X.list <- c(X.list.1,X.list.2,X.list.3)
#'
#' #Hierarchical clustering based on k=3 clusters with dendrogram plotted
#' HC <- hclust_hurst(X.list,k=3,dendrogram=TRUE)
#' print(HC)
#'
#' #Plot of smoothed Hurst functions in each cluster with cluster centers
#' plot(HC,type ="ec")
hclust_hurst<-function(X.t,k=NULL,h=NULL,dist.method="euclidean",method="complete",dendrogram=FALSE,N=100,Q=2,L=2)
{
  if (!is.list(X.t)) {
    stop("X.t must be a list of numeric data frames")
  }

  if (is.null(k) & is.null(h)){
    stop("Either k or h must be provided")
  }

  if (!is.logical(dendrogram)) {
    stop("dendrogram must have a logical input either TRUE or FALSE")
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

  H<-list()

  for (i in 1:length(X.t)) {
    H[[i]] <- Hurst(X.t[[i]],N,Q,L)
  }

  loess.fn<-function(df){
    sm<-loess(df[,2]~df[,1],data=df,span=0.3)
    return(sm$fitted)
  }

  Smooth_df0 <- data.frame(rbind(t(sapply(H,loess.fn))))
  Smooth_df <- as.data.frame(lapply(Smooth_df0, function(x) pmax(pmin(x, 1), 0)))

  Dist<-dist(Smooth_df,method = dist.method)

  hc<-hclust(Dist,method=method)

  if (!is.null(k)){

    if (!is.numeric(k) | !(k %% 1 == 0) | !(k > 0) | !(k <= length(X.t))) {
      stop(paste("k must be a numeric positive integer between 1 and ", length(X.t)))
    }

    clusters<-cutree(hc, k =k)
    n_cl<-k

    if(dendrogram)
    {
      plot(as.dendrogram(hc),ylab="Height",main=paste("Cluster dendogram - ",method))
      rect.hclust(hc, k = k, border = "red")
    }

  }

  else if (!is.null(h))

  {

    if (!is.numeric(h) | !(h >= 0)  ) {
      stop("h must be a numeric positive value")
    }

    clusters<-cutree(hc, h=h)
    n_cl<-length(unique(clusters))

    if(dendrogram)
    {
      plot(as.dendrogram(hc),ylab="Height",main=paste("Cluster dendrogram - ",method))
      rect.hclust(hc, h=h, border = "red")
    }
  }

  DF<-data.frame(Item=row.names.data.frame(Smooth_df),Cluster=factor(clusters))
  DF <- DF[order(clusters), ]
  rownames(DF)<-NULL

  cl_df<-list()
  center<-list()
  df1<-list()
  dist_df<-list()
  df2<-list()

  for(i in 1:n_cl){
    cl_df[[i]]<-Smooth_df[clusters == i,]
    center[[i]]<-colMeans(cl_df[[i]])
    df1[[i]]<-rbind(cl_df[[i]],center[[i]])
    dist_df[[i]]<-as.matrix(dist(df1[[i]],method=dist.method))
    df2[[i]]<-as.numeric(dist_df[[i]][1:(nrow(cl_df[[i]])), nrow(df1[[i]])])
  }

  clust_DF<-data.frame(DF,Distance_from_centre=unlist(df2))

  structure(list(cluster_info=clust_DF, cluster=clusters, cluster_sizes=as.vector(table(clusters)),
                 centers=as.data.frame(do.call(rbind,center)), smoothed_Hurst_estimates=Smooth_df,
                 raw_Hurst_estimates=H, call=match.call()),class="hc_hurst")
}


#' Print method for "hc_hurst" class objects
#'
#' @description
#' Prints the results of hierarchical clustering of realisations of processes.
#'
#'
#' @param x Object of class \code{"hc_hurst"}.
#' @param ... Other arguments
#'
#' @seealso \code{\link{hclust_hurst}}
#'
#' @exportS3Method Rmfrac::print
print.hc_hurst <- function(x, ...)
{
  cat("Hierarchical clustering with ", length(x$cluster_sizes), " clusters of sizes ",
      paste(x$cluster_sizes, collapse = ", "), "\n", sep = "")
  cat("\nClustering information with the distance from the cluster center:\n")
  print(x$cluster_info)
  cat("\nOther available components:\n", sep = "\n")
  print(names(x))
  invisible(x)
}

#' @importFrom ggplot2 autoplot ggplot facet_wrap geom_line labs aes
#' @importFrom rlang .data
#' @export
autoplot.hc_hurst<-function(object,...,type="estimates")
{
  smth_h<-object$smoothed_Hurst_estimates
  raw_h<-object$raw_Hurst_estimates
  cluster<-object$cluster
  cent<-object$centers

  DF<-data.frame(
    clus = rep(cluster, each = ncol(smth_h)),
    item = rep(1:nrow(smth_h), each = ncol(smth_h)),
    t = unlist(lapply(raw_h, function(df) df[[1]])),
    smth_est = as.vector(t(smth_h))
  )

  DF1<-aggregate(smth_est~clus+t,data=DF,FUN=mean)

  DF1["item"] <- DF1["clus"]

  if (type == "estimates")
  {
    p<-ggplot(DF, aes(.data$t, .data$smth_est, group = .data$item)) +
      facet_wrap(~clus, ncol=2, scales= "free_x") +
      geom_line(color = "black") +
      labs(title = "Smoothed Hurst estimates in each cluster",
           x = "Time", y = "Smoothed Hurst estimates")


  }
  else if (type == "centers")
  {
    p<-ggplot(DF1,aes(.data$t,.data$smth_est)) +
      facet_wrap(~clus, ncol=2, scales= "free_x") +
      geom_line(color = "red") +
      labs(title = "Cluster centers",
           x = "Time", y = "Smoothed Hurst estimates")


  }
  else if (type == "ec")
  {
    p<-ggplot(DF, aes(.data$t, .data$smth_est, group = .data$item)) +
      geom_line(color = "black") +
      geom_line(data = DF1, aes(.data$t, .data$smth_est), color = "red") +
      facet_wrap(~clus, ncol=2, scales= "free_x") +
      labs(title = "Smoothed Hurst estimates in each cluster and cluster center",
           x = "Time", y = "Smoothed Hurst estimates")


  }
  else
  {
    print("Invalid type")
  }


}

#' Plot smoothed Hurst functions in each cluster with cluster centers
#'
#' @description
#' Creates a plot of the smoothed Hurst functions of realisations of multifractional processes separately in each cluster with cluster centers using the return from
#' \code{\link{hclust_hurst}}. Options to plot only estimates, only centers or both are available.
#'
#' @param x Return from \code{\link{hclust_hurst}}.
#' @param type The type of plot required:
#' \describe{
#' \item{\code{"estimates"}}{Only the smoothed Hurst functions in each cluster.}
#' \item{\code{"centers"}}{Only the cluster centers. Center denotes average of all smoothed Hurst functions in the cluster}
#' \item{\code{"ec"}}{Both \code{"estimates"} and \code{"centers"}.}
#' }
#' @param ... Other arguments
#'
#' @return A ggplot object which is used to plot the relevant \code{type} of plot: \code{"estimates"}, \code{"centers"} or \code{"ec"}.
#' @exportS3Method Rmfrac::plot
#' @importFrom ggplot2 ggplot facet_wrap geom_line labs aes
#'
#' @seealso \code{\link{hclust_hurst}}
#'
#' @examples
#' #Simulation of multifractional processes
#' t <- seq(0,1,by=(1/2)^10)
#' H1 <- function(t) {return(0.1+0*t)}
#' H2 <- function(t) {return(0.2+0.45*t)}
#' H3 <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
#' X.list.1 <- replicate(3, GHBMP(t,H1),simplify = FALSE)
#' X.list.2 <- replicate(3, GHBMP(t,H2),simplify = FALSE)
#' X.list.3 <- replicate(3, GHBMP(t,H3),simplify = FALSE)
#' X.list <- c(X.list.1,X.list.2,X.list.3)
#'
#' #Hierarchical clustering based on k=3 clusters with dendrogram plotted
#' HC<- hclust_hurst(X.list,k=3,dendrogram=TRUE)
#' print(HC)
#'
#' #Plot of smoothed Hurst functions in each cluster with cluster centers
#' plot(HC,type ="ec")
plot.hc_hurst<-function(x,type="estimates",...) {
  print(autoplot(x,type=type))
}
