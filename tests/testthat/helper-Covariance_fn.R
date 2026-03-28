set.seed(123)
X.t_Bm <- replicate(10, Bm(N = 10), simplify = FALSE)
X_Bm <- do.call(rbind, lapply(X.t_Bm, function(df) df[, 2]))
t <- seq(0, 1, by = 0.1)
Data_Bm <- data.frame(t, t(X_Bm))

H <- function(t) {return(0.2 + 0.45*t)}




