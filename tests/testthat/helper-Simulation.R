# set.seed(123)
# time <- seq(0, 1, by = 0.15)
# H <- function(t) {return(0.2 + 0.45*t)}
# GHBMP_sim <- GHBMP(time, H, 2)

set.seed(123)
Bm_sim <- Bm(N = 10)

set.seed(123)
fBm_sim <- FBm(H = 0.5, N = 10)

set.seed(123)
fGn_sim <- FGn(H = 0.5, N = 10)

set.seed(123)
Bbridge_sim <- Bbridge(x_end = 1, t_end = 1, N = 10)

set.seed(123)
FBbridge_sim <- FBbridge(H = 0.5, x_end = 2, t_end = 1, N = 10)


