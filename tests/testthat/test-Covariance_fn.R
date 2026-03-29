#est_cov

test_that("est_cov works", {
  expected_cov_est <- c(0.004097762, 0.042825573, 0.125753473, 0.197736999, 0.299328563,
    0.487519993, 0.695189740, 0.809483046, 0.896743403, 0.887964940, 0.781976102)
  expect_equal(diag(est_cov(Data_Bm)), expected_cov_est, tolerance = 1e-6)
})

test_that("est_cov fails if X is not a data frame", {
  expect_error(est_cov(c(0.2788,0.1365,0.5567)))
})

test_that("est_cov fails if X is a nonnumeric data frame", {
  expect_error(est_cov(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567))))
  expect_error(est_cov(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567))))
})

test_that("est_cov fails if X[,1] have negative values", {
  expect_error(est_cov(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567))))
})

test_that("est_cov fails if theta is nonnumeric", {
  expect_error(est_cov(Data_Bm, theta = "A"))
})

test_that("est_cov fails for negative theta", {
  expect_error(est_cov(Data_Bm, theta = -10))
})

test_that("est_cov fails for non logical plot", {
  expect_error(est_cov(Data_Bm, plot = 1))
  expect_error(est_cov(Data_Bm, plot = "TRUE"))
})

#cov_GHBMP

test_that("cov_GHBMP fails for nonnumeric t", {
  expect_error(cov_GHBMP("a", H, J = 4))
})

test_that("cov_GHBMP fails for t not in [0,1]", {
  expect_error(cov_GHBMP(2:3, H, J = 4))
  expect_error(cov_GHBMP(-1, H, J = 4))
})

test_that("cov_GHBMP fails for empty t ", {
  expect_error(cov_GHBMP(c(), H, J = 4))
})

test_that("cov_GHBMP fails for H(t) not in (0,1)", {
  expect_error(cov_GHBMP(t, function(t) 2 + 0.5 * t, J = 4))
  expect_error(cov_GHBMP(t, function(t) -2 - 0.5 * t, J = 4))
})

test_that("cov_GHBMP fails for nonnumeric J ", {
  expect_error(cov_GHBMP(t, H, J = "a"))
})

test_that("cov_GHBMP fails for negative J", {
  expect_error(cov_GHBMP(t, H, J = -1))
})

test_that("cov_GHBMP fails for non integer J", {
  expect_error(cov_GHBMP(t, H, J = 1.5))
})

test_that("cov_GHBMP fails for nonnumeric num.cores", {
  expect_error(cov_GHBMP(t, H, J = 4, num.cores = "a"))
})

test_that("cov_GHBMP fails for negative num.cores", {
  expect_error(cov_GHBMP(t, H, J = 4, num.cores = -1))
})

test_that("cov_GHBMP fails for non integer num.cores", {
  expect_error(cov_GHBMP(t, H, J = 4, num.cores = 1.5))
})

test_that("cov_GHBMP fails if theta is nonnumeric", {
  expect_error(cov_GHBMP(t, H, J = 4, theta = "A"))
})

test_that("cov_GHBMP fails for negative theta", {
  expect_error(cov_GHBMP(t, H, J = 4, theta = -10))
})

test_that("cov_GHBMP fails for non logical plot", {
  expect_error(cov_GHBMP(t, H, J = 4, plot = 1))
  expect_error(cov_GHBMP(t, H, J = 4, plot = "TRUE"))
})
