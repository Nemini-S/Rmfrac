#sojourn

test_that("sojourn works", {
  expect_equal(sojourn(TS_Ana_1, 0.8, N = 100), 0.2489572, tolerance = 1e-6)
  expect_equal(sojourn(TS_Ana_1, 0.8, N = 100, subI = c(0.2,0.5)), 0.05664624, tolerance = 1e-6)
  expect_equal(sojourn(TS_Ana_1, 0.8, N = 100, level = 'lower'), 0.7510428, tolerance = 1e-6)
  expect_equal(sojourn(TS_Ana_1, 0.8, N = 100, level = 'lower', subI = c(0.2,0.5)), 0.165576, tolerance = 1e-6)
})

test_that("sojourn fails if X is not a data frame", {
  expect_error(sojourn(c(0.2788, 0.1365, 0.5567), 0.8, N = 10))
})

test_that("sojourn fails if X is a data frame with more than or less than 2 columns", {
  expect_error(sojourn(data.frame(x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
  expect_error(sojourn(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567)), 0.8, N = 10))
})

test_that("sojourn fails if X is a nonnumeric data frame", {
  expect_error(sojourn(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
  expect_error(sojourn(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567)), 0.8, N = 10))
})

test_that("sojourn fails if X[,1] have negative values", {
  expect_error(sojourn(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
})

test_that("sojourn fails if A is nonnumeric", {
  expect_error(sojourn(TS_Ana_1, "A", N = 10))
})

test_that("sojourn fails if N is nonnumeric", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = "A"))
})

test_that("sojourn fails for negative N", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = -10))
})

test_that("sojourn fails for non integer N", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = 2.5))
})

test_that("sojourn fails for irrelavant inpuTS_Ana_1 for level", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, level = "high"))
})

test_that("sojourn fails if subI is not a numeric vector with 2 elemenTS_Ana_1", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, subI = c(0,1, "A")))
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, subI = c(0,1, 0.2, 0.3)))
})

test_that("sojourn fails if the second element of subI is larger than the first", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, subI = c(0,5, 0.2)))
})

test_that("sojourn fails for non logical plot", {
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, plot = 1))
  expect_error(sojourn(TS_Ana_1, 0.8, N = 10, plot = "TRUE"))
})

#exc_Area

test_that("exc_Area works", {
  expect_equal(exc_Area(TS_Ana_1, 0.8, N = 100), 0.1052229, tolerance = 1e-6)
  expect_equal(exc_Area(TS_Ana_1, 0.8, N = 100, subI = c(0.2,0.5)), 0.02148899, tolerance = 1e-6)
  expect_equal(exc_Area(TS_Ana_1, 0.8, N = 100, level = 'lower'), 0.7664732, tolerance = 1e-6)
  expect_equal(exc_Area(TS_Ana_1, 0.8, N = 100, level = 'lower', subI = c(0.2,0.5)), 0.09765494, tolerance = 1e-6)
})

test_that("exc_Area fails if X is not a data frame", {
  expect_error(exc_Area(c(0.2788, 0.1365, 0.5567), 0.8, N = 10))
})

test_that("exc_Area fails if X is a data frame with more than or less than 2 columns", {
  expect_error(exc_Area(data.frame(x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
  expect_error(exc_Area(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567)), 0.8, N = 10))
})

test_that("exc_Area fails if X is a nonnumeric data frame", {
  expect_error(exc_Area(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
  expect_error(exc_Area(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567)), 0.8, N = 10))
})

test_that("exc_Area fails if X[,1] have negative values", {
  expect_error(exc_Area(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567)), 0.8, N = 10))
})

test_that("exc_Area fails if A is nonnumeric", {
  expect_error(exc_Area(TS_Ana_1, "A", N = 10))
})

test_that("exc_Area fails if N is nonnumeric", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = "A"))
})

test_that("exc_Area fails for negative N", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = -10))
})

test_that("exc_Area fails for non integer N", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 2.5))
})

test_that("exc_Area fails for irrelavant inpuTS_Ana_1 for level", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, level = "high"))
})

test_that("exc_Area fails if subI is not a numeric vector with 2 elemenTS_Ana_1", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, subI = c(0,1, "A")))
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, subI = c(0,1, 0.2, 0.3)))
})

test_that("exc_Area fails if the second element of subI is larger than the first", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, subI = c(0,5, 0.2)))
})

test_that("exc_Area fails for non logical plot", {
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, plot = 1))
  expect_error(exc_Area(TS_Ana_1, 0.8, N = 10, plot = "TRUE"))
})

#X_max

test_that("X_max works", {
  expect_equal(X_max(TS_Ana_1), list(c(0.5555556, 1.7150650)), tolerance = 1e-6)
  expect_equal(X_max(TS_Ana_1, subI = c(0.5, 0.8)), list(c(0.5555556, 1.7150650)),  tolerance = 1e-6)
})

test_that("X_max fails if X is not a data frame", {
  expect_error(X_max(c(0.2788, 0.1365, 0.5567)))
})

test_that("X_max fails if X is a data frame with more than or less than 2 columns", {
  expect_error(X_max(data.frame(x = c(0.2788, 0.1365, 0.5567))))
  expect_error(X_max(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567))))
})

test_that("X_max fails if X is a nonnumeric data frame", {
  expect_error(X_max(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567))))
  expect_error(X_max(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567))))
})

test_that("X_max fails if X[,1] have negative values", {
  expect_error(X_max(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567))))
})

test_that("X_max fails if subI is not a numeric vector with 2 elemenTS_Ana_1", {
  expect_error(X_max(TS_Ana_1, subI = c(0,1, "A")))
  expect_error(X_max(TS_Ana_1, subI = c(0,1, 0.2, 0.3)))
})

test_that("X_max fails if the second element of subI is larger than the first", {
  expect_error(X_max(TS_Ana_1, subI = c(0,5, 0.2)))
})

test_that("X_max fails for non logical plot", {
  expect_error(X_max(TS_Ana_1, plot = 1))
  expect_error(X_max(TS_Ana_1, plot = "TRUE"))
})

test_that("X_max fails for non logical vline", {
  expect_error(X_max(TS_Ana_1, vline = 1))
  expect_error(X_max(TS_Ana_1, vline = "TRUE"))
})

test_that("X_max fails for non logical hline", {
  expect_error(X_max(TS_Ana_1, hline = 1))
  expect_error(X_max(TS_Ana_1, hline = "TRUE"))
})

#X_min

test_that("X_min works", {
  expect_equal(X_min(TS_Ana_1), list(c(0.7777778, -1.2650612)), tolerance = 1e-6)
  expect_equal(X_min(TS_Ana_1, subI = c(0.5, 0.8)), list(c(0.7777778, -1.2650612)),  tolerance = 1e-6)
})

test_that("X_min fails if X is not a data frame", {
  expect_error(X_min(c(0.2788, 0.1365, 0.5567)))
})

test_that("X_min fails if X is a data frame with more than or less than 2 columns", {
  expect_error(X_min(data.frame(x = c(0.2788, 0.1365, 0.5567))))
  expect_error(X_min(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567))))
})

test_that("X_min fails if X is a nonnumeric data frame", {
  expect_error(X_min(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567))))
  expect_error(X_min(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567))))
})

test_that("X_min fails if X[,1] have negative values", {
  expect_error(X_min(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567))))
})

test_that("X_min fails if subI is not a numeric vector with 2 elemenTS_Ana_1", {
  expect_error(X_min(TS_Ana_1, subI = c(0,1, "A")))
  expect_error(X_min(TS_Ana_1, subI = c(0,1, 0.2, 0.3)))
})

test_that("X_min fails if the second element of subI is larger than the first", {
  expect_error(X_min(TS_Ana_1, subI = c(0,5, 0.2)))
})

test_that("X_min fails for non logical plot", {
  expect_error(X_min(TS_Ana_1, plot = 1))
  expect_error(X_min(TS_Ana_1, plot = "TRUE"))
})

test_that("X_min fails for non logical vline", {
  expect_error(X_min(TS_Ana_1, vline = 1))
  expect_error(X_min(TS_Ana_1, vline = "TRUE"))
})

test_that("X_min fails for non logical hline", {
  expect_error(X_min(TS_Ana_1, hline = 1))
  expect_error(X_min(TS_Ana_1, hline = "TRUE"))
})

