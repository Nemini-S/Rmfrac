test_that("H_LFD works", {
  Exp_1 <- c(0.6276124, 1.0000000, 1.0000000, 0.0000000, 1.0000000, 1.0000000,
    1.0000000, 0.0000000, 1.0000000, 1.0000000, 1.0000000, 0.0000000,
    1.0000000, 1.0000000, 0.0000000, 1.0000000, 1.0000000, 1.0000000, 0.0000000)
  Exp_2 <- c(1.372388, 1.000000, 1.000000, 2.000000, 1.000000, 1.000000, 1.000000,
    2.000000, 1.000000, 1.000000, 1.000000, 2.000000, 1.000000, 1.000000,
    2.000000, 1.000000, 1.000000, 1.000000, 2.000000)

  expect_equal(Actual_H_LFD$Raw_Hurst_estimates[,2], Exp_1, tolerance = 1e-5)
  expect_equal(Actual_H_LFD$Raw_LFD_estimates[,2], Exp_2, tolerance = 1e-5)
})

test_that("H_LFD has the specified class", {
  expect_s3_class(Actual_H_LFD, c("H_LFD", "list"))
})

test_that("H_LFD fails if X is not a data frame", {
  expect_error(H_LFD(c(0.2788,0.1365,0.5567)))
})

test_that("H_LFD fails if X is a data frame with more than or less than 2 columns", {
  expect_error(H_LFD(data.frame(x = c(0.2788,0.1365,0.5567))))
  expect_error(H_LFD(data.frame(t = c(1, 2, 3), x = c(0.2788,0.1365,0.5567), y =c(0.2788,0.1365,0.5567))))
})

test_that("H_LFD fails if X is a nonnumeric data frame", {
  expect_error(H_LFD(data.frame(t = c("A", "B", "C"), x = c(0.2788,0.1365,0.5567))))
  expect_error(H_LFD(data.frame(t = c(1, 2, 3), x = c(0.2788,"B",0.5567))))
})

test_that("H_LFD fails if X[,1] have negative values", {
  expect_error(H_LFD(data.frame(t = c(1, -2, 3), x = c(0.2788,0.1365,0.5567))))
})

test_that("H_LFD fails if N is nonnumeric", {
  expect_error(H_LFD(TS, "a"))
})

test_that("H_LFD fails if N is negative", {
  expect_error(H_LFD(TS, -100))
})

test_that("H_LFD fails if N is non integer", {
  expect_error(H_LFD(TS, 100.5))
})

test_that("H_LFD fails if Q is nonnumeric", {
  expect_error(H_LFD(TS, Q = "a"))
})

test_that("H_LFD fails if Q is less than 2", {
  expect_error(H_LFD(TS, Q = -2))
  expect_error(H_LFD(TS, Q = 1))
})

test_that("H_LFD fails if Q is non integer", {
  expect_error(H_LFD(TS, Q = 5.5))
})

test_that("H_LFD fails if L is nonnumeric", {
  expect_error(H_LFD(TS, L = "a"))
})

test_that("H_LFD fails if L is less than 2", {
  expect_error(H_LFD(TS, L = -2))
  expect_error(H_LFD(TS, L = 1))
})

test_that("H_LFD fails if L is non integer", {
  expect_error(H_LFD(TS, L = 5.5))
})

#plot_tsest

test_that("plot_tsest fails if X is not a data frame", {
  expect_error(plot_tsest(c(0.2788,0.1365,0.5567)))
})

test_that("plot_tsest fails if X is a data frame with more than or less than 2 columns", {
  expect_error(plot_tsest(data.frame(x = c(0.2788,0.1365,0.5567))))
  expect_error(plot_tsest(data.frame(t = c(1, 2, 3), x = c(0.2788,0.1365,0.5567), y =c(0.2788,0.1365,0.5567))))
})

test_that("plot_tsest fails if X is a nonnumeric data frame", {
  expect_error(plot_tsest(data.frame(t = c("A", "B", "C"), x = c(0.2788,0.1365,0.5567))))
  expect_error(plot_tsest(data.frame(t = c(1, 2, 3), x = c(0.2788,"B",0.5567))))
})

test_that("plot_tsest fails if X[,1] have negative values", {
  expect_error(plot_tsest(data.frame(t = c(1, -2, 3), x = c(0.2788,0.1365,0.5567))))
})

test_that("plot_tsest fails if H_Est or H_Smooth_Est or LFD_Est or LFD_Smooth_Est is non logical", {
  expect_error(plot_tsest(TS, H_Est = 1))
  expect_error(plot_tsest(TS, H_Smooth_Est = 1))
  expect_error(plot_tsest(TS, LFD_Est = 1))
  expect_error(plot_tsest(TS, LFD_Smooth_Est = 1))
})

test_that("plot_tsest fails if N is nonnumeric", {
  expect_error(plot_tsest(TS, "a"))
})

test_that("plot_tsest fails if N is negative", {
  expect_error(plot_tsest(TS, -100))
})

test_that("plot_tsest fails if N is non integer", {
  expect_error(plot_tsest(TS, 100.5))
})

test_that("plot_tsest fails if Q is nonnumeric", {
  expect_error(plot_tsest(TS, Q = "a"))
})

test_that("plot_tsest fails if Q is less than 2", {
  expect_error(plot_tsest(TS, Q = -2))
  expect_error(plot_tsest(TS, Q = 1))
})

test_that("plot_tsest fails if Q is non integer", {
  expect_error(plot_tsest(TS, Q = 5.5))
})

test_that("plot_tsest fails if L is nonnumeric", {
  expect_error(plot_tsest(TS, L = "a"))
})

test_that("plot_tsest fails if L is less than 2", {
  expect_error(plot_tsest(TS, L = -2))
  expect_error(plot_tsest(TS, L = 1))
})

test_that("plot_tsest fails if L is non integer", {
  expect_error(plot_tsest(TS, L = 5.5))
})
