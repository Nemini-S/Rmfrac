#Hurst()

test_that("Hurst works", {
  expect_equal(Hurst(df_est)$Hurst_estimate, c(0.0000000, 1.0000000, 0.0000000, 0.6266602, 1.0000000, 1.0000000, 0.4116034), tolerance = 1e-6)
})

test_that("Hurst fails if X is not a data frame", {
  expect_error(Hurst(c(0.2788,0.1365,0.5567)))
})

test_that("Hurst fails if X is a data frame with more than or less than 2 columns", {
  expect_error(Hurst(data.frame(x = c(0.2788,0.1365,0.5567))))
  expect_error(Hurst(data.frame(t = c(1, 2, 3), x = c(0.2788,0.1365,0.5567), y =c(0.2788,0.1365,0.5567))))
})

test_that("Hurst fails if X is a nonnumeric data frame", {
  expect_error(Hurst(data.frame(t = c("A", "B", "C"), x = c(0.2788,0.1365,0.5567))))
  expect_error(Hurst(data.frame(t = c(1, 2, 3), x = c(0.2788,"B",0.5567))))
  })

test_that("Hurst fails if X[,1] have negative values", {
  expect_error(Hurst(data.frame(t = c(1, -2, 3), x = c(0.2788,0.1365,0.5567))))
})

test_that("Hurst fails if N is nonnumeric", {
  expect_error(Hurst(df_est, "a"))
})

test_that("Hurst fails if N is negative", {
  expect_error(Hurst(df_est, -100))
})

test_that("Hurst fails if N is non integer", {
  expect_error(Hurst(df_est, 100.5))
})

test_that("Hurst fails if Q is nonnumeric", {
  expect_error(Hurst(df_est, Q = "a"))
})

test_that("Hurst fails if Q is less than 2", {
  expect_error(Hurst(df_est, Q = -2))
  expect_error(Hurst(df_est, Q = 1))
})

test_that("Hurst fails if Q is non integer", {
  expect_error(Hurst(df_est, Q = 5.5))
})

test_that("Hurst fails if L is nonnumeric", {
  expect_error(Hurst(df_est, L = "a"))
})

test_that("Hurst fails if L is less than 2", {
  expect_error(Hurst(df_est, L = -2))
  expect_error(Hurst(df_est, L = 1))
})

test_that("Hurst fails if L is non integer", {
  expect_error(Hurst(df_est, L = 5.5))
})

#LFD()

test_that("LFD works", {
  expected_est <- data.frame(
      Time = c(0.00, 0.12, 0.24, 0.25, 0.37, 0.49, 0.50),
      LFD_estimate = c(2.000000, 1.000000, 2.000000, 1.373340, 1.000000, 1.000000, 1.588397))
  expect_equal(LFD(df_est), expected_est, tolerance = 1e-5)
})

test_that("LFD fails if X is not a data frame", {
  expect_error(LFD(c(0.2788,0.1365,0.5567)))
})

test_that("Hurst fails if X is a data frame with more than or less than 2 columns", {
  expect_error(LFD(data.frame(x = c(0.2788,0.1365,0.5567))))
  expect_error(LFD(data.frame(t = c(1, 2, 3), x = c(0.2788,0.1365,0.5567), y =c(0.2788,0.1365,0.5567))))
})

test_that("LFD fails if X is a nonnumeric data frame", {
  expect_error(LFD(data.frame(t = c("A", "B", "C"), x = c(0.2788,0.1365,0.5567))))
  expect_error(LFD(data.frame(t = c(1, 2, 3), x = c(0.2788,"B",0.5567))))
})

test_that("LFD fails if X[,1] have negative values", {
  expect_error(LFD(data.frame(t = c(1, -2, 3), x = c(0.2788,0.1365,0.5567))))
})


test_that("LFD fails if N is nonnumeric", {
  expect_error(LFD(df_est, "a"))
})

test_that("LFD fails if N is negative", {
  expect_error(LFD(df_est, -100))
})

test_that("LFD fails if N is non integer", {
  expect_error(LFD(df_est, 100.5))
})

test_that("LFD fails if Q is nonnumeric", {
  expect_error(LFD(df_est, Q = "a"))
})

test_that("LFD fails if Q is less than 2", {
  expect_error(LFD(df_est, Q = -2))
  expect_error(LFD(df_est, Q = 1))
})

test_that("LFD fails if Q is non integer", {
  expect_error(LFD(df_est, Q = 5.5))
})

test_that("LFD fails if L is nonnumeric", {
  expect_error(LFD(df_est, L = "a"))
})

test_that("LFD fails if L is less than 2", {
  expect_error(LFD(df_est, L = -2))
  expect_error(LFD(df_est, L = 1))
})

test_that("LFD fails if L is non integer", {
  expect_error(LFD(df_est, L = 5.5))
})

