#cross_T

test_that("cross_T works", {
  expect_equal(cross_T(TS_Ana_2, 0.1), c(0.1316191, 0.3311314, 0.3890816, 0.6899009), tolerance = 1e-6)
  expect_equal(cross_T(TS_Ana_2, 0.1, subI = c(0.2,0.5)), c(0.3311314, 0.3890816), tolerance = 1e-6)
})

test_that("cross_T fails if X is not a data frame", {
  expect_error(cross_T(c(0.2788,0.1365,0.5567), 0.1))
})

test_that("cross_T fails if X is a data frame with more than or less than 2 columns", {
  expect_error(cross_T(data.frame(x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_T(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567)), 0.1))
})

test_that("cross_T fails if X is a nonnumeric data frame", {
  expect_error(cross_T(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_T(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567)), 0.1))
})

test_that("cross_T fails if X[,1] have negative values", {
  expect_error(cross_T(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567)), 0.1))
})

test_that("cross_T fails if A is nonnumeric", {
  expect_error(cross_T(TS_Ana_2, "A"))
})

test_that("cross_T fails if subI is not a numeric vector with 2 elemenTS_Ana_2", {
  expect_error(cross_T(TS_Ana_2, 0.1, subI = c(0,1, "A")))
  expect_error(cross_T(TS_Ana_2, 0.1, subI = c(0,1, 0.2, 0.3)))
})

test_that("cross_T fails if the second element of subI is larger than the first", {
  expect_error(cross_T(TS_Ana_2, 0.1, subI = c(0,5, 0.2)))
})

test_that("cross_T fails for non logical plot", {
  expect_error(cross_T(TS_Ana_2, 0.1, plot = 1))
  expect_error(cross_T(TS_Ana_2, 0.1, plot = "TRUE"))
})

test_that("cross_T fails for non logical vline", {
  expect_error(cross_T(TS_Ana_2, 0.1, vline = 1))
  expect_error(cross_T(TS_Ana_2, 0.1, vline = "TRUE"))
})

#cross_rate

test_that("cross_rate works", {
  expect_equal(cross_rate(TS_Ana_2, 0.1), 4, tolerance = 1e-0)
  expect_equal(cross_rate(TS_Ana_2, 0.1, subI = c(0.2,0.5)), 9, tolerance = 1e-0)
})

test_that("cross_rate fails if X is not a data frame", {
  expect_error(cross_rate(c(0.2788,0.1365,0.5567), 0.1))
})

test_that("cross_rate fails if X is a data frame with more than or less than 2 columns", {
  expect_error(cross_rate(data.frame(x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_rate(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567)), 0.1))
})

test_that("cross_rate fails if X is a nonnumeric data frame", {
  expect_error(cross_rate(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_rate(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567)), 0.1))
})

test_that("cross_rate fails if X[,1] have negative values", {
  expect_error(cross_rate(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567)), 0.1))
})

test_that("cross_rate fails if A is nonnumeric", {
  expect_error(cross_rate(TS_Ana_2, "A"))
})

test_that("cross_rate fails if subI is not a numeric vector with 2 elemenTS_Ana_2", {
  expect_error(cross_rate(TS_Ana_2, 0.1, subI = c(0,1, "A")))
  expect_error(cross_rate(TS_Ana_2, 0.1, subI = c(0,1, 0.2, 0.3)))
})

test_that("cross_rate fails if the second element of subI is larger than the first", {
  expect_error(cross_rate(TS_Ana_2, 0.1, subI = c(0,5, 0.2)))
})

test_that("cross_rate fails for non logical plot", {
  expect_error(cross_rate(TS_Ana_2, 0.1, plot = 1))
  expect_error(cross_rate(TS_Ana_2, 0.1, plot = "TRUE"))
})

#cross_mean

test_that("cross_mean works", {
  expect_equal(cross_mean(TS_Ana_2, 0.1), 0.186094, tolerance = 1e-6)
  expect_equal(cross_mean(TS_Ana_2, 0.1, subI = c(0.2,0.5)), 0.05795013, tolerance = 1e-6)
})

test_that("cross_mean fails if X is not a data frame", {
  expect_error(cross_mean(c(0.2788,0.1365,0.5567), 0.1))
})

test_that("cross_mean fails if X is a data frame with more than or less than 2 columns", {
  expect_error(cross_mean(data.frame(x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_mean(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567)), 0.1))
})

test_that("cross_mean fails if X is a nonnumeric data frame", {
  expect_error(cross_mean(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567)), 0.1))
  expect_error(cross_mean(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567)), 0.1))
})

test_that("cross_mean fails if X[,1] have negative values", {
  expect_error(cross_mean(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567)), 0.1))
})

test_that("cross_mean fails if A is nonnumeric", {
  expect_error(cross_mean(TS_Ana_2, "A"))
})

test_that("cross_mean fails if subI is not a numeric vector with 2 elemenTS_Ana_2", {
  expect_error(cross_mean(TS_Ana_2, 0.1, subI = c(0,1, "A")))
  expect_error(cross_mean(TS_Ana_2, 0.1, subI = c(0,1, 0.2, 0.3)))
})

test_that("cross_mean fails if the second element of subI is larger than the first", {
  expect_error(cross_mean(TS_Ana_2, 0.1, subI = c(0,5, 0.2)))
})

test_that("cross_mean fails for non logical plot", {
  expect_error(cross_mean(TS_Ana_2, 0.1, plot = 1))
  expect_error(cross_mean(TS_Ana_2, 0.1, plot = "TRUE"))
})

#long_streak

test_that("long_streak works", {
  expected_long_streak <- data.frame(streak_no = 1, t_start = 0.2222222, t_end = 0.3333333, X_start = 1.558708, X_end = 0.07050839)
  expect_equal(long_streak(TS_Ana_2, direction = 'decreasing', subI = c(0.2,0.5)), expected_long_streak, tolerance = 1e-6)
})

test_that("long_streak fails if X is not a data frame", {
  expect_error(long_streak(c(0.2788,0.1365,0.5567)))
})

test_that("long_streak fails if X is a data frame with more than or less than 2 columns", {
  expect_error(long_streak(data.frame(x = c(0.2788, 0.1365, 0.5567))))
  expect_error(long_streak(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567))))
})

test_that("long_streak fails if X is a nonnumeric data frame", {
  expect_error(long_streak(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567))))
  expect_error(long_streak(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567))))
})

test_that("long_streak fails if X[,1] have negative values", {
  expect_error(long_streak(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567))))
})

test_that("long_streak fails for irrelavant inpuTS_Ana_2 for level", {
  expect_error(long_streak(TS_Ana_2, direction = "increase"))
})

test_that("long_streak fails if subI is not a numeric vector with 2 elemenTS_Ana_2", {
  expect_error(long_streak(TS_Ana_2, subI = c(0,1, "A")))
  expect_error(long_streak(TS_Ana_2, subI = c(0,1, 0.2, 0.3)))
})

test_that("long_streak fails if the second element of subI is larger than the first", {
  expect_error(long_streak(TS_Ana_2, subI = c(0,5, 0.2)))
})

test_that("long_streak fails for non logical plot", {
  expect_error(long_streak(TS_Ana_2, plot = 1))
  expect_error(long_streak(TS_Ana_2, plot = "TRUE"))
})

#mean_streak

test_that("mean_streak works", {
  expect_equal(mean_streak(TS_Ana_2, direction = 'increasing'), 0.2222222, tolerance = 1e-6)
  expect_equal(mean_streak(TS_Ana_2, direction = 'increasing', subI = c(0.2,0.5)), 0.1111111, tolerance = 1e-6)
  expect_equal(mean_streak(TS_Ana_2, direction = 'decreasing'), 0.1666667, tolerance = 1e-6)
  expect_equal(mean_streak(TS_Ana_2, direction = 'decreasing', subI = c(0.2,0.5)), 0.1111111, tolerance = 1e-6)
})

test_that("mean_streak fails if X is not a data frame", {
  expect_error(mean_streak(c(0.2788,0.1365,0.5567)))
})

test_that("mean_streak fails if X is a data frame with more than or less than 2 columns", {
  expect_error(mean_streak(data.frame(x = c(0.2788, 0.1365, 0.5567))))
  expect_error(mean_streak(data.frame(t = c(1, 2, 3), x = c(0.2788, 0.1365, 0.5567), y = c(0.2788,0.1365,0.5567))))
})

test_that("mean_streak fails if X is a nonnumeric data frame", {
  expect_error(mean_streak(data.frame(t = c("A", "B", "C"), x = c(0.2788, 0.1365, 0.5567))))
  expect_error(mean_streak(data.frame(t = c(1, 2, 3), x = c(0.2788, "A", 0.5567))))
})

test_that("mean_streak fails if X[,1] have negative values", {
  expect_error(mean_streak(data.frame(t = c(1, -2, 3), x = c(0.2788, 0.1365, 0.5567))))
})

test_that("mean_streak fails for irrelavant inpuTS_Ana_2 for level", {
  expect_error(mean_streak(TS_Ana_2, direction = "increase"))
})

test_that("mean_streak fails if subI is not a numeric vector with 2 elemenTS_Ana_2", {
  expect_error(mean_streak(TS_Ana_2, subI = c(0,1, "A")))
  expect_error(mean_streak(TS_Ana_2, subI = c(0,1, 0.2, 0.3)))
})

test_that("mean_streak fails if the second element of subI is larger than the first", {
  expect_error(mean_streak(TS_Ana_2, subI = c(0,5, 0.2)))
})

test_that("mean_streak fails for non logical plot", {
  expect_error(mean_streak(TS_Ana_2, plot = 1))
  expect_error(mean_streak(TS_Ana_2, plot = "TRUE"))
})


#RS_Index

test_that("RS_Index works", {
  expect_equal(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7),
               c(NA, NA, NA, NA, NA, NA, NA, 68.75000, 73.52025, 80.83427), tolerance = 1e-4)
})

test_that("RS_Index fails if X is not a numeric vector with enough data", {
  expect_error(RS_Index(c(74.44, "A", 74.25, "B", 74.37, 74.73, "C", 75.46, 75.88, 76.78), period = 7))
  expect_error(RS_Index(c(74.44, 74.19), period = 5))
})

test_that("RS_Index fails if period is nonnumeric", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = "A"))
})

test_that("RS_Index fails for negative period", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = -10))
})

test_that("RS_Index fails for non integer period", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 2.5))
})

test_that("mean_streak fails for non logical plot", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, plot = 1))
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, plot = "TRUE"))
})

test_that("RS_Index fails if overbought is nonnumeric and not in the correct range", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, overbought = TRUE))
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, overbought = 150))
})

test_that("RS_Index fails if oversold is nonnumeric and not in the correct range", {
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, oversold = TRUE))
  expect_error(RS_Index(c(74.44, 74.19, 74.25, 73.65, 74.37, 74.73, 75.15, 75.46, 75.88, 76.78), period = 7, oversold = 150))
})
