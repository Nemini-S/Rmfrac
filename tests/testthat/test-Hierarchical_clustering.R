#hclust_hurst

test_that("hclust_hurst works", {
  expect_equal(sort(HC_actual_1$cluster_sizes), sort(c(3, 1)))
  expect_equal(sort(HC_actual_1$cluster_info[,3]), sort(c(0.0000000, 0.8247233, 0.7271482, 0.7361240)), tolerance = 1e-6)
})

test_that("hclust_hurst works", {
  expect_equal(sort(HC_actual_2$cluster_sizes), sort(c(1, 3)))
  expect_equal(sort(HC_actual_2$cluster_info[,3]), sort(c(0.0000000, 0.8247233, 0.7271482, 0.7361240)), tolerance = 1e-6)
})

test_that("hclust_hurst has the specified class", {
  expect_s3_class(HC_actual_1, c("hc_hurst", "list"))
})

test_that("hclust_hurst fails if X.t is not a list of data frames", {
  expect_error(hclust_hurst(data.frame(t = c(1, 2, 3), x = c(0.2788,0.1365,0.5567)), k = 2))
  expect_error(hclust_hurst(c(0.2788, 0.1365, 0.5567), k = 2))
  expect_error(hclust_hurst(list(0.2788, 0.1365, 0.5567), k = 2))
})

test_that("hclust_hurst fails if any data frame in X.t is not numeric", {
  expect_error(hclust_hurst(list(data.frame(t = c("A", 2, 3), x = c(0.2788,0.1365,0.5567)), data.frame(t = c(1, 2, 3), x = c(-0.2788,0.1365,-0.5567))), k = 1))
  expect_error(hclust_hurst(list(data.frame(t = c(1, 2, 3), x = c("A",0.1365,0.5567)), data.frame(t = c(1, 2, 3), x = c(-0.2788,0.1365,-0.5567))), k = 1))
})

test_that("hclust_hurst fails if any data frame in X.t has less than or more than 2 columns", {
  expect_error(hclust_hurst(list(data.frame(t = c(1, 2, 3)), data.frame(t = c(1, 2, 3), x = c(-0.2788,0.1365,-0.5567))), k = 1))
  expect_error(hclust_hurst(list(data.frame(t = c(1, 2, 3)), data.frame(t = c(1, 2, 3), x = c(-0.2788,0.1365,-0.5567), y = c(0.2788,0.1365,0.5567))), k = 1))
})

test_that("hclust_hurst fails if first column in any data frame in X.t has negative values", {
  expect_error(hclust_hurst(list(data.frame(t = c(-1, 2, 3), x = c(0.2788,0.1365,0.5567)), data.frame(t = c(1, 2, 3), x = c(-0.2788,0.1365,-0.5567))), k = 1))
})

test_that("hclust_hurst fails if either k or h not provided", {
  expect_error(hclust_hurst(X.list))
})

test_that("hclust_hurst fails for irrelevant k", {
  expect_error(hclust_hurst(X.list, k = "A"))
  expect_error(hclust_hurst(X.list, k = 2.5))
  expect_error(hclust_hurst(X.list, k = -2))
  expect_error(hclust_hurst(X.list, k = length(X.list) +2))
})

test_that("hclust_hurst fails if h is nonnumeric", {
  expect_error(hclust_hurst(X.list, h = "A"))
})

test_that("hclust_hurst fails if h is negative", {
  expect_error(hclust_hurst(X.list, h = -1.5))
})

test_that("hclust_hurst fails for incorrect dist.method", {
  expect_error(hclust_hurst(X.list, k = 2, dist.method = "Any"))
})

test_that("hclust_hurst fails for incorrect method", {
  expect_error(hclust_hurst(X.list, k = 2, method = "Any"))
})

test_that("hclust_hurst fails for non logical dendrogram", {
  expect_error(hclust_hurst(X.list, k = 2, dendrogram = 1))
  expect_error(hclust_hurst(X.list, k = 2, dendrogram = "TRUE"))
})

test_that("hclust_hurst fails if N is nonnumeric", {
  expect_error(hclust_hurst(X.list, k = 2, N = "A"))
})

test_that("hclust_hurst fails if N is negative", {
  expect_error(hclust_hurst(X.list, k = 2, N = -100))
})

test_that("hclust_hurst fails if N is non integer", {
  expect_error(hclust_hurst(X.list, k = 2, N = 100.5))
})

test_that("hclust_hurst fails if Q is nonnumeric", {
  expect_error(hclust_hurst(X.list, k = 2, Q = "A"))
})

test_that("hclust_hurst fails if Q is less than 2", {
  expect_error(hclust_hurst(X.list, k = 2, Q = -2))
  expect_error(hclust_hurst(X.list, k = 2, Q = 1))
})

test_that("hclust_hurst fails if Q is non integer", {
  expect_error(hclust_hurst(X.list, k = 2, Q = 5.5))
})

test_that("hclust_hurst fails if L is nonnumeric", {
  expect_error(hclust_hurst(X.list, k = 2, L = "A"))
})

test_that("hclust_hurst fails if L is less than 2", {
  expect_error(hclust_hurst(X.list, k = 2, L = -2))
  expect_error(hclust_hurst(X.list, k = 2, L = 1))
})

test_that("hclust_hurst fails if L is non integer", {
  expect_error(hclust_hurst(X.list, k = 2, L = 5.5))
})
