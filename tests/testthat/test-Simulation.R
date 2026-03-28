#GHBMP()

# test_that("GHBMP works", {
#   expected_sim <- structure( data.frame(
#     t = c(0.00, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90),
#     X = c(0.00000000, 0.17136907, -0.22563262, -0.37522456, -0.46829054, -0.01514723, 0.15794085)),
#     class = c("mp", "data.frame"))
#
#   expect_equal(GHBMP_sim, expected_sim, tolerance = 1e-6)
# })
#
# test_that("GHBMP has the specified class", {
#   expect_s3_class(GHBMP_sim, c("mp", "data.frame"))
# })

test_that("GHBMP fails for nonnumeric t", {
  expect_error(GHBMP("a", H, 1))
})

test_that("GHBMP fails for t not in [0,1]", {
  expect_error(GHBMP(2:3, H, 1))
  expect_error(GHBMP(-1, H, 1))
})

test_that("GHBMP fails for empty t ", {
  expect_error(GHBMP(c(), H, 1))
})

test_that("GHBMP fails for H(t) not in (0,1)", {
  expect_error(GHBMP(0.1, function(t) 2 + 0.5 * t, 1))
  expect_error(GHBMP(0.1, function(t) -2 - 0.5 * t, 1))
})

test_that("GHBMP fails for nonnumeric J ", {
  expect_error(GHBMP(0.1, H, "a"))
})

test_that("GHBMP fails for negative J", {
  expect_error(GHBMP(0.1, H, -1))
})

test_that("GHBMP fails for non integer J", {
  expect_error(GHBMP(0.1, H, 1.5))
})

test_that("GHBMP fails for nonnumeric num.cores", {
  expect_error(GHBMP(0.1, H, 1, "a"))
})

test_that("GHBMP fails for negative num.cores", {
  expect_error(GHBMP(0.1, H, 1, -1))
})

test_that("GHBMP fails for non integer num.cores", {
  expect_error(GHBMP(0.1, H, 1, 1.5))
})


#Bm()

test_that("Bm works", {

  expected_sim <- data.frame(
    t = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    X = c(0.0000000, -0.1772380, -0.2500265, 0.2428804, 0.2651771, 0.3060615, 0.8484126, 0.9941671, 0.5941196, 0.3769177, 0.2359870))

  expect_equal(Bm_sim, expected_sim, tolerance = 1e-6)

})

test_that("Bm fails if x_start is nonnumeric", {
  expect_error(Bm("a", N = 5))
})

test_that("Bm fails if t_start is nonnumeric", {
  expect_error(Bm(t_start = "a", N = 5))
})

test_that("Bm fails if t_start is negative", {
  expect_error(Bm(t_start = -1, N = 5))
})

test_that("Bm fails if t_end is nonnumeric", {
  expect_error(Bm(t_end = "a", N = 5))
})

test_that("Bm fails if t_end is negative", {
  expect_error(Bm(t_end = -1, N = 5))
})

test_that("Bm fails if t_end < t_start", {
  expect_error(Bm(t_start = 5, t_end = 4, N = 5))
})

test_that("Bm fails for nonnumeric N ", {
  expect_error(Bm(N = "a"))
})

test_that("Bm fails for negative N", {
  expect_error(Bm(N = -1))
})

test_that("Bm fails for non integer N", {
  expect_error(Bm(N = 1.5))
})

test_that("Bm fails for non logical plot", {
  expect_error(Bm(plot = 1, N = 5))
  expect_error(Bm(plot = "TRUE", N = 5))
})

#FBm()

test_that("FBm works", {

  expected_sim <- data.frame(
    t = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    X = c(0.0000000, -0.1772380, -0.2500265, 0.2428804, 0.2651771, 0.3060615, 0.8484126, 0.9941671, 0.5941196, 0.3769177, 0.2359870))

  expect_equal(fBm_sim, expected_sim, tolerance = 1e-6)

})

test_that("FBm fails for nonnumeric H ", {
  expect_error(FBm("a", N = 5))
})

test_that("FBm fails for H not in (0,1)", {
  expect_error(FBm(-2, N = 5))
  expect_error(FBm(2, N = 5))
})

test_that("FBm fails if x_start is nonnumeric", {
  expect_error(FBm(0.5, "a", N = 5))
})

test_that("FBm fails if t_start is nonnumeric", {
  expect_error(FBm(0.5, t_start = "a", N = 5))
})

test_that("FBm fails if t_start is negative", {
  expect_error(FBm(0.5, t_start = -1, N = 5))
})

test_that("FBm fails if t_end is nonnumeric", {
  expect_error(FBm(0.5, t_end = "a", N = 5))
})

test_that("FBm fails if t_end is negative", {
  expect_error(FBm(0.5, t_end = -1, N = 5))
})

test_that("FBm fails if t_end < t_start", {
  expect_error(FBm(0.5, t_start = 5, t_end = 4, N = 5))
})

test_that("FBm fails for nonnumeric N ", {
  expect_error(FBm(0.5, N = "a"))
})

test_that("FBm fails for negative N", {
  expect_error(FBm(0.5, N = -1))
})

test_that("FBm fails for non integer N", {
  expect_error(FBm(0.5, N = 1.5))
})

test_that("FBm fails for non logical plot", {
  expect_error(FBm(0.5, plot = 1, N = 5))
  expect_error(FBm(0.5, plot = "TRUE", N = 5))
})

#FGn()

test_that("FGn works", {

  expected_sim <- data.frame(
    t = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    X = c(-0.56047565, -0.23017749, 1.55870831, 0.07050839, 0.12928774,
          1.71506499, 0.46091621, -1.26506123, -0.68685285, -0.44566197, 1.22408180))

  expect_equal(fGn_sim, expected_sim, tolerance = 1e-6)

})

test_that("FGn fails for nonnumeric H ", {
  expect_error(FGn("a", N = 5))
})

test_that("FGn fails for H not in (0,1)", {
  expect_error(FGn(-2, N = 5))
  expect_error(FGn(2, N = 5))
})

test_that("FGn fails if t_start is nonnumeric", {
  expect_error(FGn(0.5, t_start = "a", N = 5))
})

test_that("FGn fails if t_start is negative", {
  expect_error(FGn(0.5, t_start = -1, N = 5))
})

test_that("FGn fails if t_end is nonnumeric", {
  expect_error(FGn(0.5, t_end = "a", N = 5))
})

test_that("FGn fails if t_end is negative", {
  expect_error(FGn(0.5, t_end = -1, N = 5))
})

test_that("FGn fails if t_end < t_start", {
  expect_error(FGn(0.5, t_start = 5, t_end = 4, N = 5))
})

test_that("FGn fails for nonnumeric N ", {
  expect_error(FGn(0.5, N = "a"))
})

test_that("FGn fails for negative N", {
  expect_error(FGn(0.5, N = -1))
})

test_that("FGn fails for non integer N", {
  expect_error(FGn(0.5, N = 1.5))
})

test_that("FGn fails for non logical plot", {
  expect_error(FGn(0.5, plot = 1, N = 5))
  expect_error(FGn(0.5, plot = "TRUE", N = 5))
})

#Bbridge()

test_that("Bbridge works", {

  expected_sim <- data.frame(
    t = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    X = c(0.00000000, -0.10083666, -0.09722388, 0.47208427, 0.57078228,
          0.68806795, 1.30682042, 1.52897622, 1.20533003, 1.06452939, 1.00000000))

  expect_equal(Bbridge_sim, expected_sim, tolerance = 1e-6)

})

test_that("Bbridge fails for nonnumeric x_end", {
  expect_error(Bbridge("a", 1, N = 5))
})

test_that("Bbridge fails for nonnumeric t_end", {
  expect_error(Bbridge(2, "a", N = 5))
})

test_that("Bbridg fails if t_end is negative or 0", {
  expect_error(Bbridge(2, -2, N = 5))
  expect_error(Bbridge(2, 0, N = 5))
})

test_that("Bbridge fails for nonnumeric x_start", {
  expect_error(Bbridge(2, 1, "a", N = 5))
})

test_that("Bbridge fails for nonnumeric N", {
  expect_error(Bbridge(2, 1, N = "a"))
})

test_that("Bbridge fails for negative N", {
  expect_error(Bbridge(2, 1, N = -2))
})

test_that("Bbridge fails for non integer N", {
  expect_error(Bbridge(2, 1, N = 1.5))
})

test_that("Bbridge fails for non logical plot", {
  expect_error(Bbridge(2, 1, N = 5, plot = 1))
  expect_error(Bbridge(2, 1, N = 5, plot = "TRUE"))
})

#FBbridge()

test_that("FBbridge works", {

  expected_sim <- data.frame(
    t = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    X = c(0.0000000000, -0.0008366623, 0.1027761237, 0.7720842710, 0.9707822814,
          1.1880679523, 1.9068204210, 2.2289762224, 2.0053300336, 1.9645293899, 2.0000000000))

  expect_equal(FBbridge_sim, expected_sim, tolerance = 1e-6)

})

test_that("FBbridge fails for nonnumeric H ", {
  expect_error(FBbridge("a", 2, 1, N = 5))
})

test_that("FBbridge fails for H not in (0,1)", {
  expect_error(FBbridge(-2, 2, 1, N = 5))
  expect_error(FBbridge(2, 2, 1, N = 5))
})

test_that("FBbridge fails for nonnumeric x_end", {
  expect_error(FBbridge(0.5, "a", 1, N = 5))
})

test_that("FBbridge fails for nonnumeric t_end", {
  expect_error(FBbridge(0.5, 2, "a", N = 5))
})

test_that("FBbridg fails if t_end is negative or 0", {
  expect_error(FBbridge(0.5, 2, -2, N = 5))
  expect_error(FBbridge(0.5, 2, 0, N = 5))
})

test_that("FBbridge fails for nonnumeric x_start", {
  expect_error(FBbridge(0.5, 1, 1, "a", N = 5))
})

test_that("FBbridge fails for nonnumeric N", {
  expect_error(FBbridge(0.5, 1, 1, N = "a"))
})

test_that("FBbridge fails for negative N", {
  expect_error(FBbridge(0.5, 1, 1, N = -2))
})

test_that("FBbridge fails for non integer N", {
  expect_error(FBbridge(0.5, 1, 1, N = 1.5))
})

test_that("FBbridge fails for non logical plot", {
  expect_error(FBbridge(0.5, 1, 1, N = 5, plot = 1))
  expect_error(FBbridge(0.5, 1, 1, N = 5, plot = "TRUE"))
})
