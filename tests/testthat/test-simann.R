test_that("generating candidate points works", {
  set.seed(123)
  res <- genptry(c(0, 0), 2)
  expect_equal(res, c(-1.93718545,  1.41221816))
})
test_that("coordinate transformation and its inverse work", {
  expect_equal(phiinv(phi(7, 2, 3), 2, 3), 7)
})
test_that("reporting trace can be switched off", {
  set.seed(123)
  f <- function (x) x^2
  res <- simann(par = 50, fn = f,
                control = list(maxit = 100,
                               temp = 20,
                               parscale = 1,
                               REPORT = NA_real_))
  expect_null(res$trace)
})
