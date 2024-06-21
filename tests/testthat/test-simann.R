test_that("generating candidate points works", {
  set.seed(123)
  res <- genptry(c(0, 0), 2)
  expect_equal(res, c(-1.93718545,  1.41221816))
})
test_that("coordinate transformation and its inverse work", {
  expect_equal(phiinv(phi(7, 2, 3), 2, 3), 7)
})
