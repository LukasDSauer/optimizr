test_that("grid search finds minimum", {
  fn1 <- function(x) (-1)*dnorm(x, mean = 3.9)
  expect_equal(gridsearch(fn1,
                          lower = c(x = -10),
                          upper = c(x = 10),
                          step = c(x = 0.5))$par, 4)
})
