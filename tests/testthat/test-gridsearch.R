test_that("gridsearch returns warning if grid is unnamed", {
  fn1 <- function(x) (-1)*dnorm(x, mean = 3.9)
  expect_warning(gridsearch(fn1,
                            lower = -10,
                            upper = 10,
                            step = 0.5),
                 regexp = NULL)
  expect_equal(gridsearch(fn1,
                          lower = c(x = -10),
                          upper = c(x = 10),
                          step = c(x = 0.5))$par, 4)
})
