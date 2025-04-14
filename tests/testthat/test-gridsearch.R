fn1 <- function(x) (-1)*dnorm(x, mean = 3.9)
test_that("grid search finds minimum (using future_apply_options)", {
  expect_equal(gridsearch(fn1,
                          lower = c(x = -10),
                          upper = c(x = 10),
                          step = c(x = 0.5),
                          control = list(use_future = TRUE,
                                         future_apply_options =
                                           list(future.seed = 123)))$par, 4)
})
test_that("trace can be switched off", {
  res <- gridsearch(fn1,
                    lower = c(x = -10),
                    upper = c(x = 10),
                    step = c(x = 0.5),
                    control = list(REPORT = NA_real_))
  expect_null(res$trace)
})
test_that("trace and future_apply can be switched off", {
  res <- gridsearch(function(x) sin(x),
                    lower = c(x = 0),
                    upper = c(x = pi),
                    step = c(x = 0.01),
                    control = list(fnscale = -1,
                                   REPORT = -1,
                                   use_future = FALSE))
  expect_null(res$trace)
  expect_equal(res$par, pi/2, tolerance = 0.01)
  expect_equal(res$value, 1, tolerance = 1e-6)
})
test_that("grid search finds maximum of multidimensional function",
          {
  f <- function (v) {
    x <- v["x"]
    y <- v["y"]
    return((-1)*(((x-1)^2 + 1) * (y + 3)^4))
  }
  res <- gridsearch(f,
                    lower = c(x = 0, y = 0),
                    upper = c(x = 10, y = 10),
                    step =  c(x = 0.5, y = 0.5),
                    control = list(fnscale = -1,
                                   use_future = FALSE))
  # The maximal value of f in the square [0, 10] x [0, 10] is
  # equal to -81 at the coordinates c(1, 0)
  expect_equal(res$par, c(x = 1, y = 0))
  expect_equal(res$value, -81)
})

test_that("grid search returns error message when grid is not defined", {
  expect_error(gridsearch(fn1,
                          grid = NULL))
  expect_error(gridsearch(fn1,
                          grid = data.frame(x = numeric())))
})
