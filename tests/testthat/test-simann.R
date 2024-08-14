test_that("generating candidate points works", {
  set.seed(123)
  res <- genptry(c(0, 0), 2)
  #expect_equal(res, c(-1.93718545,  1.41221816))
  # If this point generation by fixed seed is not reliable, we might fall back
  # to the following:
  expect_true(is.numeric(res))
  expect_length(res, 2)
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

test_that("bounded multidimensional simulated annealing works", {
  set.seed(123)
  f <- function (v) {
      x <- v["x"]
      y <- v["y"]
      return(((x-1)^2 + 1) * (y + 3)^4)
  }
  res <- simann(par = c(x = 3, y = 4), fn = f,
                lower = c(x = 0, y = 0),
                upper = c(x= 10, y = 10),
                control = list(maxit = 3000,
                               temp = 20,
                               tmax = 1,
                               parscale = 1,
                               fnscale = 0.5,
                               REPORT = 30))
  # The minimal value of f in the square [0, 10] x [0, 10] is
  # equal to 81 at the coordinates c(1, 0)
  expect_equal(res$par, c(x = 1, y = 0), tolerance = 0.05)
  expect_equal(res$value, c(x = 81), tolerance = 0.05)
})

test_that("error messages and other messages work", {
  f <- function (v){
    x <- v["x"]
    y <- v["y"]
    return(((x-1)^2 + 2)*((y-1)^2 + 2))
  }
  # Lower too short
  expect_error(simann(par = c(x = 3, y = 3), fn = f,
                lower = c(x = 0),
                upper = c(x= 3, y = 10),
                control = list(maxit = 100)))
 # Lower to high
 expect_error(simann(par = c(x = 3, y = 3), fn = f,
                     lower = c(x = 3.1, y = 0),
                     upper = c(x= 0, y = 10),
                     control = list(maxit = 100)))
 # Upper too short
 expect_error(simann(par = c(x = 3, y = 3), fn = f,
                     lower = c(x = 0, y = 0),
                     upper = c(x= 3),
                     control = list(maxit = 100)))
 # Upper to low
 expect_error(simann(par = c(x = 3, y = 3), fn = f,
                     lower = c(x = 0, y = 0),
                     upper = c(x= 0, y = 10),
                     control = list(maxit = 100)))
 # Reporting step width is bigger than number of iterations
 expect_message(simann(par = c(x = 3, y = 3), fn = f,
                     lower = c(x = 0, y = 0),
                     upper = c(x = 10, y = 10),
                     control = list(maxit = 100,
                                    REPORT = 200)))
})
test_that("one-sided boundaries work", {
  f <- function (v){
    x <- v["x"]
    y <- v["y"]
    return(((x-1)^2 + 2)*((y-1)^2 + 2))
  }
  res_l <- simann(par = c(x = 3, y = 4), fn = f,
                lower = c(x = 0, y = 0),
                control = list(maxit = 1000,
                               temp = 20))
  res_u <- simann(par = c(x = -3, y = -4), fn = f,
                  upper = c(x = 0, y = 0),
                  control = list(maxit = 3000,
                                 temp = 10))
  # res_l should have its minimal value 4 at (1, 1)
  expect_equal(res_l$par, c(x = 1, y = 1), tolerance = 0.05)
  expect_equal(res_l$value, c(x = 4), tolerance = 0.05)
  # res_u should find its minimal value 9 at (0, 0)
  expect_equal(res_u$par, c(x = 0, y = 0), tolerance = 0.05)
  expect_equal(res_u$value, c(x = 9), tolerance = 0.05)
})
