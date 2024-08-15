#' Optimization algorithm
#'
#' @name algorithm
#' @rdname algorithm
#' @param fn The function to be optimized, taking a numeric vector of parameters
#' and returning a numeric value.
#' @param lower A numeric vector, lower boundary of the parameter vector.
#' @param upper A numeric vector, upper boundary of the parameter vector.
#' @param control A list of further control parameters for the algorithm.
#'
#' @return A list containing `par`, the parameter combination of the optimum,
#' `value`, the optimal function value, and `trace`, a `data.frame` of parameter
#' values visited during the algorithm's run.
NULL
