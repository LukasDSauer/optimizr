#' Grid Search Algorithm
#'
#' The grid search algorithm searches the parameter space of a function `fn` for
#' the optimal parameter values on a pre-defined grid.
#'
#' The grid can be defined by either
#' 1. lower and upper bounds of the parameter space together with
#' a step width for each dimension,
#' 2. axis vectors for each dimension of the
#' parameter space, or
#' 3. a "grid" containing every parameter combination to be visited.
#'
#'
#' @inheritParams algorithm
#' @param step A numeric vector of step widths for each dimension of the
#' parameter space.
#' @param axes A list of numeric vectors, defining the axis values of the grid
#' for each dimension of the parameter space.
#' @param grid A data frame of all parameter combinations to be visited.
#'
#' @section Control parameters:
#'
#' The list `control` may contain the following objects:
#' * `fnscale`: A numeric, if it is non-negative or NULL, the algorithm searches
#'    for the minimum, if it is negative, it searches for the maximum.
#' * `REPORT`: An integer, if it is `NA_integer_` or non-positive, no trace is
#'    reported. By default, a trace is reported.
#'
#' @inherit algorithm return
#' @export
#'
#' @examples
#' fn <- function(x, y) (-1)*dnorm(x, mean = 3.9)*dnorm(y, mean = 3.02)
#' # Define grid using lower and upper bounds and step widths
#' gridsearch(fn,
#'            lower = c(x = -10, y = -10),
#'            upper = c(x = 10, y = 10),
#'            step = c(x = 0.5, y = 0.5))
#' # Define grid using axes
#' gridsearch(fn,
#'            axes = list(x = (-10:10), y = (-10:10)))
#' # Custom grid, e.g. only the diagonal
#' grid <- data.frame(x = (-10:10), y = (-10:10))
#' gridsearch(fn,
#'            grid = grid)
gridsearch <- function(fn,
                       lower = NULL, upper = NULL, step = NULL,
                       axes = mapply(seq, from = lower, to = upper, by = step,
                                     SIMPLIFY = FALSE),
                       grid = expand.grid(axes),
                       control = NULL){
  # Default control values
  fnscale <- 1
  trace_rep <- TRUE
  # Custom control values
  if(!is.null(control$fnscale)) fnscale <- control$fnscale
  if(!is.null(control$REPORT)) trace_rep <- !is.na(REPORT) & (REPORT > 0)
  if(is.null(grid)){
    stop("grid cannot be NULL. You need to supply either lower and upper
    parameter space boundaries together with step widths, or parameter space
    grid axes, or a complete grid to be searched.")
  }
  # Calculate utility function on all grid values
  grid_l <- as.list(grid)

  y <- tryCatch({do.call(fn, grid_l)},
                error = function(e){
                  warning(paste0("An error occured when trying to apply fn to",
                                 " the grid. Original error message:\n", e,
                                 "\nTrying to remove the grid's names and",
                                 " reapplying the function. This may lead to",
                                 " unwanted results. It is recommended to name",
                                 " the grid dimensions",
                                 " like the function arguments."))
                  names(grid_l) <- NULL
                  return(do.call(fn, grid_l))
                }
                )
  # Find index of minimal or maximal value, respectively
  if(fnscale >= 0){
    iopt <- which.min(y)
  } else if(fnscale < 0){
    iopt <- which.max(y)
  }
  # Get optimal values
  yopt <- y[iopt]
  popt <- unlist(grid[iopt, ])
  res <- list(par = popt,
              value = yopt)
  # Save trace and name its columns
  if(trace_rep){
    par_names <- get_par_names(popt)
    fn_name <- get_fn_name(yopt)
    trace <- cbind(grid, y)
    names(trace) <- c(par_names, fn_name)
    res <- c(res, trace = list(trace))

  }
  # Return output
  return(res)
}
