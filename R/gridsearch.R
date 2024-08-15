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
#' * `REPORT`: An integer, if it is `NA_integer_` or negative, no trace is
#'    reported. If `>=0`, a trace is reported. If `>0`, status updates are sent
#'    to a `progressr` handler every step. By default, `REPORT = 0`,
#' * `use_future`: A logical, if `TRUE` the grid is searched using
#'   `future.apply::future_apply`, if `FALSE`, it is searched using apply.
#'    By default,
#'   `use_future = TRUE`. Note that for actually using parallelization, you still
#'   need to `future::plan()` the session.
#'
#' @inherit algorithm return
#' @importFrom future.apply future_apply
#' @export
#'
#' @examples
#' fn <- function(vec){
#'   return((-1)*dnorm(vec[["x"]], mean = 3.9)*dnorm(vec[["y"]], mean = 3.02))
#' }
#'
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
#' # Diagnostics with progress bar
#' # Attention: Progress bar impedes performance!
#' \dontrun{
#' progressr::handlers(global = TRUE)
#' fn <- function(vec){
#'   Sys.sleep(0.001)
#'   return(c(result =
#'   (-1)*dnorm(vec[["x"]], mean = 3.9)*dnorm(vec[["y"]], mean = 3.02)))
#' }
#' gridsearch(fn,
#'            lower = c(x = -10, y = -10),
#'            upper = c(x = 10, y = 10),
#'            step = c(x = 0.5, y = 0.5),
#'            control = list(REPORT = 1))
#' }
#' # Parallelized grid search using doFuture
#' library(doFuture)
#' plan(multisession)
#' gridsearch(fn,
#'            lower = c(x = -10, y = -10),
#'            upper = c(x = 10, y = 10),
#'            step = c(x = 0.5, y = 0.5))
#' \dontshow{
#' ## R CMD check: make sure any open connections are closed afterward
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
gridsearch <- function(fn,
                       lower = NULL, upper = NULL, step = NULL,
                       axes = mapply(seq, from = lower, to = upper, by = step,
                                     SIMPLIFY = FALSE),
                       grid = expand.grid(axes),
                       control = NULL){
  # Default control values
  fnscale <- 1
  trace_rep <- TRUE
  REPORT <- 0
  use_future <- TRUE
  # Custom control values
  if(!is.null(control$fnscale)) fnscale <- control$fnscale
  if(!is.null(control$REPORT)){
    REPORT <- control$REPORT
    trace_rep <- !is.na(REPORT) & (REPORT >= 0)
  }
  if(!is.null(control$use_future)) use_future <- control$use_future
  if(is.null(grid) | nrow(grid) == 0 | ncol(grid) == 0){
    stop("grid cannot be NULL or have 0 columns or rows. You need to supply
    either lower and upper parameter space boundaries together with step widths,
    or parameter space grid axes, or a complete grid to be searched.")
  }
  # Calculate utility function on all grid values
  if(trace_rep){
    pb <- progressr::progressor(steps = nrow(grid),
                                label = "Grid search",
                                message = "Running grid search")
    pb("Running grid search", class = "sticky", amount = 0)
    fn_report <- function(x){
      pb()
      return(fn(x))
    }
    if(use_future){
      y <- future.apply::future_apply(X = grid, MARGIN = 1, FUN = fn,
                                      future.seed = TRUE)
    } else {
      message("Use of doFuture is switched off.")
      y <- apply(X = grid, MARGIN = 1, FUN = fn_report)
    }

  } else{
    if(use_future){
      y <- future.apply::future_apply(X = grid, MARGIN = 1, FUN = fn,
                                      future.seed = TRUE)
    } else {
      message("Use of doFuture is switched off.")
      y <- apply(X = grid, MARGIN = 1, FUN = fn)
    }
  }
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
    trace <- cbind(grid, y)
    names(trace) <- c(par_names, "fn")
    res <- c(res, trace = list(trace))

  }
  # Return output
  return(res)
}
