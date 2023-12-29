#' Get names for the parameter vector
#'
#' @param x a named object
#'
#' @return a character vector, either the names of `x` or strings of the form
#' `"par1"`, `"par2"`, etc.
get_par_names <- function(x){
  par_names <- names(x)
  if(is.null(par_names)){
    par_names <- paste0("par", (1:length(x)))
  }
}
#' Get a name for the function value
#'
#' @inheritParams get_par_names
#'
#' @return a character, either name of `x` or `"fn"`
get_fn_name <- function(x){
  fn_name <- names(x)
  if(is.null(fn_name)){
    fn_name <- "fn"
  }
}
