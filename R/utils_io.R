#' Get names for the parameter vector
#'
#' @param x a named object
#'
#' @return a character vector, either the names of `x` or strings of the form
#' `"Var1"`, `"Var2"`, etc.
get_par_names <- function(x){
  par_names <- names(x)
  if(is.null(par_names)){
    par_names <- paste0("Var", (1:length(x)))
  }
  return(par_names)
}
