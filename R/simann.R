simann <- function(par, fn,
                   lower, upper,
                   control){
  par_now <- par
  fn_now <- fn(par)
  par_opt <- par
  fn_opt <- par_opt
  trace <- data.frame((1:maxit), rep(par, maxit), rep(fn_now, maxit), temp_now)

  for(i in (1:maxit)){
    temp_now <- temp / log(((t-1) %/% tmax)*tmax + exp(1))
    par_candidate <- rtruncnorm(par_now)
    fn_candidate <- fn(par_candidate)
    delta <- fn_candidate - fn_now
    if(delta < 0){
      fn_now <- fn_candidate
      par_now <- par_candidate
      if(fn_candidate < fn_opt){
        fn_opt <- fn_candidate
        par_opt <- par_candidate
      }
    } else if(runif(1, 0, 1) < exp(-delta/(temp_now))){
      fn_now <- fn_candidate
      par_now <- par_candidate
    }
    trace[i,] <- c(i, par_now, fn_now, temp_now)
  }
  return(list(par = par_opt,
              value = fn_opt,
              trace = trace))
}


rtruncnorm_kernel <- function(a, b, mean, sd){
}
