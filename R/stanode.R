#' stanode
#' @description Create the output for the user after simulating/fitting the Stan program..
#' @param obj Stanfit object.
#' @param sampling Logical whether sampling is taking place.
#' @return A list that contains information on the simulation/fitted Stan program.
#'

stanode <- function(obj, sampling) {
  if (isTRUE(sampling)) {
    out <- list(stanfit = obj)
  }
  else {
    sims <- rstan::extract(obj, pars = "y_hat")
    sims <- sims$y_hat[1,,]
    out <- list(simulations = sims,
                stanfit = obj)
  }
  return(out)
}
