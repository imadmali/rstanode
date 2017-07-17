#' read_stan_file
#' @description Return the path of the desired Stan program
#' @param has_events A logical value that indicates whether or not the model contains events.
#' @param integrator The type of integrator to use. One of either \code{"rk45"} or \code{"bdf"}.
#' @param sampling Logical variable indicating whether the user wants to perform sampling.

read_stan_file <- function(has_events, integrator, sampling) {
  if (has_events == FALSE) {
    if (isTRUE(sampling)) {
      if (integrator == "rk45")
        path <- system.file(package = "rstanode", "ode_wrap_sampling_rk45.stan")
      else
        path <- system.file(package = "rstanode", "ode_wrap_sampling_bdf.stan")
    }
    else {
      if (integrator == "rk45")
        path <- system.file(package = "rstanode", "ode_wrap_rk45.stan")
      else
        path <- system.file(package = "rstanode", "ode_wrap_bdf.stan")
    }
  }
  else {
    if (integrator == "rk45")
      path <- system.file(package = "rstanode", "ode_wrap_events_rk45.stan")
    else
      path <- system.file(package = "rstanode", "ode_wrap_events_bdf.stan")
  }
  return(path)
}
