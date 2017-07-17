
stan_model_generate <- function(prog, likelihoods, priors, new_params, K) {

  stan_wrapper <- prog

  # include parameters
  sel <- which(stan_wrapper == "  #include \"user_parameters.stan\"")
  upr <- 1:(sel-1)
  lwr <- (sel+1):length(stan_wrapper)

  param_dec <- c("  // AUTO GENERATED CODE",
                 "  real theta[K];",
                 new_params)
  stan_wrapper <- c(stan_wrapper[upr], param_dec, stan_wrapper[lwr])

  # include likelihood/priors
  sel <- which(stan_wrapper == "  #include \"user_model.stan\"")
  upr <- 1:(sel-1)
  lwr <- (sel+1):length(stan_wrapper)

  model_eqns <- c("  // AUTO GENERATED CODE",
                 priors,
                 likelihoods)
  out <- c(stan_wrapper[upr], model_eqns, stan_wrapper[lwr])
  return(paste(out, collapse = "\n"))
}

