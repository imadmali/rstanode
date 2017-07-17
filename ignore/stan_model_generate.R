# 
# stan_model_generate <- function(obj, likelihoods, priors) {
#   
#   stan_wrapper <- obj
#   sel <- which(stan_wrapper == "    #include \"user_model.stan\"")
#   upr <- 1:(sel-1)
#   lwr <- (sel+1):length(stan_wrapper)
#   
#   ode_eqns <- c("    // AUTO GENERATED CODE",
#                 paste0("    real dydt[",n_states,"];"),
#                 obj,
#                 "    return dydt;")
#   out <- c(stan_wrapper[upr], ode_eqns, stan_wrapper[lwr])
# }