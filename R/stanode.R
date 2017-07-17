# stanode
# @description Create the output for the user after simulating/fitting the Stan program..
# @param state_nms State names given by the user.
# @return A list that contains information on the simulation/fitted Stan program.
# 

stanode <- function(obj) {
  sims <- rstan::extract(obj, pars = "y_hat")
  sims <- sims$y_hat[1,,]
  out <- list(simulations = sims,
              stanfit = obj)
  return(out)
}