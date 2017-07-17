remove(list = ls())
library(rstanode)
library(rstan)

sho <- function(t, y, p) {
  with(as.list(c(y,p)), {
    dy1 = y2
    dy2 = - y1 - theta * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
  })
}

pars <- c("theta" = 0.15)
yini <- c("y1" = 1, "y2" = 0)
time_steps <- seq(1, 100, by = 0.001)

sl <- rstanode:::stan_lines(sho, state = yini,
               pars = pars,
               times = time_steps)
stan_prg <- rstanode:::read_stan_file(has_events = FALSE, integrator = "rk45", sampling = TRUE)
stan_prg <- rstanode:::stan_ode_generate(sl$f_out, stan_prg, n_states = 2)


sho_rstan_ode <- stan_ode(sho, state = yini,
                pars = pars,
                times = time_steps,
                integrator = "rk45",
                sampling = FALSE)
sims <- sho_rstan_ode$simulations

sho_rstanode_fit <- stan_ode(sho, state = yini,
                          pars = pars,
                          times = time_steps,
                          integrator = "rk45",
                          sampling = TRUE,
                          y = sims[,-1],
                          likelihoods = list(y1 = normal(NULL, "sigma"),
                                             y2 = normal(NULL, "sigma")),
                          priors = list(theta = cauchy(0,1),
                                        sigma = normal(0,1)), iter = 100, chains = 4)
