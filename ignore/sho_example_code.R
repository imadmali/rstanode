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
cat(rstanode:::stan_ode_generate(sl, integrator = "rk45", has_events = FALSE, n_states = 2, sampling = FALSE))

sho_rstan_ode <- stan_ode(sho, state = yini,
                pars = pars,
                times = time_steps,
                integrator = "rk45",
                sampling = FALSE)
sims <- sho_rstan_ode$simulations

stan_data <- list(N = length(yini),
                  y0 = yini,
                  t0 = array(1-1e-6,1),
                  ts = time_steps[1:500],
                  K = length(pars),
                  theta = array(pars,length(pars)),
                  y = sims[1:500,-1])
stan_data$T <- nrow(stan_data$y)

fit <- stan("ignore/delete_me.stan", data = stan_data, chains = 4, iter = 2e3, cores = 2)
print(fit)
traceplot(fit)
