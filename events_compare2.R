remove(list = ls())

library(rstan)
library(deSolve)
library(stanode)

rstan_options(auto_write = TRUE)

simple_ode <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 = -a * y1
    dy2 = a * y1 - b * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
  })
}

# IN DESOLVE

pars <- c("a" = 6, "b" = 0.6)
yini <- c("y1" = 0, "y2" = 0)
time_steps <- seq(1, 5, by = 0.01)
dosing_table <- data.frame(var = "y1",
                           time = 2:4,
                           value = 2,
                           method = "add")
deSolve_simple_ode <- deSolve::ode(y = yini, times = time_steps, func = simple_ode, parms = pars, events = list(data = dosing_table))
plot(deSolve_simple_ode)


# MANUALLY IN STANODE
dosing_table_stan <- data.frame(time = dosing_table$time, y1 = 2, y2 = 0, method = "add")
sl <- stan_lines(simple_ode, state = yini,
                 pars = pars,
                 times = time_steps)
stan_mod <- stan_ode_generate(sl, has_events = TRUE)
N <- length(yini)
K <- length(pars)
stan_data <- list(integrator = 0,
                  sampling = 0,
                  N = N,
                  K = K,
                  T = length(time_steps),
                  y0 = array(unname(yini),N),
                  theta = array(unname(pars),K),
                  ts = time_steps,
                  t0 = c(time_steps[1], dosing_table_stan$time) - 1e-16)
stan_data$sequence <- 1:length(time_steps)
stan_data$n_seg <- length(stan_data$t0)
stan_data$n_events <- nrow(dosing_table_stan)
stan_data$events <- as.array(as.matrix(dosing_table_stan[, names(yini)]), dim = c(stan_data$n_events, N))

for (i in 1:length(dosing_table_stan$time)) {
  if (i == 1)
    stan_data$seg <- append(stan_data$seg,
                            length(which(stan_data$ts < dosing_table_stan$time[1])))
  else
    stan_data$seg <- append(stan_data$seg,
                            length(which(stan_data$ts < dosing_table_stan$time[i] & stan_data$ts >= dosing_table_stan$time[i-1])))
}
stan_data$seg <- append(stan_data$seg, length(which(stan_data$ts >= dosing_table_stan$time[length(dosing_table_stan$time)])))

# as.array(as.matrix(dosing_table_stan[, c("time", "y")]), dim = c(2, 2))
compiled_model <- stan_model(model_code = stan_mod)
fit <- sampling(compiled_model, data = stan_data,
                algorithm = "Fixed_param", chains = 1, iter = 1)
fit <- stan(model_code = stan_mod, data = stan_data,
                   algorithm = "Fixed_param", chains = 1, iter = 1)

sims <- stanode(fit)
plot(stan_data$ts, deSolve_simple_ode[,"y1"], type = "l")
lines(stan_data$ts, sims$simulations[,1], col = "red")

plot(stan_data$ts, deSolve_simple_ode[,"y2"], type = "l")
lines(stan_data$ts, sims$simulations[,2], col = "red")
