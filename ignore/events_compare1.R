remove(list = ls())
simple_ode <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy = -k * y
    return(list(c(dy = dy)))
  })
}

# IN DESOLVE

pars <- c("k" = 0.15)
yini <- c("y" = 10)
time_steps <- seq(1, 100, by = 0.01)
dosing_table <- data.frame(var = "y",
                           time = c(40,80),
                           value = 5,
                           method = "add")
deSolve_simple_ode <- deSolve::ode(y = yini, times = time_steps, func = simple_ode, parms = pars, events = list(data = dosing_table))
plot(deSolve_simple_ode)

# MANUALLY IN STANODE
dosing_table_stan <- data.frame(time = c(40, 80), y = 5, method = "add")
sl <- stan_lines(simple_ode, state = yini,
                 pars = pars,
                 times = time_steps)
stan_model <- stan_ode_generate(sl, has_events = TRUE)
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
                  t0 = c(time_steps[1], dosing_table_stan$time) - 1e-6)
stan_data$sequence <- 1:length(time_steps)
stan_data$n_seg <- length(stan_data$t0)
stan_data$n_events <- nrow(dosing_table_stan)
stan_data$events <- as.array(as.matrix(dosing_table_stan[, names(yini)]), dim = c(stan_data$n_events, 1))

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

fit <- rstan::stan(model_code = stan_model, data = stan_data,
                   algorithm = "Fixed_param", chains = 1, iter = 1)

sims <- stanode(fit)

lines(stan_data$ts, sims$simulations, col = "red")
