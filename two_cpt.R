two_cpt <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 = -a * y1
    dy2 = a * y1 - b * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
    })
}

y0 <- c("y1" = 0, "y2" = 0)
parameters <- c("a" = 6, "b" = 0.6)
tstep <- seq(1, 10, by = 0.1)
t0 <- 0

events <- data.frame(var = "y1", time = seq(1,10), value = 2, method = "add")

ode_sims <- deSolve::ode(y0, tstep, two_cpt, parms = parameters, events = list(data = events))

sel <- which(tstep %in% events$time)
period <- rep(0, length(tstep))
period[sel[-1]] <- 1:9

stan_data <- list(N = length(y0),
                  K = length(parameters),
                  T = length(tstep),
                  E = nrow(events)-1,
                  y0 = y0,
                  theta = parameters,
                  ts = tstep,
                  t0 = t0,
                  event = cbind(rep(2,nrow(events)),rep(0,nrow(events)))[-1,],
                  period = period)

fit <- rstan::stan("~/desktop/ode_events.stan", data = stan_data, chains = 1, iter = 1, algorithm = "Fixed_param")
sims <- extract(fit, pars = "y_hat")$y_hat[1,,]
plot(sims[,1], type = "l")
plot(sims[,2], type = "l")
