############################
### ODE EXAMPLES IN STAN ###
############################

library(rstan)
library(deSolve)

# simple harmonic oscillator example

sim_data <- list(y0 = c(1,0), t0 = 0,
                 ts = seq(0.1,100, by = 0.1), theta = array(0.15,1))
sim_data$T <- length(sim_data$ts)

fit <- stan("sho.stan", data = sim_data,
            chains = 1, iter = 1, algorithm = "Fixed_param")
y <- extract(fit, pars = "y_hat")$y_hat[1,,]

y <- rbind(sim_data$y0, y)
plot(y[,1], y[,2], type = "l", lwd = 2, col = "darkgrey")
points(1, 0, col = "red", pch = 20)

yini <- sim_data$y0
times <- sim_data$ts
theta <- sim_data$theta
sho <- function(t, y, p) {
  dy1 = y[2]
  dy2 = - y[1] - theta * y[2]
  return(list(c(dy1, dy2)))
}
fit_desolve <- ode(func = sho, y = yini, times = times, parms = NULL, method = "bdf")

plot(fit_desolve[,2], fit_desolve[,3], type = "l", lwd = 2, col = "darkgrey", ylab = "dy2", xlab = "dy1")
lines(y[,1], y[,2], type = "l", lwd = 1, col = "red")

# simple harmonic oscillator simulation example (finding parameters/inits conditional on data)

fit2 <- stan("sho_fit.stan", data = sim_data, cores = 4, iter = 500)
print(fit2, digits = 2)
y_fit2 <- extract(fit2)
names(y_fit2)

### really simple one compartment model ###

setwd("/Users/imad/desktop/stan/stan_ode")
remove(list = ls())

one_comp_data <- list(ts = seq(1,2,by=1/24),
                      t0 = 0.999,
                      y0 = array(0,1),
                      theta = array(0.6,1),
                      D = 9,
                      Dv = 40)
one_comp_data$T <- length(one_comp_data$ts)
one_comp <- stan("one_comp.stan", data = one_comp_data,
                 chains = 1, iter = 1, algorithm = "Fixed_param")
y <- extract(one_comp, pars = "y_hat")$y_hat[1,,,]

# this plot is not correct since you're plotting the
plot(c(y), type = "l", lwd = 2, col = "darkgrey",
     ylab = "blood concentration", xlab = "time")

# same model in deSolve

library(deSolve)
yini <- c(y = 0)
times <- seq(1, 10, by = 1/24)
pkpd <- function(t, y, theta) {
  dy <- - theta * y
  list(dy)
}

injectevents <- data.frame(var = "y",
                           time = 1:10,
                           value = 40,
                           method = "add")

pkpd_sim <- ode(func = pkpd, y = yini, times = times, parms = c(theta = 0.6),
                events = list(data = injectevents), method = "ode45")

# pkpd_sim[(seq(1, 10, by = 1/24) * 24) %% 24 == 0,]

plot(pkpd_sim[1:nrow(pkpd_sim),2], ylab = "concentration", xlab = "time", type = 'l', lwd = 2)
lines(1:25,c(y)[1:25], col = "red", lwd = 2)
lines(25:49,c(y)[26:50], col = "red", lwd = 2)
lines(49:73,c(y)[51:75], col = "red", lwd = 2)
lines(73:97,c(y)[76:100], col = "red", lwd = 2)
lines(97:121,c(y)[101:125], col = "red", lwd = 2)
lines(121:145,c(y)[126:150], col = "red", lwd = 2)
lines(145:169,c(y)[151:175], col = "red", lwd = 2)
lines(169:193,c(y)[176:200], col = "red", lwd = 2)
lines(193:217,c(y)[201:225], col = "red", lwd = 2)

plot(pkpd_sim[,2], ylab = "concentration", xlab = "time", type = 'l', lwd = 2)
lines(c(y[-1,]), col = "red", lwd = 2)

plot(pkpd_sim[2:55,2], ylab = "concentration", xlab = "time", type = 'l', lwd = 2)
lines(c(y[2:55]), col = "red", lwd = 2)
abline(v = 25:80, col = "darkgrey", lty = 2)

cbind("deSolve" = pkpd_sim[2:30,2], "Stan" = c(y)[2:30])
cbind("deSolve" = pkpd_sim[,2], "Stan" = c(y[1,1],y[-1,]))

# this is the right transformation of the stan output
plot(pkpd_sim[,2], ylab = "concentration", xlab = "time (hrs each day)", type = 'l', lwd = 2, xaxt = "n")
axis(1, at = 1:length(seq(1,10,by=1/24)), labels = (seq(1,10,by=1/24)*24)%%24, cex.axis = 0.5)
abline(v = which((seq(1, 10, by = 1/24) * 24) %% 24 == 0), lwd = 1, lty = 2, col = "darkgrey")
lines(1:217,c(0,c(y[1,1],y[-1,])[-1]), col = "red", lwd = 2)

# compare correct stan transformation with column-major order transformation
plot(pkpd_sim[,2], ylab = "concentration", xlab = "time (hrs each day)", type = 'l', lwd = 2, xaxt = "n")
axis(1, at = 1:length(seq(1,10,by=1/24)), labels = (seq(1,10,by=1/24)*24)%%24, cex.axis = 0.5)
lines(c(y), col = "darkgrey")
lines(1:217,c(0,c(y[1,1],y[-1,])[-1]), col = "red", lwd = 2)
