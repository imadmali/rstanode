# simple harmonic oscillator example
library(rstan)
sim_data <- list(T = 991, y0 = c(1,0), t0 = 0,
                 ts = seq(1,100, by = 0.1), theta = array(0.15,1))
# stanc('sho.stan')

fit <- stan("sho.stan", data = sim_data,
            chains = 1, iter = 1, algorithm = "Fixed_param")
y <- extract(fit, pars = "y_hat")$y_hat[1,,]

y <- rbind(sim_data$y0, y)
plot(y[,1], y[,2], type = "l", lwd = 2, col = "darkgrey")
points(1, 0, col = "red", pch = 20)


fit2 <- stan("sho_fit.stan", data = sim_data, cores = 4, iter = 500)
print(fit2, digits = 2)
y_fit2 <- extract(fit2)
names(y_fit2)
