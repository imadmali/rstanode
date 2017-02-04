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

f <- function(y, t, p) {
  dy1 = y2
  dy2 = -y1 - theta1 * y2
}

clean_f <- function(x) {
  sep <- deparse(f)
  beginning <- which(deparse(f) == "{") + 1
  end <- which(deparse(f) == "}") - 1
  out <- sep[beginning:end]
  out <- gsub(" ", "", out)
  for(i in 1:length(out)) {
    out[i] <- paste0(out[i], ";")
  }
  out
}


trans_f <- function(x, n_theta, n_y) {
  out <- x
  for(j in 1:length(x)) {
    for (i in 1:n_y) {
      out[j] <- gsub(paste0("y",i), paste0("y[",i,"]"), out[j]) 
      out[j] <- gsub(paste0("dy\\[",i,"\\]"), paste0("dy_dt[",i,"]"), out[j])
    } 
    for (k in 1:n_theta) {
      out[j] <- gsub(paste0("theta[",k,"]"), paste0("theta[",k,"]"), out[j])
    }
  }
  out
}

# write function to Stan include
write_to_stan <- function(x) {
  n_eq <- length(x)
  out <- c(paste0("real dy_dt[",n_eq,"];"),
           x,
           "return dy_dt;")
  writeLines(out, "user_ode_func.stan") 
}

clean_f(f)
trans_f(clean_f(f), 2, 2)
write_to_stan(trans_f(clean_f(f), 2, 2))

ode_model <- stan_model(stanc_ret = stanc_builder("sho.stan"))
del <- sampling(ode_model, data = sim_data, chains = 1, iter = 1, algorithm = "Fixed_param")
del <- extract(del)
