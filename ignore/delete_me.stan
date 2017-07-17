functions {
  real[] ode_sys(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    // AUTO GENERATED CODE
    real dydt[2];
    dydt[1]=y[2];
    dydt[2]=-y[1]-theta[1]*y[2];
    return dydt;
  }
}
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=1> T;
  real y0[N];
  real t0[1];
  real ts[T];
  real y[T,N];
}
transformed data {
  real x[0];
  int x_int[0];
}
parameters {
  real theta[K];
  real<lower=0> sigma;
}
model {
  real y_hat[T,2];
  y_hat = integrate_ode_rk45(ode_sys, y0, t0[1], ts, theta, x, x_int);
  y[,1] ~ normal(y_hat[,1], sigma);
  y[,2] ~ normal(y_hat[,2], sigma);
  theta[1] ~ normal(0, 1);
}
