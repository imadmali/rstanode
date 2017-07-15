functions {
  real[] ode_sys(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    // AUTO GENERATED CODE
    real dydt[1];
    dydt[1]=-theta[1]*y[1];
    return dydt;
  }
}
data {
  int<lower=0,upper=1> sampling;
  int<lower=0> N;
  int<lower=0> K;
  int<lower=1> T;
  real y0[N];
  real t0[1];
  real ts[T];
  real theta[K];
}
transformed data {
  real x[(sampling == 0)? 0 : 1];
  int x_int[(sampling == 0)? 0 : 1];
}
model {}
generated quantities {
  real y_hat[T,N];
  y_hat = integrate_ode_rk45(ode_sys, y0, t0[1], ts, theta, x, x_int);
}