functions {
  real[] sho(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    #include "func.stan"
  }
}
data {
  int<lower=0,upper=1> integrator;
  int<lower=0,upper=1> sampling;
  int<lower=0> N;
  int<lower=0> K;
  int<lower=1> T;
  real y0[N];
  real t0;
  real ts[T];
  real theta[K];
}
transformed data {
  if (sampling == 0) {
    real x[0];
    int x_int[0];
  }
}
model {

}
generated quantities {
  real y_hat[T,N];
  if (integrator == 0)
    y_hat = integrate_ode_rk45(sho, y0, t0, ts, theta, x, x_int);
  else
    y_hat = integrate_ode_bdf(sho, y0, t0, ts, theta, x, x_int);
}
