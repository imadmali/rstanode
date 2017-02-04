functions {
  real[] sho(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    #include "fun.stan"
  }
}
data {
  int<lower=1> T;
  // int<lower=0> K;
  real y0[2];
  real t0;
  real ts[T];
  real theta[1];
}
transformed data {
  real x[0];
  int x_int[0];
}
model {

}
generated quantities {
  real y_hat[T,2];
  y_hat = integrate_ode_bdf(sho, y0, t0, ts, theta, x, x_int);
}
