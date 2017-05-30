functions {
  real[] sho(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    // AUTO GENERATED CODE
    real dydt[3];
    if(dy[1]dt[2]>=100);
    dy[1]dt[2]=0;
    dydt[3]=theta[1]*dy[1]dt[2]+theta[2];
    return dydt;
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
  real x[(sampling == 0)? 0 : 1];
  int x_int[(sampling == 0)? 0 : 1];
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
