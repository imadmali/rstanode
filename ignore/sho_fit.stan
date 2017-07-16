functions {
  real[] sho(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    real dy_dt[2];
    dy_dt[1] = y[2];
    dy_dt[2] = -y[1] - theta[1] * y[2];
    return dy_dt;
  }
}
data {
  int<lower=1> T;
  real y[T+1,2];
  real y0[2];
  real t0;
  real ts[T];
}
transformed data {
  real x[0];
  int x_int[0];
}
parameters {
  real theta[1];
  real<lower=0> sigma;
}
model {
  real y_hat[T,2];
  y_hat = integrate_ode_bdf(sho, y0, t0, ts, theta, x, x_int);

  for (t in 1:T)
    y[t] ~ normal(y_hat[t], sigma);

  sigma ~ normal(0, 1);
  theta ~ normal(0, 1);
}
