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

  // add measurement error
  # for(t in 1:T) {
  #   y_hat[t,1] = y_hat[t,1] + normal_rng(0, 0.1);
  #   y_hat[t,2] = y_hat[t,2] + normal_rng(0, 0.1);
  # }
}
