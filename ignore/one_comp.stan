functions {
  real[] pkpd(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    real dy_dt[1];
    dy_dt[1] = -theta[1] * y[1];
    return dy_dt;
  }
}
data {
  int<lower=1> T;
  real y0[1];
  real t0;
  real ts[T];
  real theta[1];
  int<lower=1> D;  // number of dosing periods
  int<lower=1> Dv; // dosing value
}
transformed data {
  real x[0];
  int x_int[0];
}
model {

}
generated quantities {
  real y_hat[T,1,D];
  real new_y0[1];
  for(i in 1:D) {
    if (i == 1)
      new_y0[1] = y0[1] + Dv;               // initial value + dose
    else if (i <= D && i != 1)
      new_y0[1] = y_hat[T, 1, i - 1] + Dv;  // last value of previous dosing period + dose
    if(i != 1)
      print(y_hat[T, 1, i - 1]);
    y_hat[,,i] = integrate_ode_rk45(pkpd, new_y0, t0, ts, theta, x, x_int);
  }

}
