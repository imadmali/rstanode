functions {
  real[] ode_sys(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    // AUTO GENERATED CODE
    real dydt[4];
    real D1;
    real D2;
    D1=((y[1]+theta[1])^2.0+y[2]^2.0)^(3.0/2.0);
    D2=((y[1]-theta[2])^2.0+y[2]^2.0)^(3.0/2.0);
    dydt[1]=y[3];
    dydt[2]=y[4];
    dydt[3]=y[1]+2.0*y[4]-theta[2]*(y[1]+theta[1])/D1-theta[1]*(y[1]-theta[2])/D2;
    dydt[4]=y[2]-2.0*y[3]-theta[2]*y[2]/D1-theta[1]*y[2]/D2;
    return dydt;
  }
}
data {
  int<lower=0,upper=1> sampling;
  int<lower=0> N;
  int<lower=0> K;
  int<lower=1> T;
  real y0[N];  // y[1];
  real ts[T];  // time[T];
  real theta[K]; // theta[1];
  int n_seg;
  real t0[n_seg];  // t0[n_seg];
  int seg[n_seg];
  int sequence[T];
  int n_events;
  real events[n_events,N];
  real event_type[n_events];
}
transformed data {
  real x[(sampling == 0)? 0 : 1];     // incorrect condition if sampling = TRUE? x_r[0];
  int x_int[(sampling == 0)? 0 : 1];  // incorrect condition if sampling = TRUE? x_i[0];
}
model {}
generated quantities {
  real y_hat[T,N];
  {
    int pos;
    pos = 1;
    for (i in 1:n_seg) {
      int indx[size(segment(sequence, pos, seg[i]))];
      real y_init[N];
      indx = segment(sequence, pos, seg[i]);
      if (i == 1)
        y_init = y0;
      else {
        if (event_type[i-1] == 1)
          y_init = to_array_1d(to_vector(y_hat[pos - 1,]) + to_vector(events[i-1,]));
        else if (event_type[i-1] == 2)
          y_init = to_array_1d(to_vector(y_hat[pos - 1,]) .* to_vector(events[i-1,]));
        else // event_type[i] == 3
          y_init = events[i-1,];
      }
      y_hat[indx,] = integrate_ode_rk45(ode_sys, y_init, t0[i], ts[indx], theta, x, x_int);
      pos = pos + seg[i];
    }
  }
}
