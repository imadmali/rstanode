real dy_dt[2];
dy_dt[1]=y[2];
dy_dt[2]=-y[1]-theta[1]*y[2];
return dy_dt;
