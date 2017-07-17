library(rstanode)
library(deSolve)

context("Test the components that constitute the parser (R to Stan)")

f1 <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy = -k * y
    return(list(c(dy = dy)))
  })
}

f2 <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- y2
    dy2 <- -y1 - theta1 * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
  })
}

f4 <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 = -a * y1
    dy2 = a * y1 - b * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
  })
}

f5 <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    dy_gut = -ka * y_gut
    dy_cent = ka * y_gut - (CL/V_cent + Q/V_cent) * y_cent + (Q/V_peri) * y_peri
    dy_peri = (Q/V_cent) * y_cent - (Q/V_peri) * y_peri
    return(list(c(dy_gut=dy_gut, dy_cent=dy_cent, dy_peri=dy_peri)))
  })
}

test_that("stan_lines works for 1 state and 1 parameter", {
  pars <- c("k" = 0.15)
  yini <- c("y" = 10)
  time_steps <- seq(1, 10, by = 0.001)
  sl <- stan_lines(f1, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- "    dydt[1]=-theta[1]*y[1];"
  expect_equivalent(sl, truth)
})

test_that("stan_lines works for multiple states and 1 parameter", {
  pars <- c("theta1" = 0.5)
  yini <- c("y1" = 2, "y2" = 5)
  time_steps <- seq(1,10,by=0.001)
  sl <- stan_lines(f2, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    dydt[1]=y[2];", "    dydt[2]=-y[1]-theta[1]*y[2];")
  expect_equivalent(sl, truth)
})

# test_that("stan_lines works for 1 state and multiple parameters", {})

test_that("stan_lines works for multiple states and multiple parameters", {
  pars <- c("a" = 6, "b" = 0.6)
  yini <- c("y1" = 0, "y2" = 0)
  time_steps <- seq(1, 5, by = 0.001)
  sl <- stan_lines(f4, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    dydt[1]=-theta[1]*y[1];", "    dydt[2]=theta[1]*y[1]-theta[2]*y[2];")
  expect_equivalent(sl, truth)
})

test_that("stan_lines works for a complicated ODE system", {
  pars <- c("CL" = 10, Q = 13, "V_cent" = 20, "V_peri" = 73, ka = 3)
  yini <- c("y_gut" = 0, "y_cent" = 0, "y_peri" = 0)
  time_steps <- seq(0, 150, by = 0.01)
  sl <- stan_lines(f5, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    dydt[1]=-theta[5]*y[1];",
             "    dydt[2]=theta[5]*y[1]-(theta[1]/theta[3]+theta[2]/theta[3])*y[2]+(theta[2]/theta[4])*y[3];",
             "    dydt[3]=(theta[2]/theta[3])*y[2]-(theta[2]/theta[4])*y[3];")
  expect_equivalent(sl, truth)
})

test_that("stan_lines works when return in ODE function is not named", {
  f4 <- function(t, y, parms) {
    with(as.list(c(y,parms)), {
      dy1 = -a * y1
      dy2 = a * y1 - b * y2
      return(c(dy1, dy2))
    })
  }
  pars <- c("a" = 6, "b" = 0.6)
  yini <- c("y1" = 0, "y2" = 0)
  time_steps <- seq(1, 5, by = 0.001)
  sl <- stan_lines(f4, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    dydt[1]=-theta[1]*y[1];",
             "    dydt[2]=theta[1]*y[1]-theta[2]*y[2];")
  expect_equivalent(sl, truth)
})

test_that("stan_lines parses assignment operator within ODE", {
  f4 <- function(t, y, parms) {
    with(as.list(c(y,parms)), {
      dy1 <- -a * y1
      dy2 <- a * y1 - b * y2
      return(c(dy1, dy2))
    })
  }
  pars <- c("a" = 6, "b" = 0.6)
  yini <- c("y1" = 0, "y2" = 0)
  time_steps <- seq(1, 5, by = 0.001)
  sl <- stan_lines(f4, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    dydt[1]=-theta[1]*y[1];",
             "    dydt[2]=theta[1]*y[1]-theta[2]*y[2];")
  expect_equivalent(sl, truth)
})

test_that("stan_lines parses user defined functions within ODE", {
  Arenstorf <- function(t, y, p) {
    with(as.list(c(y,p)), {
      D1 <- ((y[1] + mu1)^2 + y[2]^2)^(3/2)
      D2 <- ((y[1] - mu2)^2 + y[2]^2)^(3/2)
      dy1 <- y[3]
      dy2 <- y[4]
      dy3 <- y[1] + 2*y[4] - mu2*(y[1]+mu1)/D1 - mu1*(y[1]-mu2)/D2
      dy4 <- y[2] - 2*y[3] - mu2*y[2]/D1 - mu1*y[2]/D2
      return(list( c(dy1, dy2, dy3, dy4) ))
    })
  }

  mu1 <- 0.012277471
  pars <- c(mu1 = mu1, mu2 = 1 - mu1)
  yini <- c(y1 = 0.994, y2 = 0,
            y3 = 0, y4 = -2.00158510637908252240537862224)
  time_steps <- seq(from = 0, to = 18, by = 0.01)
  sl <- stan_lines(Arenstorf, state = yini,
                   pars = pars,
                   times = time_steps)
  sl <- sl$f_out
  truth <- c("    real D1;",
             "    real D2;",
             "    D1=((y[1]+theta[1])^2.0+y[2]^2.0)^(3.0/2.0);",
             "    D2=((y[1]-theta[2])^2.0+y[2]^2.0)^(3.0/2.0);",
             "    dydt[1]=y[3];",
             "    dydt[2]=y[4];",
             "    dydt[3]=y[1]+2.0*y[4]-theta[2]*(y[1]+theta[1])/D1-theta[1]*(y[1]-theta[2])/D2;",
             "    dydt[4]=y[2]-2.0*y[3]-theta[2]*y[2]/D1-theta[1]*y[2]/D2;")
  expect_equivalent(sl, truth)
})
