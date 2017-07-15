library(stanode)
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
  truth <- c("    dydt[1]=-theta[1]*y[1];",
             "    dydt[2]=theta[1]*y[1]-theta[2]*y[2];")
  expect_equivalent(sl, truth)
})
