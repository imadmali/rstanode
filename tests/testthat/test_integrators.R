library(rstanode)
library(deSolve)

context("Test integrators (rk45 and bdf) run and accuracy")

simple_ode <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy = -k * y
    return(list(c(dy = dy)))
  })
}

sho <- function(t, y, p) {
  with(as.list(c(y,p)), {
    dy1 = y2
    dy2 = - y1 - theta * y2
    return(list(c(dy1 = dy1, dy2 = dy2)))
  })
}

test_that("rk45 works for 1 state and 1 parameter", {
  pars <- c("k" = 0.15)
  yini <- c("y" = 10)
  time_steps <- seq(1, 10, by = 0.001)
  truth <- deSolve::ode(y = yini, times = time_steps, func = simple_ode, parms = pars)
  fit <- stan_ode(simple_ode, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "rk45",
                  sampling = FALSE)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
})

test_that("bdf works for 1 state and 1 parameter", {
  pars <- c("k" = 0.15)
  yini <- c("y" = 10)
  time_steps <- seq(1, 10, by = 0.001)
  truth <- deSolve::ode(y = yini, times = time_steps, func = simple_ode, parms = pars, method = "bdf")
  fit <- stan_ode(simple_ode, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "bdf",
                  sampling = FALSE)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
})

test_that("rk45 works for multiple states and 1 parameter", {
  pars <- c("theta" = 0.15)
  yini <- c("y1" = 1, "y2" = 0)
  time_steps <- seq(1, 100, by = 0.001)
  truth <- ode(func = sho, y = yini, times = time_steps, parms = pars)
  fit <- stan_ode(sho, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "rk45",
                  sampling = FALSE)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
  expect_equal(sims[,3], truth[,3], tolerance = 0.01)
})

test_that("bdf works for multiple states and 1 parameter", {
  pars <- c("theta" = 0.15)
  yini <- c("y1" = 1, "y2" = 0)
  time_steps <- seq(1, 100, by = 0.001)
  truth <- ode(func = sho, y = yini, times = time_steps, parms = pars, method = "bdf")
  fit <- stan_ode(sho, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "bdf",
                  sampling = FALSE)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
  expect_equal(sims[,3], truth[,3], tolerance = 0.01)
})
