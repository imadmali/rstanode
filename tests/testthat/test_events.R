library(rstanode)
library(deSolve)

context("Test events run and accuracy (rk45 only)")

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

test_that("Event 'add' works for 1 state ODE", {
  pars <- c("k" = 0.15)
  yini <- c("y" = 10)
  time_steps <- seq(1, 10, by = 0.001)
  events_table <- data.frame(var = "y",
                             time = seq(2,5,by=1),
                             value = 5,
                             method = "add")
  truth <- deSolve::ode(y = yini, times = time_steps, func = f1, parms = pars, events = list(data = events_table))
  events_table_stan <- data.frame("time" = events_table$time,
                                  "y" = 5,
                                  method = "add")
  fit <- stan_ode(f1, state = yini,
                  pars = pars,
                  times = time_steps,
                  events = events_table_stan,
                  integrator = "rk45",
                  sampling = FALSE)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
})

test_that("Event 'add' works for multiple states ODE", {
  pars <- c("theta1" = 0.5)
  yini <- c("y1" = 2, "y2" = 5)
  time_steps <- seq(1,10,by=0.001)
  events_table <- data.frame(var = "y1",
                             time = 2:4,
                             value = 2,
                             method = "add")
  truth <- deSolve::ode(y = yini, times = time_steps, func = f2, parms = pars, events = list(data = events_table))
  events_table_stan <- data.frame(time = events_table$time, y1 = 2, y2 = 0, method = "add")
  fit <- stan_ode(f2, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "rk45",
                  sampling = FALSE, events = events_table_stan)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
  expect_equal(sims[,3], truth[,3], tolerance = 0.01)
})

test_that("Event 'multiply' works for multiple states ODE", {
  pars <- c("theta1" = 0.5)
  yini <- c("y1" = 2, "y2" = 5)
  time_steps <- seq(1,10,by=0.001)
  events_table <- data.frame(var = "y1",
                             time = 2:4,
                             value = 1.5,
                             method = "multiply")
  truth <- deSolve::ode(y = yini, times = time_steps, func = f2, parms = pars, events = list(data = events_table))
  events_table_stan <- data.frame(time = events_table$time, y1 = 1.5, y2 = 1, method = "multiply")
  fit <- stan_ode(f2, state = yini,
                  pars = pars,
                  times = time_steps,
                  integrator = "rk45",
                  sampling = FALSE, events = events_table_stan)
  sims <- fit$simulations
  expect_equal(sims[,2], truth[,2], tolerance = 0.01)
  expect_equal(sims[,3], truth[,3], tolerance = 0.01)
})
