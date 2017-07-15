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
