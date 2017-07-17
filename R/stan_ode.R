#' Simulating/Fitting ODE Systems via Stan
#' @description Ordinary differential equation (ODE) simulation and parameter estimation.
#' The behavior is similar to the \code{\link[deSolve]{ode}} function in the \pkg{deSolve} package.
#' Particularly, the user may use the same ODE function, inital state values, parameter values
#' used in \code{ode}.
#' @param func A function (currently \code{if-else} statements are not supported).
#' @param state A named vector of the initial conditions of the state variables.
#' @param pars A named vector of the parameter values.
#' @param times A sequence of time steps.
#' @param t0 Initial time. The default (\code{NULL}) is \code{t0 = times[1] - 1e-6}
#' @param sampling A logical declaration as to whether you want sample the parameters or
#' initial state values. The default is \code{FALSE}.
#' @param integrator The type of integrator to use.
#' @param events A dataframe that defines the event schedule in the ODE system.
#' See details below.
#' @param ... Optional parameters for \code{\link[rstan]{stan}}.
#' @return A list that contains the simulations and the \code{stanfit} object.
#'
#' @details Currently the user cannot use the event dataframe used in \code{\link[deSolve]{ode}}.
#' The dimensions of the events dataframe equals [number of events] by [number of state variables
#' + 2]. There must be a column denoting the \strong{time} of each event, the \strong{method} to
#' apply to the state variables, and the \strong{value of each state} variable that the user
#' wants to apply to the last simulated value using the aforementioned method.
#'
#' Some examples of event schedules are provided below.
#' The event schedule below is adding 5 to the last simulated state variable y1 in all events
#' accept for the one at taking place time = 40 where 5 is being added to y1 \emph{and} 2 is
#' being added to y2.
#' \tabular{ccccc}{
#' time \tab y1 \tab y2 \tab ... \tab method\cr
#' 10 \tab 5 \tab 0 \tab ... \tab add\cr
#' 20 \tab 5 \tab 0 \tab ... \tab add\cr
#' 30 \tab 5 \tab 0 \tab ... \tab add\cr
#' 40 \tab 5 \tab 2 \tab ... \tab add
#' }
#' The event schedule below is multiplying 1.5 with the last simulated value of y1 and leaving y2
#' unchanged.
#' \tabular{ccccc}{
#' time \tab y1 \tab y2 \tab ... \tab method\cr
#' 10 \tab 1.5 \tab 1 \tab ... \tab multiply\cr
#' 20 \tab 1.5 \tab 1 \tab ... \tab multiply\cr
#' 30 \tab 1.5 \tab 1 \tab ... \tab multiply
#' }
#' @seealso \code{\link[deSolve]{ode}}, \code{\link[rstan]{stan}}.
#' @examples
#' \dontrun{
#' # Simple ODE
#' f <- function(y, t, p) {
#'   dy1 <- y2
#'   dy2 <- -y1 - theta1 * y2
#'   return(dy1 = dy1, dy2 = dy2)
#' }
#' fit <- stan_ode(f, state = c("y1" = 2, "y2" = 5),
#'                 pars = c("theta1" = 0.5),
#'                 times = seq(1,10,by=0.01), t0 = 0,
#'                 integrator = "bdf",
#'                 sampling = FALSE)
#' sims <- extract(fit, pars = "y_hat")
#' sims <- unname(unlist(sims))
#'
#' # Simple Harmonic Oscillator
#' sho <- function(y, p ,t) {
#'   dy1 = y2
#'   dy2 = -y1 - theta * y2
#'   return(list(dy1, dy2))
#' }
#'
#' fit <- stan_ode(sho, state = c("y1" = 1, "y2" = 0),
#'                 pars = c("theta" = 0.15),
#'                 times = seq(1,50,by=0.1), t0 = 0,
#'                 integrator = "bdf",
#'                 sampling = FALSE)
#'
#' sims <- extract(fit, pars = "y_hat")$y_hat[1,,]
#' plot(sims[,1], sims[,2], type = "l", lwd = 2,
#'      xlab = "y1", ylab = "y2", main = "Simple Harmonic Oscillator")
#'}
#'
#' @export
#' @import Rcpp methods

stan_ode <- function(func, state, pars, times, t0 = NULL,
                    integrator = c("rk45", "bdf"), events = NULL,
                    sampling = FALSE, y = NULL, likelihoods = NULL, priors = NULL, ...) {

  integrator <- match.arg(integrator)

  # deal with start times
  t0_accuracy <- 1e-6
  if (is.null(t0)) {
    if (is.null(events))
      t0 <- times[1] - t0_accuracy
    else
      t0 <- c(times[1], events$time) - t0_accuracy
  }

  # create stan program
  stan_lines_stuff <- stan_lines(func, state, pars, times)
  stan_ode_eqns <- stan_lines_stuff$f_out
  stan_path <- read_stan_file(has_events = ifelse(is.null(events), FALSE, TRUE),
                              integrator = integrator,
                              sampling)
  stan_prog <- stan_ode_generate(stan_ode_eqns,
                                 stan_path,
                                 n_states = length(state))
  if (isTRUE(sampling)) {
    ll_nms <- names(likelihoods)
    prior_nms <- names(priors)
    likelihood_eqns <- sapply(1:length(ll_nms), function(n){create_likelihood(likelihoods[[n]], ll_nms[n])})
    prior_eqns <- sapply(1:length(prior_nms), function(n){create_prior(priors[[n]], prior_nms[n], stan_lines_stuff$map)})
    new_pars <- get_new_pars(names(pars), names(priors))
    param_dec <- sapply(1:length(new_pars), function(n){create_new_pars(new_pars[n], likelihoods)})
    stan_prog <- stan_model_generate(stan_prog, likelihood_eqns, prior_eqns, param_dec)
  }
  stan_prog <- paste(stan_prog, collapse = "\n")
  # create stan data
  N = length(state)
  K = length(pars)
  stan_data <- list(integrator = ifelse(integrator == "rk45", 0, 1),
                    sampling = ifelse(isTRUE(sampling), 1, 0),
                    N = N,
                    K = K,
                    T = length(times),
                    y0 = array(unname(state),N),
                    theta = array(unname(pars),K),
                    ts = array(times, length(times)),
                    t0 = array(t0, length(t0)))

  # include event data if applicable
  if (!is.null(events)) {
    stan_data$sequence <- 1:stan_data$T
    stan_data$n_seg <- length(stan_data$t0)
    stan_data$n_events <- nrow(events)
    stan_data$events <- as.array(as.matrix(events[, names(state)]), dim = c(stan_data$n_events, N))
    for (i in 1:length(events$time)) {
      if (i == 1)
        stan_data$seg <- append(stan_data$seg,
                                length(which(stan_data$ts < events$time[1])))
      else
        stan_data$seg <- append(stan_data$seg,
                                length(which(stan_data$ts < events$time[i] & stan_data$ts >= events$time[i-1])))
    }
    stan_data$seg <- append(stan_data$seg, length(which(stan_data$ts >= events$time[length(events$time)])))
    stan_data$event_type <- sapply(events$method, function(x){if(x=="add"){1} else if(x=="multiply"){2} else {3}})
    if (any(stan_data$event_type == 3))
      stop("Currently 'replace' events are not supported.")
  }
  if (isTRUE(sampling)) {
    stan_data$y <- y
  }

  # simulate/fit from stan model
  if (sampling == FALSE)
    fit <- rstan::stan(model_code = stan_prog, data = stan_data,
                       algorithm = "Fixed_param", chains = 1, iter = 1, ...)
  else
    fit <- rstan::stan(model_code = stan_prog, data = stan_data,
                       algorithm = "NUTS", ...)

  # structure output
  out <- stanode(obj = fit, sampling)
  if (!isTRUE(sampling)) {
    out$simulations <- cbind(times, out$simulations)
    colnames(out$simulations) <- c("time", names(state))
  }

  structure(out, class = c("stanode"))
  return(out)
}
