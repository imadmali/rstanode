#' stan_ode
#' @description An ODE wrapper for RStan.
#' @param func A function (currently if-else statements are not supported).
#' @param state A named vector of the initial conditions of the state variables.
#' @param pars A named vector of the parameter values.
#' @param times A sequence of time steps.
#' @param t0 Initial time.
#' @param integrator The type of integrator to use.
#' @param sampling A logical declaration as to whether you want sample the parameters.
#' @param ... Optional parameters for \code{rstan::stan()}.
#' @return A Stan fit object containing ODE simulations/samples.
#' @examples
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
#'@export

stan_ode <- function(func, state, pars, times, t0 = NULL,
                    integrator = c("rk45", "bdf"), sampling = FALSE,
                    events = NULL, ...) {
  
  # deal with start times
  t0_accuracy <- 1e-6
  if (is.null(t0)) {
    if (is.null(events))
      t0 <- times[1] - t0_accuracy
    else
      t0 <- c(times[1], events$time) - t0_accuracy
  }

  # create stan program
  stan_ode_eqns <- stan_lines(func, state, pars, times)
  stan_prog <- stan_ode_generate(stan_ode_eqns,
                                 has_events = ifelse(is.null(events), FALSE, TRUE),
                                 integrator = integrator)
  
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
                    ts = times,
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

  # simulate/fit from stan model
  if (sampling == FALSE)
    fit <- rstan::stan(model_code = stan_prog, data = stan_data,
                       algorithm = "Fixed_param", chains = 1, iter = 1, ...)
  else
    stop("Currently samping = TRUE is not supported.")
  
  # structure output
  out <- stanode(obj = fit)
  out$simulations <- cbind(times, out$simulations)
  colnames(out$simulations) <- c("time", names(state))

  structure(out, class = c("stanode"))
  return(out)
}
