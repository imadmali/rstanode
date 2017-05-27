#' stan_lines
#' @description Workhorse function that inputs in the ODE system as an R function
#'  (and associated state variables and parameters) and outputs the analogous
#'  function in terms of Stan syntax.
#'  
#' @param func A function (currently if-else statements are not supported).
#' @param state A named vector of the initial conditions of the state variables.
#' @param pars A named vector of the parameter values.
#' @param times A sequence of time steps.
#' @return A a string vector whose elements are equations of the R ODE system
#' translated into Stan syntax.
#' @examples
#' # EXAMPLE 1
#'
#' f1 <- function(y, t, p) {
#'   dy1 <- y2
#'   dy2 <- -y1 - theta1 * y2
#'   return(dy1 = dy1, dy2 = dy2)
#' }
#' stan_lines(f1, state = c("y1" = 2, "y2" = 5),
#'            pars = c("theta1" = 0.5),
#'            times = seq(1,10,by=0.01))
#' 
#' # EXAMPLE 2
#' 
#' f2 <- function(y, t, p) {
#'   dy1 <- theta1 * y1 + y2 * y3
#'   dy2 <- theta2 * (y2 - y3)
#'   dy3 <- -y1*y2 + (theta3)^2*y2 - y3
#' }
#' stan_lines(f2, state = c("y1" = 2, "y2" = 5, "y3" = 8),
#'            pars = c("theta1" = 0.5, "theta2" = 0.2, "theta3" = 0.8),
#'            times = seq(1,10,by=0.01))
#' 
#' # EXAMPLE 3
#' 
#' f3 <- function(y, t, p) {
#'   dy1 <- ((y1 + theta1)^2 + y2^2)^(3/2)
#'   dy2 <- ((y1 - theta2)^2 + y2^2)^(3/2)
#'   dy3 <- y1 + 2*dy2 - theta2 * ((y1 + theta1)/dy1) - theta1 * ((y1 - theta2)/ dy2)
#'   dy4 <- y2 - 2*dy1 - theta2 * (y2/dy1) - theta1 * (y2/dy2)
#' }
#' stan_lines(f3, state = c("y1" = 2, "y2" = 5),
#'            pars = c("theta1" = 0.5, "theta2" = 0.2),
#'            times = seq(1,10,by=0.01))
#' 
#' @export

stan_lines <- function(func, state, pars, times) {
  state_names <- names(state)
  pars_names <- names(pars)
  map <- list(state = cbind("stan" = paste0("y[", 1:length(state_names), "]"), "user" = state_names),
              pars = cbind("stan" = paste0("theta[", 1:length(pars_names), "]"), "user" = pars_names),
              n_states = length(state_names),
              n_pars= length(pars_names))
  f_out <- clean_f(func)
  f_out <- clean_operator(f_out)
  map$lhs <- cbind("stan" = paste0("dydt[", 1:length(get_lhs(f_out)), "]"), "user" = get_lhs(f_out))
  f_out <- trans_vars(f_out, map)
  f_out <- sapply(f_out, function(x) {paste0("    ", x, ";")}, USE.NAMES = FALSE)
  return(f_out)
}