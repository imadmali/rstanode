#' Normal Distribution
#' @param loc Location parameter.
#' @param scale Scale parameter.
#' @export
normal <- function(loc, scale) {
  out <- list (
    dist = "normal",
    loc = loc,
    scale = scale
  )
  return(out)
}

#' Student-t Distrubtion
#' @param df Degrees of freedom parameter.
#' @param loc Location parameter.
#' @param scale Scale parameter.
#' @export
student_t <- function(df, loc, scale) {
  out <- list(dist = "student_t",
              df = df,
              loc = loc,
              scale = scale)
  return(out)
}

#' Cauchy Distribution
#' @param loc Location parameter.
#' @param scale Scale parameter.
#' @export
cauchy <- function(loc, scale) {
  out <- list(dist = "cauchy",
              loc = loc,
              scale = scale)
}

#' Uniform Distribution
#' @param a lower limit.
#' @param b upper limit.
#' @export
uniform <- function(a, b) {
  out <- list(dist = "cauchy",
              a = a,
              b = b)
}
