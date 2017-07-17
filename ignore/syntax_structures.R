# 
# normal <- function(loc, scale) {
#   out <- list (
#     dist = "normal",
#     loc = loc,
#     scale = scale
#   )
#   return(out)
# }
# 
# student_t <- function(df, loc, scale) {
#   out <- list(dist = "student_t",
#               df = df,
#               loc = loc,
#               scale = scale)
#   return(out)
# }
# 
# cauchy <- function(loc, scale) {
#   out <- list(dist = "cauchy",
#               loc = loc,
#               scale = scale)
# }
# 
# create_prior <- function(obj, par_name, map) {
#   indx <- unname(which(map$pars[,"user"] == par_name))
#   if (obj$dist == "student_t") {
#     stan_code <- paste0(obj$dist, "(", obj$df, ",", obj$loc, ",", obj$scale, ")", collapse = "")
#   }
#   else {
#     stan_code <- paste0(obj$dist, "(", obj$loc, ",", obj$scale, ")", collapse = "")
#   }
#   stan_code <- paste0(map$pars[indx, "stan"], " ~ ", stan_code, collapse = "")
#   stan_code <- paste0("  ", stan_code, ";\n", collapse = "")
#   return(stan_code)
# }
# 
