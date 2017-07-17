create_prior <- function(obj, par_name, map) {
  indx <- unname(which(map$pars[,"user"] == par_name))
  if (obj$dist == "student_t") {
    stan_code <- paste0(obj$dist, "(", obj$df, ",", obj$loc, ",", obj$scale, ")", collapse = "")
  }
  else if (obj$dist == "uniform") {
    stan_code <- paste0(obj$dist, "(", obj$a, ",", obj$b, ")", collapse = "")
  }
  else {
    stan_code <- paste0(obj$dist, "(", obj$loc, ",", obj$scale, ")", collapse = "")
  }
  if (length(indx) == 0 )
    stan_code <- paste0(par_name, " ~ ", stan_code, collapse = "")
  else
    stan_code <- paste0(map$pars[indx, "stan"], " ~ ", stan_code, collapse = "")
  stan_code <- paste0("  ", stan_code, ";", collapse = "")
  return(stan_code)
}

create_likelihood <- function(obj, par_name) {
  num <- gsub("[[:alpha:]]", "", par_name)

  for (i in 1:length(obj)) {
    if (is.null(obj[[i]][[1]]))
      obj[[i]][[1]] <- paste0("y_hat[,", num,"]")
  }

  if (obj$dist == "student_t") {
    stan_code <- paste0(obj$dist, "(", obj$df, ", ", obj$loc, ", ", obj$scale, ")", collapse = "")
  }
  else {
    stan_code <- paste0(obj$dist, "(", obj$loc, ", ", obj$scale, ")", collapse = "")
  }
  stan_code <- paste0("y[,", num,"]", " ~ ", stan_code, collapse = "")
  stan_code <- paste0("  ", stan_code, ";", collapse = "")
  return(stan_code)
}

get_new_pars <- function(pars, new_pars) {
  sel <- new_pars %in% pars
  new_pars_nms <- new_pars[!sel]
  return(new_pars_nms)
}
create_new_pars <- function(par, likelihood_list) {
  ll <- unlist(likelihood_list)
  sel <- which(par == unlist(likelihood_list, use.names = FALSE))
  sel <- head(sel, n = 1)
  if (grepl("(scale)|(shape)", names(ll)[sel]))
    const <- "<lower=0> "
  else
    const <- " "
  out <- paste0("  real", const, par, ";", collapse = "")
  return(out)
}

# likelihood <- list(y1 = normal(NULL, "sigma1"), y2 = student_t(2, "sigma2", NULL))
# priors <- list(theta = normal(0,1), sigma2 = cauchy(0,1))






