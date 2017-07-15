#' clean_f
#' @description Extract and clean ODE equations in user defined function
#' @param obj A function.
#' @return A list that contains \code{ret} and \code{equations}. \code{ret}
#' provides the return argument from the function and \code{equations}
#' provides a character vector of the equations with spacing removed.
clean_f <- function(obj) {
  separate <- deparse(body(obj), width.cutoff = 500)
  separate <- gsub("[[:space:]]", "", separate)
  equations <- separate[-c(1,length(separate))]
  with_sel <- grep("with", equations)
  if (length(with_sel) > 0) {
    equations <- equations[-c(with_sel, length(equations))]
  }
  return_sel <- grep("return", equations)
  out <- list(ret = equations[return_sel])
  if (length(return_sel)==0) {
    out$equations <- equations
    return(out)
  }
  else {
    out$equations <- equations[-return_sel]
    return(out)
  }
}

# cleanup assignment operator
clean_operator <- function(eqn) {
  n_eqn <- length(eqn)
  out <- NULL
  for (i in 1:n_eqn)
    out[i] <- gsub("<-", "=", eqn[i])
  return(out)
}

# get the derivatives with respect to time as defined by the user
#' get_lhs
#' @description Get the derivatives with respect to time as defined by the user
#' in the return call at the end of the function body.
#' @param ret_string The return call as a string.
#' @return A character vector whose elements contain the names of the state
#' variables derived with resepect to time.
#' @examples
#' \dontrun{
#' ret_call <- "return(list(c(dy1,dy2,dy3,dy4)))"
#' get_lhs(ret_call)
#' }
get_lhs <- function(ret_string) {
  uses_equals <- grepl("\\=", ret_string)
  ret_string <- gsub("([[:space:]])|(return)|(list)|(c\\()|(\\()|(\\))", "", ret_string)
  ret_string_split <- unlist(strsplit(ret_string, ","))
  if (uses_equals)
    lhs <- sapply(strsplit(c(ret_string_split), "="), "[[", 2)
  else
    lhs <- ret_string_split
  #eqn_split <- strsplit(eqn, "=")
  #lhs <- sapply(eqn_split, "[[", 1)
  return(lhs)
}

get_other_vars <- function(eqn, map) {
  assigned_vars <- NULL
  n_eqn <- length(eqn)
  for (i in 1:n_eqn) {
    assigned_vars[i] <- unlist(strsplit(eqn[i], "="))[1]
  }
  sel <- assigned_vars %in% map$lhs[,"user"]
  if (all(sel))
    return(NULL)
  else {
    vars <- assigned_vars[!sel]
    return(vars)
  }
}

#' trans_vars
#' @description Translate state and parameter variables from R to Stan
#' @param eqn The return value from \code{clean_f()$equations}
#' @param map A list that defines the mapping from what the user has declared
#' (as the states, parameters, and left-hand-side) and the Stan equivalents.
#' @return A character vector that contains the ODE equations in Stan
#' language syntax.
trans_vars <- function(eqn, map) {
  n_eqn <- length(eqn)
  out <- NULL
  for (i in 1:n_eqn) {
    eqn_trim <- trim(eqn[i])
    eqn_trim <- int2real(eqn_trim)
    out[i] <- paste0(swap(eqn_trim, map), collapse = "")
  } 
  return(out)
}

#' create_other_vars
#' @description Create the declaration of other variables defined by the
#' user in the function body in Stan syntax.
#' @param x A character vector, each element of which contains a variable
#' name.
#' @return The appropriate Stan syntax variable declaration of \code{x}.
create_other_vars <- function(x) {
  return(paste0("real ", x))
}

#' swap
#' @description Replace lhs, state, and pars with the stan appropriate values
swap <- function(eqn_trim, map) {
  n_eqn_line <- length(eqn_trim)
  for (i in 1:n_eqn_line) {
    #browser()
    # clean_val <- gsub("[[:punct:]]", "", eqn_trim[i])
    clean_val <- gsub("([\\=\\*\\-\\+\\/\\^\\(\\)])", "", eqn_trim[i], perl = TRUE)
    sel_lhs <- which(map$lhs[,"user"] == clean_val)
    sel_state <- which(map$state[,"user"] == clean_val)
    sel_pars <- which(map$pars[,"user"] == clean_val)
    if (length(sel_lhs) > 0)
      eqn_trim[i] <- gsub(clean_val, map$lhs[sel_lhs, "stan"], eqn_trim[i])
    if (length(sel_state) > 0)
      eqn_trim[i] <- gsub(clean_val, map$state[sel_state, "stan"], eqn_trim[i])
    if (length(sel_pars) > 0)
      eqn_trim[i] <- gsub(clean_val, map$pars[sel_pars, "stan"], eqn_trim[i])
  }
  return(eqn_trim)
}

#' int2real
#' @description Determine whether an element of a character vector contains
#' an integer. If it does then replace it with its real value analogue.
#' @param x A character vector. Specifically the object returned by
#' \code{rstanode::trim}.
#' @return A character vector whose integer values have been converted to
#' their real value analogue.
int2real <- function(x) {
  out <- x
  no_punct <- gsub("([\\=\\*\\-\\+\\/\\(\\)\\^])", "", x, perl = TRUE)
  # sel <- !is.na(suppressWarnings(as.numeric(no_punct, na.rm = T)))
  sel <- which(!is.na(suppressWarnings(as.numeric(no_punct, na.rm = T))))
  if (length(sel) == 0)
    out <- x
  else {
    for (indx in sel)
      out[indx] <- gsub(no_punct[indx], paste0(no_punct[indx], ".0"), x[indx], fixed = TRUE)
  }
  return(out)
}

# split the equation at each operator (keeping the operator)
trim <- function(eqn_line) {
  # out <- unlist(strsplit(eqn_line, "(?<=[[:punct:]])", perl=TRUE))
  out <- unlist(strsplit(eqn_line, "(?<=[\\=\\*\\-\\+\\/\\(\\)\\^])", perl=TRUE)) # "(?<=[\\=\\*\\-\\+\\/\\^])"
  return(out)
}

# does a string end in a curly bracket
curly <- function(eqn_str) {
  len <- nchar(eqn_str)
  last <- substr(eqn_str, len, len)
  return(last %in% c("{", "}"))
}
