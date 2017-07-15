.onAttach <- function(...) {
  version <- utils::packageVersion("rstanode")
  packageStartupMessage("rstanode version ", version, " (experimental)")
}