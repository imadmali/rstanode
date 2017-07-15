library(rstanode)

context("Test miscellaneous functions")

test_that("get_lhs() works appropriately", {
  truth <- c("dy1", "dy2", "dy3", "dy4")
  # with '=' operator
  ret_string <- "return(list(c(dy1=dy1,dy2=dy2,dy3=dy3,dy4=dy4)))"
  x <- rstanode:::get_lhs(ret_string)
  expect_equal(x, truth)
  # without '=' operator
  ret_string <- "return(list(c(dy1,dy2,dy3,dy4)))"
  x <- rstanode:::get_lhs(ret_string)
  expect_equal(x, truth)
})

