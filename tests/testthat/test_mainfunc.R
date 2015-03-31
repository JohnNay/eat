library(eat)
context("Sobol SA function")

test_that("sobol_sa() returns correct object", {
  fake_abm <- function(params, out) {
    x1 <- params[1]
    x2 <- params[2]
    if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
    if (out=="ident") return(x1 + x2 + rnorm(1, 0))
  }
  inputs <- lapply(list(param1 = NA, param2 = NA), 
                   function(x) list(random_function = "qunif",
                                    ARGS = list(min = 0, max = 1)))
  expect_is(sobol_sa(fake_abm, inputs, "sq"), "sobol2007")
})

