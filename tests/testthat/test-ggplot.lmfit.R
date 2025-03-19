
if (!capabilities("tcltk")) {
  skip("Tcl/Tk not available; skipping all Tk-dependent tests in this file")
}

library(testthat)
library(visa)

test_that("Test my ggplot.lmfit function", {
  expect_true(is.ggplot(ggplot.lmfit(1:10, 2:11)))
})
