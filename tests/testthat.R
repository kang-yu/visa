
if (!capabilities("tcltk")) {
  skip("Tcl/Tk not available; skipping all Tk-dependent tests in this file")
}


library(testthat)
library(visa)

test_check("visa")
