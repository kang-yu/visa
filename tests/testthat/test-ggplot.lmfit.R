test_that("Test my plot.lmfit function: ",
          {
            expect_true(is.ggplot(ggplot.lmfit(1:10, 2:11)))
          })
