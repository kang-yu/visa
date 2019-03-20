test_that("Test my plot.lmfit function: ",
          {
            expect_true(is.ggplot(plot.lmfit(1:10, 2:11)))
          })
