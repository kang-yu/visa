test_that("Test my ggplot.lmfit function: ",
          {
            expect_true(is.ggplot(ggplot.lmfit(1:10, 2:11)))
          })
