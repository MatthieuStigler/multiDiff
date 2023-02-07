library(multiDiff)
library(synthdid)


data('california_prop99')
mdd_california_prop99 <- mdd_data_format(california_prop99,
                                         y_var = "PacksPerCapita",
                                         time.index = "Year",
                                         treat = "treated",
                                         unit.index = "State")


## Run
res <- mdd_synthdid(mdd_dat=mdd_california_prop99)
res_rerun <- mdd_synthdid(mdd_dat=mdd_california_prop99, feols_output=TRUE)

## Compare with feols_output
test_that("feols_output=TRUE gives same output", {
  expect_equal(res[1],
               coef(res_rerun)[[1]])
})


