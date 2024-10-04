library(multiDiff)
library(gsynth)

data(gsynth)
mdd_simdata_gs <- mdd_data_format(simdata,
                                  y_var = "Y",time.index = "time",
                                  treat = "D", unit.index = "id")

## check coef method
test_that("mdd_gsynth works", {
  expect_no_error(mdd_gsynth(mdd_dat=mdd_simdata_gs,
                      force = "two-way",
                      CV = FALSE, r = 2, se = TRUE,
                      inference = "parametric", nboots = 10,
                      parallel = FALSE)
  )
})

test_that("tidy works", {
  res_se <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                      force = "two-way",
                      CV = FALSE, r = 2, se = TRUE,
                      inference = "parametric", nboots = 10,
                      parallel = FALSE)
  expect_no_error(tidy(x=res_se, type ="time"))
  expect_no_error(tidy(x=res_se, type ="average"))
})

test_that("tidy works without se", {
  res_noSE <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                       force = "two-way",
                       CV = FALSE, r = 2, se = FALSE,
                       inference = "parametric", nboots = 10,
                       parallel = FALSE)
  expect_no_error(tidy(x=res_noSE, type ="time"))
  expect_no_error(tidy(x=res_noSE, type ="average"))
})

test_that("tidy works with MC", {
  res_mc <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                         force = "none", lambda=1,
                         CV = FALSE, r = 2, se = FALSE,
                         inference = "nonparametric", nboots = 200,
                         parallel = FALSE, estimator="mc")
  expect_no_error(tidy(x=res_mc, type ="time"))
  expect_no_error(tidy(x=res_mc, type ="average"))
})
