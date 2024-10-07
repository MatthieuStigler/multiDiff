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


test_that("Relationship with FE: with r=0 and force = 'two-way'", {
  gs_FE2_only <- gsynth(Y ~ D, data = mdd_simdata_gs, parallel = FALSE,
                         index = c("id","time"), force = "two-way", estimator="ife",
                         CV = FALSE, r = 0, se = FALSE)
  DD_simple <- mdd_DD_simple(mdd_simdata_gs)
  DD_ES_post <- mdd_event_study(mdd_simdata_gs, trim_low = -1)

  ## average is same as DiD
  expect_equal(gs_FE2_only$att.avg,
               coef(DD_simple)[[1]])
  ## average is same as DiD
  expect_equal(gs_FE2_only$att[21:30],
               coef(DD_ES_post), ignore_attr="names")
  })
