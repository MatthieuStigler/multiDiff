library(multiDiff)
suppressPackageStartupMessages(library(gsynth))

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

## estimate models
res_gs_noSE <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                          force = "two-way",
                          CV = FALSE, r = 2, se = FALSE)
res_gs_se <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                        force = "two-way",
                        CV = FALSE, r = 2, se = TRUE,
                        inference = "parametric", nboots = 10)
res_fect_se <- mdd_estim_fect(mdd_dat=mdd_simdata_gs, method="gsynth",
                              force = "two-way", nboots = 10,
                              CV = FALSE, r = 2, se = TRUE)
res_fect_noSE <- mdd_estim_fect(mdd_dat=mdd_simdata_gs,
                                force = "two-way", method="gsynth",
                                CV = FALSE, r = 2, se = FALSE)


test_that("coef() on gsynth/fect gives same result", {
  expect_equal(coef(res_gs_se, type ="average"),
               coef(res_fect_se, type ="average"))
  expect_equal(coef(res_gs_noSE, type ="average"),
               coef(res_fect_noSE, type ="average"))
  expect_equal(coef(res_gs_se, type ="time"),
               coef(res_fect_se, type ="time"), ignore_attr=TRUE)
  expect_equal(coef(res_gs_noSE, type ="time"),
               coef(res_fect_noSE, type ="time"), ignore_attr=TRUE)
})

test_that("tidy() works on gsynth/fect WITH se", {
  expect_no_error(tidy(x=res_gs_se, type ="time"))
  expect_no_error(tidy(x=res_gs_se, type ="average"))
  expect_no_error(tidy(x=res_fect_se, type ="time"))
  expect_no_error(tidy(x=res_fect_se, type ="average"))
})

test_that("tidy() works on gsynth/fect WITHOUT se", {
  expect_no_error(tidy(x=res_gs_noSE, type ="time"))
  expect_no_error(tidy(x=res_gs_noSE, type ="average"))
  expect_no_error(tidy(x=res_fect_noSE, type ="time"))
  expect_no_error(tidy(x=res_fect_noSE, type ="average"))
})

test_that("tidy works with MC", {
  res_mc <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                       force = "none", lambda=1,
                       CV = FALSE, r = 2, se = FALSE,
                       inference = "nonparametric", nboots = 200,
                       estimator="mc")
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
  expect_equal(coef(gs_FE2_only),
               coef(DD_simple)[[1]])
  ## average is same as DiD
  expect_equal(coef(gs_FE2_only, type="time")[21:30],
               coef(DD_ES_post), ignore_attr="names")
  })
