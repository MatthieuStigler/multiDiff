library(multiDiff)
library(synthdid)
suppressPackageStartupMessages(library(gsynth))
suppressPackageStartupMessages(library(texreg))

## data
mdd_data <- sim_dat_common(as_mdd = TRUE, Time = 20, timing_treatment = 15:20, seed=3746)

## models
mdd_DD <- mdd_DD_simple(mdd_data)
mdd_synthDD <- mdd_synthdid(mdd_data)
mdd_gs <- mdd_gsynth(mdd_data, echo=FALSE, se=TRUE, k=0:2, nboots=10, seed=263754, inference="parametric")
mdd_gs_mc <- mdd_gsynth(mdd_data, echo=FALSE, estimator="mc", se=FALSE, seed=83475)

all <- list(mdd_DD=mdd_DD, mdd_synthDD=mdd_synthDD, mdd_gs=mdd_gs, mdd_gs_mc=mdd_gs_mc)

##### manual
setMethod("extract", signature = className("mdd_DiD", "multiDiff"), definition = multiDiff:::extract_mdd_DiD) # see https://github.com/leifeld/texreg/issues/200
setMethod("extract", signature = className("synthdid_estimate", "multiDiff"), definition = multiDiff:::extract.synthdid)
setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)



test_that("One issue", {
  expect_error(suppressWarnings(screenreg(mdd_gs_mc)))
})

test_that("Work when run together", {
  expect_no_error(suppressWarnings(screenreg(all)))
})

test_that("Snapshot output", {
  expect_snapshot(suppressWarnings(screenreg(all)))
})


