library(multiDiff)

## data
dat_DiD_raw <- sim_dat_common()
dat_DiD <- mdd_data_format(data=dat_DiD_raw)

dat_any <- sim_dat(Time=3) |> mdd_data_format()
dat_stag <- sim_dat_staggered()|> mdd_data_format()

## compute
means_noCI <- mdd_group_means(dat_DiD)
means_CI <- mdd_group_means(dat_DiD, conf.int = TRUE)

means_any_noCI <- mdd_group_means(dat_any)
means_any_CI <- mdd_group_means(dat_any, conf.int = TRUE)
means_stag_noCI <- mdd_group_means(dat_stag)
means_stag_CI <- mdd_group_means(dat_stag, conf.int = TRUE)

length(unique(means_any_noCI$.group))
length(unique(means_stag_noCI$.group))

test_that("Same means with/without CI for common", {
  expect_equal(means_noCI$y,
               means_CI$y)
})

test_that("Same means with/without CI for any", {
  expect_equal(means_any_noCI$y,
               means_any_CI$y)
})

test_that("Same means with/without CI for stag", {
  expect_equal(means_stag_noCI$y,
               means_stag_CI$y)
})


### test weights
dat_any$weightsss <-1
dat_DiD$weightsss <-1
dat_stag$weightsss <-1

test_that("Same means with/without 1 weights", {
  expect_equal(mdd_group_means(dat_any),
               mdd_group_means(dat_any, weights=weightsss))
  expect_equal(mdd_group_means(dat_DiD),
               mdd_group_means(dat_DiD, weights=weightsss))
  expect_equal(mdd_group_means(dat_stag),
               mdd_group_means(dat_stag, weights=weightsss))
})

test_that("Same means with/without 1 weights, with CI", {
  expect_equal(mdd_group_means(dat_any, conf.int = TRUE),
               mdd_group_means(dat_any, weights=weightsss, conf.int = TRUE))
  expect_equal(mdd_group_means(dat_DiD, conf.int = TRUE),
               mdd_group_means(dat_DiD, weights=weightsss, conf.int = TRUE))
  expect_equal(mdd_group_means(dat_stag, conf.int = TRUE),
               mdd_group_means(dat_stag, weights=weightsss, conf.int = TRUE))
})
