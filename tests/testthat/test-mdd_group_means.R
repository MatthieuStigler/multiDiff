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

test_that("Same means for common", {
  expect_equal(means_noCI$y,
               means_CI$y)
})

test_that("Same means for any", {
  expect_equal(means_any_noCI$y,
               means_any_CI$y)
})

test_that("Same means for stag", {
  expect_equal(means_stag_noCI$y,
               means_stag_CI$y)
})
