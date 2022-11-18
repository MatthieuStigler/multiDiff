library(multiDiff)

## data
dat_DiD_raw <- sim_dat_common()
dat_DiD <- mdd_data_format(data=dat_DiD_raw)

dat_any <- sim_dat() |> mdd_data_format()
dat_stag_raw <- sim_dat_staggered()|> mdd_data_format()

## compute
means_noCI <- mdd_group_means(dat_DiD)
means_CI <- mdd_group_means(dat_DiD, conf.int = TRUE)

means_stag_noCI <- mdd_group_means(dat_any_raw)

test_that("Same means", {
  expect_equal(means_noCI$y,
               means_CI$y)
})
