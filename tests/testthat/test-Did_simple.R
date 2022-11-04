library(multiDiff)

set.seed(123)
DID_dat <- multiDiff::sim_dat_staggered(N=5000, perc_always = 0,
                                        Time=8,
                                        beta=1.1,
                                        timing_treatment = 6, perc_treat=0.5)

DiD <- mdd_DD_simple(data=DID_dat)
ES <- mdd_event_study(data=DID_dat)
ES_trim2 <- mdd_event_study(data=DID_dat, trim_high = 1)
ES_trim_neg <- mdd_event_study(data=DID_dat, trim_low = -1)
ES_trim_all <- mdd_event_study(data=DID_dat, trim_high = 0, trim_low = -1, time.omit = -1)


test_that("Trimming event study gives mean of trimmed coefs", {
  expect_equal(mean(tail(coef(ES),2)),
               tail(coef(ES_trim2),1)[[1]])
})

test_that("Trimming ES is equivalent to DiD", {
  expect_equal(coef(ES_trim_all)[[1]],
               coef(DiD)[[1]])
})

test_that("Trimming ES is equivalent to DiD 2", {
  expect_equal(mean(coef(ES_trim_neg)),
               coef(DiD)[[1]])
})


