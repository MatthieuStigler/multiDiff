library(multiDiff)

DID_dat_raw <- sim_dat_common(N=5000, Time=8,
                              beta=1.1,
                              timing_treatment = 6:8, perc_treat=0.5)

DID_dat <- mdd_data_format(DID_dat_raw)

## Manu means
means_manu <- DID_dat %>%
  multiDiff:::mdd_simple_means() %>%
  tidyr::spread(.group, y) %>%
  dplyr::mutate(diff=treated-control)

means_manu

## Did simple full
DiD_simple <- mdd_DD_simple(DID_dat)
coef(DiD_simple)

## Did simple: only 2Y
test_that("DID for 2Y is same as diff-diff means", {
  expect_equal(diff(means_manu[c(5,6), "diff", drop=TRUE]),
               coef(mdd_DD_simple(data=DID_dat %>% filter(Time %in% c(5, 6))))[[1]])
})

## event
ES <- mdd_event_study(data=DID_dat)
summary(ES)
plot(ES)

test_that("ES is same as diffs in DD_manu: diff- diff[5]", {
  expect_equal(coef(ES) %>% as.numeric(),
               (means_manu$diff - means_manu$diff[5])[-5])
})

test_that("ES 0 effect is same as diffs annual", {
  expect_equal(coef(ES)["timing_to_treat0"][[1]],
               coef(mdd_DD_simple(data=DID_dat %>% filter(Time %in% c(5, 6))))[[1]])
})


mdd_event_study(data=DID_dat)
mdd_event_study(data=DID_dat, trim_high = 1)
mdd_event_study(data=DID_dat, trim_high = 0)
mdd_event_study(data=DID_dat, trim_high = 0, trim_low = -3)

## weights
test_that("Using with weights in ES works", {
  expect_equal(mdd_event_study(data=DID_dat, weights = rep(c(0, 1), c(8, nrow(DID_dat)-8))) %>% coef(),
               mdd_event_study(data=DID_dat %>% tail(-8)) %>% coef())
})
