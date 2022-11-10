library(multiDiff)

## Sim data
set.seed(123)
DID_dat <- sim_dat_common(N=5000, Time=8,
                          beta=1.1,
                          timing_treatment = 6:8, perc_treat=0.5)

DID_dat_mdd <- mdd_data_format(DID_dat)
mdd_data_format(DID_dat)

## Estimate DiD
DiD <- mdd_DD_simple(data=DID_dat)

## Estimate annual DiD
DD_out <- DD(data=DID_dat) |>
  dplyr::filter(n_treat >0 & n_control>0)

## DD manu
DD_manu_out <- DD_manu(data=DID_dat,
                       control_gr = "0_0_0_0_0_0_0_0",
                       treat_gr = "0_0_0_0_0_1_1_1")

## estimate ES
ES <- mdd_event_study(mdd_dat=DID_dat_mdd)
ES_rebase_2 <- mdd_event_study(mdd_dat=DID_dat_mdd, time.omit = -2)
ES_trim2 <- mdd_event_study(mdd_dat=DID_dat_mdd, trim_high = 1)
ES_trim_neg <- mdd_event_study(mdd_dat=DID_dat_mdd, trim_low = -1)
ES_trim_all <- mdd_event_study(mdd_dat=DID_dat_mdd, trim_high = 0, trim_low = -1, time.omit = -1)


test_that("DID is same as mean(post-diff) - mean(pre-diff)", {
  expect_equal(coef(DiD)[[1]],
               mean(DD_manu_out$diff[c(6:8)])-mean(DD_manu_out$diff[c(1:5)]))
})

test_that("ES 0 with rebase -1 is same as annual DiD", {
  expect_equal(DD_out$estimate,
               coef(ES)[["timing_to_treat0"]])
})

test_that("ES is same as diffs in DD_manu: diff- diff[5]", {
  expect_equal(coef(ES) %>% as.numeric(),
               (DD_manu_out$diff - DD_manu_out$diff[5])[-5])
})

test_that("ES with rebase -2 is same as diffs in DD_manu: diff- diff[4]", {
  expect_equal(coef(ES_rebase_2) %>% as.numeric(),
               (DD_manu_out$diff - DD_manu_out$diff[4])[-4])
})


### Test more complicated relationships
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


