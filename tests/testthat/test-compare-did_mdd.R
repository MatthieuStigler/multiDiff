library(multiDiff)
library(did)

#### sim data
df_classical <- sim_dat_common(timing_treatment = 5:10, as_mdd = TRUE, seed = 12345)
df_classical

## Convert to did
df_to_did <- df_classical |>
  multiDiff:::mdd_conv_mdd_to_did()

### estimate with DiD
did_out <- att_gt(yname = "y",
                  tname = "Time",
                  idname = "unit",
                  gname = "treat_timing",
                  data = df_to_did,
                  bstrap = FALSE,
                  cband = FALSE, est_method = "reg")
did_coefs_all <- tidy(did_out)
did_coefs_agg <- tidy(aggte(did_out, type = "simple"))

### estimate with multiDiff
DD <- mdd_DD_simple(df_classical)
ES <- mdd_event_study(df_classical)
ES_coefs <- tidy(ES)

### Compare
test_that("ES/did coefficients are the same for post", {
  expect_true(all.equal(ES_coefs$estimate[-c(1:3)], did_coefs_all$estimate[-c(1:3)]))
})

test_that("mean of post ES is same as output of aggte", {
  expect_equal(mean(coef(ES)[-c(1:3)]),
               tidy(aggte(did_out, type = "simple"))$estimate)
})

## compute individual DiD year-by-year
out_group_means <- mdd_group_means(df_classical) |>
  tidyr::spread(.group, y) |>
  mutate(diff = treated-control,
         ES_vs_4 = diff-diff[4],
         diff_lag = dplyr::lag(diff),
         diff_diff = diff-diff_lag,
         ES_as_CS = if_else(Time>=5, ES_vs_4, diff_diff))

##
test_that("ES coefs are DiD vs 4", {
  expect_equal(out_group_means$ES_vs_4[-4],
               ES_coefs$estimate,
               ignore_attr=TRUE)
})

## CS do:
# - for first to last treated period: ATT() compared to last non-treated
# - for non-treated: ATT() compared to previous year
test_that("CS ATT-coefs are: -post: vs 4, -pre: vs T-1", {
  expect_equal(out_group_means$ES_as_CS[-1],
               did_coefs_all$estimate,
               ignore_attr=TRUE)
})

test_that("CS ATT-coefs are: -post: vs 4, -pre: vs T-1. Version 2", {
  ES_vs_4 <- mdd_event_study(df_classical, time.omit = -4)
  ES_vs_3 <- mdd_event_study(df_classical, time.omit = -3)
  ES_vs_2 <- mdd_event_study(df_classical, time.omit = -2)
  ES_vs_432 <- c(coef(ES_vs_4)[1], coef(ES_vs_3)[2], coef(ES_vs_2)[3])

  expect_equal(ES_vs_432,
            did_coefs_all$estimate[1:3], ignore_attr = TRUE)

})
