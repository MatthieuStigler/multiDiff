library(multiDiff)

test_that("sim_dat_common check for dyn input", {
  expect_error(sim_dat_common(beta_dyn = rep(1,6)), "`beta_dyn` should be of length 1 or 8 (number of post treatment periods)",fixed=TRUE)
  expect_no_error(sim_dat_common(beta_dyn = rep(1,8)))
  expect_no_error(sim_dat_common(beta_dyn = 1))
})

test_that("sim_dat_common with beta_dyn:  first coeffs are identical", {
  df_0 <- sim_dat_common(timing_treatment = 5, as_mdd = TRUE, seed = 123)
  df_1 <- sim_dat_common(timing_treatment = 5, beta_dyn = rep(1,5), as_mdd = TRUE, seed = 123)
  coef_0 <- coef(mdd_event_study(df_0))
  coef_1 <- coef(mdd_event_study(df_1))
  expect_equal(coef_0[1:4], coef_1[1:4])
})
