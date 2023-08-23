library(multiDiff)

## Sim data
set.seed(123)
DID_dat_T_unsym <- sim_dat_common(N=5000, Time=8, timing_treatment = 7:8, beta = 1.2) |> mdd_data_format()
DID_dat_T2 <- sim_dat_common(N=5000, Time=2, timing_treatment = 2, beta = 1.2) |> mdd_data_format()

DID_dat_stag <- sim_dat_staggered(N=200) |> mdd_data_format()
DID_dat_gen <- sim_dat(N=200) |> mdd_data_format()


## Estimate DiD
DiD_T2_TWFE <- mdd_DD_simple(mdd_dat=DID_dat_T2)
DiD_T2_DiD <- mdd_DD_means22(mdd_dat=DID_dat_T2)
DiD_T2_DiD_manu <- DD_manu(DID_dat_T2)

DiD_T_un_TWFE <- mdd_DD_simple(mdd_dat=DID_dat_T_unsym)
DiD_T_un_22 <- mdd_DD_means22(mdd_dat=DID_dat_T_unsym)
DiD_Tun_22_tests <- attr(DiD_T_un_22, "mdd_dat_slot")$DiD_manu_tests


test_that("DiD: diff over diff-time is same as diff over diff-space", {
  expect_equal(diff(DiD_Tun_22_tests$estimate[1:2]),
               diff(DiD_Tun_22_tests$estimate[3:4]))
})

test_that("DiD: diff over diff-time is same as diff-diff ", {
  expect_equal(diff(DiD_Tun_22_tests$estimate[1:2]),
               DiD_Tun_22_tests$estimate[5])
})

test_that("DiD: TWFE is same as DiD", {
  expect_equal(coef(DiD_T_un_TWFE)[[1]],
               DiD_Tun_22_tests$estimate[5])
})

test_that("mdd_DD_means22 throws error for staggered/generalized", {
  expect_error(mdd_DD_means22(DID_dat_stag), "mdd_DD_means only implemented for `classical` DiD design", fixed=TRUE)
  expect_error(mdd_DD_means22(DID_dat_gen), "mdd_DD_means only implemented for `classical` DiD design",  fixed=TRUE)
})


