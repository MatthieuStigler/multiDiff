library(multiDiff)

df_sim_common <- sim_dat_common(as_mdd = TRUE)
df_sim_stag <- sim_dat_staggered(as_mdd = TRUE)
df_sim_gen <- sim_dat(as_mdd = TRUE)

test_that("as_mdd returns mdd_dat object", {
  expect_s3_class(df_sim_common, "mdd_dat")
  expect_s3_class(df_sim_stag, "mdd_dat")
  expect_s3_class(df_sim_gen, "mdd_dat")
})


test_that("intrnl_add_treat_status_mdd returns df/mdd", {
  here <- multiDiff:::intrnl_add_treat_status_mdd

  expect_s3_class(here(df_sim_common), "data.frame")
  expect_s3_class(here(df_sim_common, keep_mdd = TRUE), "mdd_dat")
  expect_s3_class(here(df_sim_stag), "data.frame")
  expect_s3_class(here(df_sim_stag, keep_mdd = TRUE), "mdd_dat")
  expect_s3_class(here(df_sim_gen), "data.frame")
  expect_s3_class(here(df_sim_gen, keep_mdd = TRUE), "mdd_dat")
})

##
test_that("intrnl_add_treat_time_mdd", {
  df_target <- data.frame(stringsAsFactors = FALSE,
                          .group = c("0_0_0_0_0_0","0_1_1_1_1_1",
                                     "0_0_1_1_1_1","0_0_0_1_1_1","0_0_0_0_1_1",
                                     "0_0_0_0_0_1"),
                          type = c("never","treat","treat","treat",
                                   "treat","treat"),
                          treat_timing = c(0, 2, 3, 4, 5, 6))

  df <- sim_dat_staggered(as_mdd = TRUE, Time=6) |>
    multiDiff:::intrnl_add_treat_time_mdd() |>
    multiDiff:::add_group() |>
    dplyr::distinct(.group, type, treat_timing) |>
    dplyr::arrange(treat_timing) |>
    as.data.frame()
  expect_equal(df, df_target)

})
