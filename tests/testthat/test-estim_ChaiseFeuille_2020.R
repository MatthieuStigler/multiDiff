library(multiDiff)
data(GentzkowData)
GentzkowData_md <- mdd_data_format(GentzkowData,
                                   y_var="prestout",
                                   time.index = "year",
                                   treat = "numdailies",
                                   unit.index="cnty90" )


## estimate models
# CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md)
# CH_2020_out_dynamics <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md, brep=2, dynamic = 2, placebo=2)

CH_2024_out <- mdd_estim_CH(mdd_dat = GentzkowData_md, effects =2, placebo=2)
CH_2024_out_dynamics <- mdd_estim_CH(mdd_dat = GentzkowData_md, effects =1, placebo=0)

CH_all <- tibble::lst(#CH_2020_out, CH_2020_out_dynamics,
                      CH_2024_out, CH_2024_out_dynamics)

test_that("coef and vcov work", {
  expect_no_error(sapply(CH_all, coef))
  expect_no_error(sapply(CH_all, coef, type="Effects"))
  expect_no_error(sapply(CH_all, vcov))
})

test_that("tidy works", {
  expect_no_error(sapply(CH_all, tidy))
  expect_no_error(sapply(CH_all, tidy, type="Effects"))
})

################################
#'## Equalities
################################

dat_common <- sim_dat_common(timing_treatment = 5:10, as_mdd = TRUE, seed=123)

mdd_DD <- mdd_DD_simple(dat_common)
mdd_DD_cut <- mdd_DD_simple(dat_common |>subset(Time%in%c(4,5)))
mdd_ES <- mdd_event_study(dat_common)

mdd_CH_24_effct_1 <- mdd_estim_CH(mdd_dat = dat_common)
mdd_CH_24_effct_5 <- mdd_estim_CH(mdd_dat = dat_common, effects =5, placebo=3)



## pkg rounds coefficients! mat_res_XX[,1:4] <- round(mat_res_XX[,1:4],5)
test_that("CH with effects=1 is same as 2-2 (but with tol!!)", {
  expect_equal(coef(mdd_DD_cut),
               coef(mdd_CH_24_effct_1),
               ignore_attr=TRUE,
               tolerance=1e-5)
})

test_that("CH with effects=1 is same as first ES coef (but with tol!!)", {
  expect_equal(coef(mdd_ES)["timing_to_treat0"],
               coef(mdd_CH_24_effct_1),
               ignore_attr=TRUE,
               tolerance=1e-5)
})

test_that("CH with effects=5 is same as 5 first ES coefs (but with tol!!)", {
  expect_equal(coef(mdd_CH_24_effct_5, type="Effects")[1:5],
               coef(mdd_ES, type="Effects")[4:8],
               tolerance=1e-5,
               ignore_attr=TRUE)
})

test_that("ATE is just mean effects?", {
  expect_equal(mean(coef(mdd_CH_24_effct_5, type="Effects")[1:5]),
               coef(mdd_CH_24_effct_5, type="ATE"),
               tolerance=1e-5,
               ignore_attr=TRUE)
  expect_equal(coef(mdd_CH_24_effct_1, type="Effects"),
               coef(mdd_CH_24_effct_1, type="ATE"),
               tolerance=1e-5,
               ignore_attr=TRUE)
})


