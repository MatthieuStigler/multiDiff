library(multiDiff)

set.seed(123)
data <- sim_dat(N=100)

## TEST standard
DD(data=data)


## TEST no year with variation
data3 <- data |>
  dplyr::mutate(tr = 0)
test_that("Output from DD is same as from DD_manu_many", {
  expect_warning(DD(data=data3),
                 "No variation in treatment found!?")
})


## TEST some years without variation
data2 <- data |>
  dplyr::mutate(tr = dplyr::if_else(Time %in% c(1,2), 0L, tr))
DD_out <- DD(data=data2)
DD_out
