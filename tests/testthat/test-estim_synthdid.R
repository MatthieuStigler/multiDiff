library(multiDiff)
library(synthdid)


data('california_prop99')
mdd_california_prop99 <- mdd_data_format(california_prop99,
                                         y_var = "PacksPerCapita",
                                         time.index = "Year",
                                         treat = "treated",
                                         unit.index = "State")


## Run
res <- mdd_synthdid(mdd_dat=mdd_california_prop99)
res_full <- mdd_synthdid(mdd_dat=mdd_california_prop99, add_weights = TRUE)
W <- as.data.frame(attr(res_full, "mdd_data"))
head(W)

res_rerun <- mdd_DD_simple(mdd_california_prop99, weights =  W$weights)

## Compare with feols_output
test_that("add_weights=TRUE gives same output", {
  expect_equal(res[1],
               coef(res_rerun)[[1]])
})

## check coef method
test_that("Coef method works", {
  expect_equal(coef(res),
               -15.603828)
})

test_that("Tidy method works", {
  expect_s3_class(tidy(res),
                  "data.frame")
})

test_that("Tidy method works", {
  expect_no_error(tidy(res, conf.int = TRUE))
})


### means
# res_full_dat <- attr(res_full, "mdd_data")
# as_tibble(res_full_dat)
# mdd_group_means(res_full_dat, weights = weights) %>%
#   ggplot(aes(x=Year, y=PacksPerCapita, color=.group))+
#   geom_line()
#
# plot(mdd_california_prop99)+
#   geom_line(aes(x=Year, y=PacksPerCapita, color=.group), data=mdd_group_means(res_full_dat, weights = weights), linetype=2)
