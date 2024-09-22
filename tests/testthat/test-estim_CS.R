library(multiDiff)
library(broom)
library(dplyr)
# library(testthat)

## sim dat
dat_classical <- sim_dat_common(as_mdd = TRUE, timing_treatment = 2, Time = 2)
dat_common <- sim_dat_common(as_mdd = TRUE, timing_treatment = 5:10)
dat_stag <- sim_dat_staggered(as_mdd = TRUE, Time=6)
dat_stag_2 <- sim_dat_staggered(as_mdd = TRUE, Time=6, timing_treatment = 5:6)


test_that("just run it", {
  expect_no_error(mdd_CS(dat_stag, control_group = "nevertreated"))
  expect_no_error(mdd_CS(dat_stag, control_group = "notyettreated"))
})


## DiD T=2: same with mdd and CS
test_that("With T=2, CS gives same values", {

  DiD_2 <- mdd_DD_simple(dat_classical)
  CS_2 <- suppressMessages(mdd_CS(dat_classical))

  expect_equal(coef(DiD_2)[[1]],
               broom::tidy((CS_2))$estimate)
})

## DiD common, T=10
DiD <- mdd_DD_simple(dat_common)
ES <- mdd_event_study(dat_common)

CS_cmn <- mdd_CS(dat_common)
CS_cmn_rebase <- mdd_CS(dat_common, base_period = "universal")

test_that("With T=10 and common shock, CS gives same values as ES", {

  ## with base_period="varying"
  expect_equal(broom::tidy(CS_cmn)$estimate[4:9],
               unname(coef(ES)[4:9]))
  ## with base_period="varying"
  expect_equal(broom::tidy(CS_cmn_rebase)$estimate[-4],
               unname(coef(ES)))
})

test_that("With T=10 and common shock, CS gives same as did::aggte", {

  CS_aggreg <- suppressWarnings(did::aggte(CS_cmn))
  expect_equal(mean(broom::tidy(CS_cmn)$estimate[4:9]),
               broom::tidy(CS_aggreg)$estimate[1])
})


################################
#'## Compare simple staggered
################################


DID_0_5 <- mdd_data_format(dat_stag_2 %>% filter(timing_treat %in% c(Inf,5)))
DID_0_6 <- mdd_data_format(dat_stag_2 %>% filter(timing_treat %in% c(Inf,6)))
DID_5_6 <- mdd_data_format(dat_stag_2 %>%
                             filter(!(Time ==6 & timing_treat ==6)) %>%
                             mutate(tr = if_else(timing_treat ==6, 0, tr)))

pl_treat <- function(mdd_dat){
  mdd_dat %>%
    as_tibble() %>%
    add_group() %>%
    distinct(.group, Time, tr) %>%
    mutate(tr = if_else(tr==0, "Untreated", "Treated")) %>%
    ggplot(aes(x=Time, y=.group, fill = tr)) +
    geom_tile()
}
# pl_treat(mdd_dat = dat_stag_2)
# pl_treat(mdd_dat = DID_5_6)

test_that("With staggered, CS is same as separate regs", {

  ES_0_5 <- mdd_event_study(DID_0_5)
  ES_0_6 <- mdd_event_study(DID_0_6)
  ES_5_6 <- mdd_event_study(DID_5_6)

  CS_stag2_never <- tidy(mdd_CS(dat_stag_2, control_group = "nevertreated", base_period = "universal"))
  CS_stag2_notyet <- tidy(mdd_CS(dat_stag_2, control_group = "notyettreated", base_period = "universal"))

  expect_equal(unname(coef(ES_0_5)),
               filter(CS_stag2_never, group==5)$estimate[-4])
  expect_equal(unname(coef(ES_0_6)),
               filter(CS_stag2_never, group==6)$estimate[-5])
  expect_equal(unname(coef(ES_5_6))[-5],
               filter(CS_stag2_notyet, group==5)$estimate[-c(4, 6)])

  ## last one unclear...
  expect_true(unname(coef(ES_5_6))[5] !=filter(CS_stag2_notyet, group==5)$estimate[6])
})

################################
#'## Check manu
################################


test_that("mdd_CS_manu gives same", {
  dat_stag <- sim_dat_staggered(as_mdd = TRUE, Time=6)
  CS_mine <- mdd_CS_manu(dat_stag)
  CS_CS <- mdd_CS(dat_stag)

  expect_equal(CS_mine |>
                 dplyr::select(term, group, time, estimate)|>
                 as.data.frame(),
               tidy(CS_CS) |>
                 dplyr::select(term, group, time, estimate))
})

test_that("mdd_CS_manu gives same with noyettreated", {
  dat_stag <- sim_dat_staggered(as_mdd = TRUE, Time=6)
  CS_mine_notYet <- mdd_CS_manu(dat_stag, control_group = "notyettreated") |>
    dplyr::select(term, group, time, estimate)
  CS_CS_notYet <- mdd_CS(dat_stag, control_group = "notyettreated") |>
    tidy()|>
    dplyr::select(term, group, time, estimate)

  expect_equal(CS_mine_notYet|>
                 as.data.frame(),
               CS_CS_notYet)
})

test_that("mdd_CS_manu gives same even when using unequal years", {
  dat_stag_raw <- sim_dat_staggered(as_mdd = FALSE, Time=6)
  dat_stag <- dat_stag_raw |>
    dplyr::left_join(tibble::tibble(Time=1:6, Year = c(2000, 2003, 2007:2010)), by="Time") |>
    dplyr::select(unit, Year, tr, y)
  dat_stag_mdd <- mdd_data_format(dat_stag, time.index = "Year")

  CS_mine_Y <- mdd_CS_manu(dat_stag_mdd)
  CS_CS_Y <- mdd_CS(dat_stag_mdd)

  expect_equal(CS_mine_Y |>
                 dplyr::select(term, group, time, estimate)|>
                 as.data.frame(),
               tidy(CS_CS_Y) |>
                 dplyr::select(term, group, time, estimate))
})

