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
#'## Convert char unit id
################################

result <- c(Reduce(function(x, y) outer(x, y, paste, sep=""), list(letters, letters, letters)))

dat_stag2 <- dat_stag |>
  as_tibble() |>
  distinct(unit) |>
  mutate(unit_id_char=result[1:n()]) |>
  right_join(dat_stag, by ="unit") |>
  mdd_data_format(unit.index = "unit_id_char")

dat_stag2

test_that("Works with char id", {
  expect_no_error(mdd_CS(dat_stag2))

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

mdd_CS_manu <- multiDiff:::mdd_CS_manu
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

################################
#'## cross-sections
################################

## create cross-section of regressions
df_raw <- sim_dat_staggered(as_mdd = TRUE, Time=4, seed=123, perc_never=0.3,
                            perc_treat=0.7, N=1600) %>%
  select(-c(unit_fe, time_fe, error, length_treat))

## create group-wise ids
df_ids <- df_raw%>%
  as_tibble() %>%
  distinct(unit, timing_treat) %>%
  arrange(timing_treat) %>%
  add_count(timing_treat) %>%
  mutate(unit_new = 1:n(), .by=timing_treat) %>%
  arrange(unit_new)

## add group-wise ids
df_raw_plus <- df_raw %>%
  left_join(df_ids,by = join_by(unit, timing_treat))%>%
  as.data.frame() %>%
  filter((unit_new %in% 1:100 & Time==1)| (unit_new %in% 101:200 & Time==2)|(unit_new %in% 201:300 & Time==3)|
           (unit_new %in% 301:400 & Time==4)) %>%
  as_tibble()

df_raw_plus %>%
  count(Time, timing_treat) %>%
  tidyr::spread(Time, n)


test_that("mdd_CS_manu gives same results as did:: with unbalnced data", {

  ## estimate mine
  df_stag_md <- suppressWarnings(mdd_data_format(df_raw_plus))
  my_CS_cross <- multiDiff:::mdd_CS_manu(mdd_dat = df_stag_md, timing_treat_var="timing_treat")
  set.seed(123)
  CS_CS_cross <- mdd_CS(mdd_dat = df_stag_md, timing_treat_var="timing_treat")

  ## estimate did CS
  library(did)
  set.seed(123)
  did_CS_CS_cross <- did::att_gt(yname = "y", tname = "Time", idname = "unit",
                                 gname = "timing_treat",
                                 data = df_stag_md %>%
                                   as_tibble(),
                                 panel = FALSE)

  ## compare
  expect_equal(my_CS_cross%>%
                 select(term, group, time, estimate) %>%
                 as.data.frame(),
               tidy(did_CS_CS_cross) %>%
                 select(term, group, time, estimate))
  expect_equal(tidy(CS_CS_cross),
               tidy(did_CS_CS_cross))

})

test_that("mdd_CS/manu don't work with cross-section", {
  expect_error(suppressWarnings(mdd_CS_manu(mdd_dat = df_stag_md)))
  expect_error(suppressWarnings(mdd_CS(mdd_dat = df_stag_md)))
})

################################
#'## Harder: data with empty cells
################################

df_raw_plus %>%
  count(Time, timing_treat) %>%
  tidyr::spread(Time, n)

df_raw_plus_plus <- df_raw_plus %>%
  filter(!(timing_treat==Inf & Time==2) &
           !(timing_treat==3 & Time==2) & Time!=4) #control_group="notyettreated"

df_raw_plus_plus %>%
  count(Time, timing_treat) %>%
  tidyr::spread(Time, n)

did_to_tidy <- function(x) tidy(x) %>% select(term, group, time, estimate) %>% filter(!is.na(estimate)) %>% as_tibble()

test_that("mdd_CS_manu gives same results as did:: with unbalanced data and empty cells", {

  ## estimate mine
  df_stag_plus_md <- suppressWarnings(mdd_data_format(df_raw_plus_plus))
  my_CS_cross2_never <- mdd_CS_manu(mdd_dat = df_stag_plus_md, timing_treat_var="timing_treat", control_group="nevertreated")
  my_CS_cross2_notYet <- mdd_CS_manu(mdd_dat = df_stag_plus_md, timing_treat_var="timing_treat", control_group="notyettreated")

  set.seed(123)
  CS_CS_cross2_never <- suppressWarnings(mdd_CS(mdd_dat = df_stag_plus_md, timing_treat_var="timing_treat",
                                               control_group="nevertreated")) %>% did_to_tidy
  CS_CS_cross2_notYet <- suppressWarnings(mdd_CS(mdd_dat = df_stag_plus_md, timing_treat_var="timing_treat",
                                                control_group="notyettreated")) %>% did_to_tidy

  ## estimate did CS
  library(did)
  set.seed(123)
  did_CS_CS_cross2_never <- suppressWarnings(did::att_gt(yname = "y", tname = "Time", idname = "unit",
                                                         gname = "timing_treat",
                                                         data = df_raw_plus_plus,
                                                         panel = FALSE)) %>% did_to_tidy
  did_CS_CS_cross2_notYet <- suppressWarnings(did::att_gt(yname = "y", tname = "Time", idname = "unit",
                                                          gname = "timing_treat",
                                                          data = df_raw_plus_plus,
                                                          control_group="notyettreated",
                                                          panel = FALSE))%>% did_to_tidy
  ## compare
  expect_equal(did_CS_CS_cross2_never, my_CS_cross2_never %>% select(term, group, time, estimate))
  expect_equal(CS_CS_cross2_never, did_CS_CS_cross2_never)

  expect_equal(CS_CS_cross2_notYet, my_CS_cross2_notYet %>% select(term, group, time, estimate))
  expect_equal(CS_CS_cross2_notYet, did_CS_CS_cross2_notYet)

})

