

## from: https://stackoverflow.com/questions/38245975/dplyr-custom-lag-function-for-irregular-time-series
lag_group_old <- function(df, group_var, time_var, lag_var, lagamount=1){
  range_years <- range(df %>% pull(!!enquo(time_var)), na.rm=TRUE)
  df %>%
    group_by(!!enquo(group_var)) %>%
    tidyr::complete(!!enquo(time_var) := min(range_years):max(range_years)) %>% #!!enquo(group_var)
    mutate(lag = dplyr::lag(!!enquo(lag_var), lagamount)) %>%
    ungroup() %>%
    filter(!is.na(!!enquo(lag_var)))
}

#' Lag a variable by group
#'
#' Take the lag of a variable for each group.
#' Will check also that there are no missing years.
#'
#' @param df data frame
#' @param group_var The grouping variable
#' @param time_var The time variable.
#' This is necessary as the function makes sure there are no missing years.
#' @param value_var The variable to lag over
#' @param lagamount The amount of lag
#' @param complete_time Sequence to use for 'completing' df, making sure implicit NAs are turned into explicit ones.
#' @param default Default value for missing? NA by default.
#' @export
#' @examples
#' df_test <- data.frame(group = rep(c("a", "b"), each=6),
#'                       year = rep(2000:2005, 2),
#'                       value = (0:11) ^ 2,
#'                       value2 = rnorm(12))
#' lag_group(df_test, "group", time_var="year", value_var="value", lagamount = -1:2)
#'
#' ## Behaviour with missing years
#' lag_group(df_test[-4,], "group", time_var="year", value_var="value", lagamount = -1:2)
#'
#' ## Many lags and many variables:
#'
#' lag_group(df_test[-4,], "group", time_var="year", lagamount = 1:2, value_var = c("value", "value2"))
lag_group <- function(df, group_var, value_var, time_var,
                      lagamount=1, complete_time = seq(min(df[[time_var]]), max(df[[time_var]]), by=1L),
                      default = NA) {

  ## complete
  time_range <- range(pull(df, {{time_var}}))
  df_complete <- tidyr::complete(df,
                                 !!rlang::sym(group_var),
                                 !!enquo(time_var):=complete_time)

  ##
  N_var <- length(value_var)
  N_lags <- length(lagamount)
  old_class <- class(df)

  ## I am copying here, probably not correct...
  setDT(df_complete)
  lags_vec <- rep(lagamount, rep = N_var)
  lags_name <- ifelse(lags_vec>=0, "_lag", "_lead")
  new_names <- paste0(rep(value_var, each = N_lags),
                      lags_name,
                      abs(lags_vec))
  df_complete[,  matrix(new_names, nrow=N_var) := shift(.SD, lagamount, fill=default), by = group_var, .SDcols = value_var][]
  setDF(df_complete) ## maybe unnecessary?
  setattr(df_complete, "class", old_class)

  ## get dims back
  df_complete %>%
    dplyr::semi_join(df, by = c(group_var, time_var))
}

# lag_group <- function(df, group_var, time_var, lag_var, lagamount=1){
#   # cl <- class(pull(head(df, 1), !!enquo(lag_var)))
#
#   df %>%
#     group_by(!!enquo(group_var)) %>%
#     mutate(lag = ifelse(!!enquo(time_var) - dplyr::lag(!!enquo(time_var), lagamount) == lagamount,
#                              dplyr::lag(!!enquo(lag_var), lagamount),
#                              NA_real_)) %>%
#     ungroup() %>%
#     filter(!is.na(!!enquo(lag_var)))
# }


## make dplyr work for lags and leads
lead_lag <- function(x, n, ...) {
  if(n>0) {
    return(dplyr::lag(x, n, ...))
  } else if (n==0) {
    return(x)
  } else {
    return(dplyr::lead(x, abs(n), ...))
  }
}

## lag robust: in case of missing value
lag_robust <- function(x, time_var, lagamount, NA_val=NA_real_) {
  ifelse(time_var - lead_lag(time_var, lagamount) == lagamount,
         lead_lag(x, lagamount),
         NA_val)
}


## add many lags to many variables
lag_manyXN <- function(df_mini, .lagamount=1, .time_var, ...) {
  time <- pull(df_mini, !!rlang::enquo(.time_var))
  df_mini %>%
    bind_cols(purrr::imap_dfc(df_mini %>%
                                select(!!!enquos(...)), ~magrittr::set_names(map(.lagamount, function(i) lag_robust(x=.x, time, lagamount=i)),
                                                                             paste0(.y, ifelse(.lagamount>0, '_lag','_lead'), abs(.lagamount)))))
}


lag_group_many <- function(df, group_var, time_var, lagamount=1, ...){
  df %>%
    nest(data = -!!enquo(group_var)) %>%
    mutate(lags = map(.data$data, ~lag_manyXN(.,
                                              .lagamount=lagamount,
                                              .time_var=!!enquo(time_var), !!!enquos(...)) %>%
                        select(matches("(lead|lag)[0-9]$")))) %>%
    unnest(c(.data$data, .data$lags))
}

# vhttps://stackoverflow.com/questions/44750761/adding-multiple-lag-variables-using-dplyr-and-for-loops




## TEST
if(FALSE){
  dplyr::lag(1:4, order_by = c(1,2,3,4))
  dplyr::lag(1:4, order_by = c(1,2,3,5))

}


if(FALSE) {

  library(multiDiff)
  library(tidyverse)
  df_test <- tibble(group = rep(c("a", "b"), each=6),
                    year = rep(2000:2005, 2),
                    value = (0:11) ^ 2,
                    value2 = rnorm(12))

  df_test[4,3] <- NA
  df_test_implicitNA <- df_test %>%
    filter(!is.na(value))

  ## one var
  df_test_implicitNA %>%
    lag_group(time_var=year, lag_var=value, group_var=group)

  ## test unexported
  df_test_implicitNA %>%
    filter(group=="a") %>%
    multiDiff:::lag_manyXN(.lagamount = 1:2, .time_var = year, value) %>%
    mutate(manu_2 = lag_robust(value, year, lagamount = 2))

  ## many var
  res1 <- df_test_implicitNA %>%
    lag_group_many(time_var=year, group_var=group, lagamount = 1:2, value, value2)
  res2 <- df_test_implicitNA %>%
    lag_group_dt(group_var="group", time_var = "year", value_var = c("value", "value2"),
                 lagamount = 1:2)
  all.equal(res1 %>% as.data.frame(), res2 %>%  as.data.frame())
  class(a)
  class(df_test)
  df_test %>%
    lag_group_dt(group_var="group", value_var = c("value", "value2"), lagamount = 1:2)

  df_test %>%
    filter(!is.na(value)) %>%
    filter(group=="b") %>%
    lag_group(time_var=year, lag_var=value)


}
