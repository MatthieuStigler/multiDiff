
#' Overwrite seq with value from given period (default to max)
#'
#' @param df data
#' @param time.val Time period to filter for. Default to max
#' @param time.index,unit.index usual suspects
#' @param treat_seq_var The variable with treatment
#' @noRd
#' @examples
#' dat <- sim_dat(Time =4, N=3, seed=1234)[, c("unit", "Time", "tr", "lag_1")]
#' dat$seq = paste(dat$lag_1, dat$tr, sep="_")
#' dat
#' multiDiff:::overwrite_seq(dat, treat_seq_var="seq")
#' multiDiff:::overwrite_seq(dat, time.val = 3, treat_seq_var="seq")

overwrite_seq <- function(df, time.val=NULL, time.index = "Time", treat_seq_var = "tr", unit.index="unit"){

  if(is.null(time.val)) time.val <- max(df %>% pull(!!enquo(time.index)))

  df %>%
    select(-!!enquo(treat_seq_var)) %>%
    left_join(df %>%
                filter_at(vars(!!time.index), all_vars(. ==time.val)) %>%
                select(!!enquo(unit.index), !!enquo(treat_seq_var)), by = unit.index)
}


# time_var=quo(year)
# lag_var=quo(value)

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

#' Lag by group
#'
#' @param df data frame
#' @param group_var group_var
#' @param time_var time_var
#' @param lag_var lag_var
#' @param lagamount lag
#' @export
#' @examples
#' df_test <- data.frame(group = rep(c("a", "b"), each=6),
#' year = rep(2000:2005, 2),
#' value = (0:11) ^ 2)
#' lag_group(df_test, group, time_var=year, lag_var=value)

lag_group <- function(df, group_var, time_var, lag_var, lagamount=1){
  cl <- class(pull(head(df, 1), !!enquo(lag_var)))

  df %>%
    group_by(!!enquo(group_var)) %>%
    mutate(lag = ifelse(!!enquo(time_var) - dplyr::lag(!!enquo(time_var), lagamount) == lagamount,
                             dplyr::lag(!!enquo(lag_var), lagamount),
                             NA_real_)) %>%
    ungroup() %>%
    filter(!is.na(!!enquo(lag_var)))
}

## TEST
if(FALSE){
  dplyr::lag(1:4, order_by = c(1,2,3,4))
  dplyr::lag(1:4, order_by = c(1,2,3,5))

}


if(FALSE) {

  df_test <- tibble(group = rep(c("a", "b"), each=6),
                        year = rep(2000:2005, 2),
                        value = (0:11) ^ 2)

  df_test[4,3] <- NA

  df_test %>%
    filter(!is.na(value)) %>%
    lag_group(time_var=year, lag_var=value, group_var=group)

  df_test %>%
    filter(!is.na(value)) %>%
    filter(group=="a") %>%
    lag_group2(time_var=year, lag_var=value)

  df_test %>%
    filter(!is.na(value)) %>%
    filter(group=="b") %>%
    lag_group(time_var=year, lag_var=value)


}
