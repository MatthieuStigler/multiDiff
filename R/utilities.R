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
