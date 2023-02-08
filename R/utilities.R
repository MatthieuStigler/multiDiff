## Add sequence as .group variable
add_group <- function(df, time.index = "Time", treat = "tr", unit.index="unit",
                      group_rename_maybe=FALSE){

  ## get 0-1 sequences for each unit
  groups <- get_sequences(df, time.index = {{time.index}},
                          treat = {{treat}}, unit.index = {{unit.index}}) %>%
    rename(.group="seq")

  ## rename eventually
  if(n_distinct(groups$.group)<=2 & group_rename_maybe) {
    groups <- groups %>%
      mutate(.group = case_when(str_detect(.group, "0_1")~"treated",
                                str_detect(.group, "0_0")~"control"))
  }

  ## add tot dat
  # by_var <- {{unit.index}}
  # if(rlang::is_quosure(by_var)) by_var <- rlang::as_name(by_var)
  df %>%
    left_join(groups %>%
                select(all_of(unit.index), ".group"), by = unit.index)
}

# add_treat_group_simple <- function(data, time.index = "Time", treat = "tr", unit.index="unit"){
#
#   data %>%
#     group_by(dplyr::across({{unit.index}})) %>%
#     mutate(treat_group = if_else(any({{treat}}==1), "treated", "control")) %>%
#     ungroup()
# }

if(FALSE){
  DID_dat <- sim_dat_common()
  multiDiff:::get_sequences(DID_dat)
  multiDiff:::add_group(DID_dat)


  time.index = quo("Time")
  treat = quo("tr")

  unit.index_quo = quo("unit")
  a <- add_group(DID_dat, unit.index=unit.index_quo)
  a <- add_group(DID_dat, unit.index="unit")

  add_group(df=DID_dat )|>
    dplyr::count(.group)
}

#' df of sequences 0_0_1
#'@examples
#' get_sequences(sim_dat())
#' get_sequences(sim_dat()[-3,])
#'@noRd
get_sequences <- function(df, time.index = "Time", treat = "tr", unit.index="unit") {
  df %>%
    select(!!enquo(unit.index), !!enquo(time.index), !!enquo(treat)) %>%
    spread(!!enquo(time.index), !!enquo(treat)) %>%
    tidyr::unite("seq", -!!enquo(unit.index))
}

get_sequences_slower <- function(df, time.index = "Time", treat = "tr", unit.index="unit") {
  df %>%
    # rename(.treat={{treat}}) %>%
    rename(.treat=!!rlang::sym(treat)) %>%
    dplyr::arrange_at(c(unit.index, time.index)) %>%
    dplyr::group_by_at(unit.index) %>%
    summarise(seq = paste(.data$.treat, collapse = "_")) %>% #!!enquo(
    ungroup()
}




if(FALSE){
  library(tidyverse)
  library(multiDiff)
  sim_dat() %>%
    get_sequences()
  treat <- "tr"
  sim_dat() %>%
    get_sequences(treat = treat )

  ## with NA
  dat_NA <- sim_dat()[-3,]
  get_sequences(dat_NA)
  get_sequences_2(dat_NA)

  ## benchmark
  dat <- sim_dat() %>%
    slice(sample(1:nrow(.), size = nrow(.)))
  microbenchmark(smry = get_sequences(dat),
                 sprd = get_sequences_2(dat),
                 times = 50, check="equal")
  ## with NA
}


##
#'@examples
#' library(multiDiff)
#' dat <- sim_dat()
#' intrnl_is_balanced_col(dat)
#' intrnl_is_balanced_col(dat[-3,])
#' @noRd
intrnl_is_balanced_dplyr <- function(df, unit.index="unit") {
  co <- count(df, !!rlang::sym(unit.index)) %>%
    count(n)
  if(nrow(co)>1) FALSE  else TRUE
}

intrnl_is_balanced_col <- function(df, unit.index="unit") {
  collapse::fNdistinct(collapse::fNobs(df[[unit.index]],
                                       g=df[[unit.index]]))==1
}

intrnl_is_balanced <- function(df, unit.index="unit") {
  collapse::fNdistinct(collapse::fNdistinct(df[[unit.index]],
                                            g=df[[unit.index]]))==1
}

if(FALSE){
  library(microbenchmark)
  microbenchmark(dp = intrnl_is_balanced_dplyr(dat[-3,]),
                 col=intrnl_is_balanced(dat[-3,]),
                 col2=intrnl_is_balanced_col2(dat[-3,]))
}


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





################################
#'## Smal utilities add group etc
################################

#' rename/format data
#' @noRd
intrnl_dat_rename <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){

  data %>%
    dplyr::rename("treat" ={{treat}},
                  "time.index" = {{time.index}},
                  "unit.index" = {{unit.index}},
                  "y_var" = {{y_var}})
}

#' Add category
#' @noRd
intrnl_add_treat_status <- function(data) { #}, treat = "tr", unit.index="unit"){
                                    # y_var="y", time.index = "Time"

  data %>%
    group_by(.data$unit.index) %>%
    mutate(treat_categ = if_else(any(.data$treat==1), "Treat", "Control")) %>%
    ungroup()
}

#' Add category, but for mfdd object
#' @noRd
intrnl_add_treat_status_mdd <- function(mdd_dat) {

  mdd_vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names
  treat_quo <- rlang::sym(mdd_vars$treat)
  mdd_dat %>%
    group_by(across(mdd_vars$unit.index)) %>%
    mutate(treat_categ = if_else(any(!!treat_quo==1), "Treat", "Control")) %>%
    ungroup()
}

#' @noRd
intrnl_add_time_to_treat <- function(data) { #}, treat = "tr", unit.index="unit"){
  # y_var="y", time.index = "Time"

  data %>%
    group_by(.data$unit.index) %>%
    mutate(treat_categ = if_else(any(.data$treat==1), "Treat", "Control")) %>%
    ungroup()
}


#' Add category
#' @noRd
intrnl_add_treat_time <- function(data){
  data %>%
    group_by(.data$unit.index) %>%
    mutate(treat_timing = first_1(.data$treat)) %>%
    ungroup()
}

first_1 <- function(x) {
  w <- which(x==1)
  res <- rep(0, length(x))
  if(length(w)>0) {
    res[w[1]] <- 1
  }
  res
}

x_time_to_treat <- function(x, trim_low= NULL, trim_high=NULL) {
  w <- which(x==1)
  if(length(w)>0) {
    res <- seq_along(x)-w[1]
    if(!is.null(trim_low)) res[res<trim_low] <- trim_low # or pmax(res, trim_low)
    if(!is.null(trim_high)) res[res>trim_high] <- trim_high # or pmin(res, trim_high)
  } else {
    res <- rep(0, length(x))
  }
  res
}

if(FALSE){
  dt <- tibble(unit = rep(c("A", "B"), each=3),
               tr = c(0,0,0, 0,1,1), Time =NA, y=NA)
  first_1(x=dt$tr)
  dt %>%
    intrnl_dat_rename() %>%
    intrnl_add_treat_time()

  x_time_to_treat(rep(0,5))
  x_time_to_treat(rep(1,5))
  x_time_to_treat(c(0,0, 1,0,0))

  ## trim
  x_time_to_treat(c(0,0, 0, 1,0,0, 0))
  x_time_to_treat(c(0,0, 0, 1,0,0, 0), trim_low = -2, trim_high = 2)

}

################################
#'## Mdd internal functions
################################

#' Get internal slot
#'@noRd
intrnl_mdd_get_mdd_slot <- function(mdd_dat_any){
  if("mdd_dat_slot" %in% names(attributes(mdd_dat_any))) {
    res <- attributes(mdd_dat_any)$mdd_dat_slot
  } else if ("mdd_dat_slot" %in% names(mdd_dat_any)) {
    res <- mdd_dat_any$mdd_dat_slot
  } else {
    stop("object contains no slot/attribute mdd_dat_slot!?")
  }
    res
}

intrnl_mdd_get_pre_periods <- function(mdd_dat_any){

  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat_any)

  ## res
  res <- mdd_dat_slot$periods[mdd_dat_slot$periods < min(mdd_dat_slot$treated_periods)]

  ## check res
  if(length(res)==0) rlang::warn("Internal confusion in identifying treated errors")

  ##
  res
}
