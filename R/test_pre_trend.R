#' Conduct parallel pre-trends test
#'
#' @template param_mdd_dat
#' @param time_ref the reference time period against which to test, By default, the first pre-period
#' @param cluster argument passed to `feols(..., cluster=)`
#'
#' @examples
#'
#' data <- sim_dat_common(N=100, Time=10,
#'                        timing_treatment = 6:10, perc_treat=0.5)
#'
#' mdd_data <- mdd_data_format(data)
#' mdd_test_pre_trend_means(mdd_dat=mdd_data)
#'
#'@export
mdd_test_pre_trend_means <- function(mdd_dat, cluster=NULL, time_ref = NULL){

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  if(mdd_dat_slot$n_seq>2) stop("Not implemented for more than 2 groups")
  if(mdd_dat_slot$is_reversible) warning("reversible treatment? Taking till first treatment")


  ## prep data
  mdd_dat_add <- mdd_dat %>%
    add_group(time.index = mdd_vars$time.index, treat = mdd_vars$treat,
              unit.index = mdd_vars$unit.index, group_rename_maybe=TRUE) %>%
    rename(treat =".group",
           period = !!sym(mdd_vars$time.index),
           y= !!sym(mdd_vars$y_var)) %>%
    mutate(treat =as.factor(.data$treat),
           period = as.factor(.data$period))

  ## Estimate ES regression
  reg_out <- fixest::feols(y~ -1+period:treat,
                           cluster = cluster,
                           data=mdd_dat_add)

  nam_coef <- names(coef(reg_out))
  K <- length(nam_coef)

  ## get pre-preiods
  pre_periods <- mdd_dat_slot$periods[mdd_dat_slot$periods < min(mdd_dat_slot$treated_periods)]
  pre_periods_num <- seq_along(mdd_dat_slot$periods)[mdd_dat_slot$periods < min(mdd_dat_slot$treated_periods)]

  ## H1: individual pairs
  # H_pairs <- sapply(pre_periods, \(i) paste(nam_coef[c(i,i+(K/2))], collapse =" - "))
  H_pairs <- sapply(pre_periods_num, \(i) paste(nam_coef[c(i,i+(K/2))], collapse =" - "))

  ## internal checkI got it right
  nam_check_1 <- purrr::map(stringr::str_split(H_pairs, " = "), \(str) lapply(stringr::str_split(str, ":"), \(i) i[[1]]) %>% unlist())
  if(!all(map_int(nam_check_1, dplyr::n_distinct)==1)) {
    stop("Problem with creating hypo H1, vector is: ", H_pairs[1])
  }

  ## H2: join tests
  if(is.null(time_ref)) time_ref <- pre_periods[1]
  if(!time_ref %in% pre_periods) {
    rlang::abort(paste("Arg. 'time_ref' not found in identified pre-periods:",
                       paste(pre_periods,collapse = ",")))
  }
  base_ref <- which(str_detect(H_pairs, paste0("period", time_ref, ":")))
  H2_char <- paste(H_pairs[-base_ref],"=",  H_pairs[base_ref])

  test_joint <- car::linearHypothesis(reg_out, hypothesis.matrix = H2_char)
  test_indiv <- purrr::map_dfr(H2_char, \(i) car::linearHypothesis(reg_out, hypothesis.matrix = i) %>%
                                 broom::tidy())

  ## assemble
  rbind(test_joint %>% broom::tidy() %>%
          distinct(.data$statistic, .data$p.value) %>%
          mutate(estimate = NA,
                 term = paste(H2_char, collapse = " AND ")),
        test_indiv %>%
          distinct(.data$estimate, .data$term, .data$statistic, .data$p.value)) %>%
    mutate(test = c("test_joint", rep("test_indiv", nrow(test_indiv)))) %>%
    rename(coefficient = "estimate") %>%
    dplyr::relocate("test", "coefficient")
}

#'@param ... Further arguments passed to \code{\link{mdd_event_study}}
#'@seealso \code{\link{mdd_event_study}} to estimate the event study.
#'@export
#'@rdname mdd_test_pre_trend_means
mdd_test_pre_trend_event <- function(mdd_dat, ...){

  ## mdd formatting
  if(!inherits(mdd_dat, c("mdd_dat", "mdd_event_study"))) {
    stop("Data should be either of class 'mdd_data_format' or 'mdd_event_study'")
  }

  ## to ES if not
  if(!inherits(mdd_dat, c("mdd_event_study"))) {
    mdd_dat <- mdd_event_study(mdd_dat, ...)
  }


  ## format hypo H matrix
  coef_nam <- names(coef(mdd_dat))
  which_before <- stringr::str_detect(coef_nam, "-[0-9]+")
  K_before <- sum(which_before)
  H_mat <- matrix(0, nrow = K_before, ncol = length(coef_nam))
  for(i in 1:K_before) {
    H_mat[i, which(which_before)[i]] <- 1
  }

  ## hypo
  test_joint <- my_wald(mdd_dat, H_mat)
  test_indiv <- purrr::map_dfr(1:K_before, \(i) my_wald(mdd_dat, H_mat[i,,drop=FALSE]))

  ## tidy res  assemble
  rbind(test_joint,
        test_indiv ) %>%
    mutate(test = c("test_joint", rep("test_indiv", nrow(test_indiv)))) %>%
    dplyr::relocate("test", "coefficient") %>%
    as_tibble()

}

if(FALSE){
  library(multiDiff)

  ##
  df <- sim_dat_common(N=100, Time=10,
                       timing_treatment = 6:10, perc_treat=0.5)
  mdd_dat <- mdd_data_format(df)
  plot(mdd_dat)

  ES <- mdd_event_study(mdd_dat)
  ES_omit5 <- mdd_event_study(mdd_dat, time.omit = -5)

  ##
  mdd_test_pre_trend_means(mdd_dat = mdd_dat)

  mdd_test_pre_trend_event(mdd_dat = mdd_dat)
  mdd_test_pre_trend_event(mdd_dat = ES)
  mdd_test_pre_trend_event(mdd_dat = ES_omit5)


  library(fixest)
  library(tidyverse)

  data <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                            timing_treatment = 6, perc_treat=0.5)
  data %>%
    count(Time, tr) %>%
    spread(tr, n)

  DD <- feols(y~tr|unit+Time, cluster = "unit", data=data)
  DD
  data_modif <- data %>%
    group_by(unit) %>%
    mutate(type = if_else(any(tr==1), "Treat", "Control"),
           time_fac = as.factor(Time)) %>%
    select(y, type, tr, Time, unit,time_fac) %>%
    ungroup()

  DD <- feols(y~tr|unit+Time, cluster = "unit", data=data)

  ##
  means_manu <- data_modif %>%
    group_by(Time, type) %>%
    summarise(y = mean(y)) %>%
    arrange(type)
  means_manu%>%
    spread(type, y) %>%
    mutate(diff =Control-Treat)
  cf <- feols(y~-1+time_fac:type, cluster = "unit", data=data_modif) %>%
    coef()
  all.equal(cf, means_manu$y, check.attributes=FALSE)
  -0.0840--0.292
  #
  lm(y~-1+time_fac*type, data=data_modif)

  ## Full means
  lm(y~-1+time_fac:type, data=data_modif)

  ## aov?
  ao <- aov(y~time_fac+type, data=data_modif)
  summary(aov(y~time_fac:type, data=data_modif))
  TukeyHSD(ao, which = "time_fac")

  ### Full
  treat.group.index = quo(type)
  time.index <- quo(Time)
  mdd_test_pre_trend_means(data_modif, cluster=NULL)
}

################################
#'## Wald test
################################

#' Internal Wald test for 0
#'
#' @param object any reg object with coef/vcov
#' @param H Hypo matrix, N col shold be same aas N coef
#'
#' @noRd
my_wald <- function(object, H){


  B <- coef(object)
  R <- H %*% B
  SIG <- H%*% stats::vcov(object) %*% t(H)

  ## Try to inverse
  S_inv <- try(solve(SIG), silent = TRUE)
  if(inherits(S_inv, "try-error")) {
    error <- attributes(S_inv)$condition
    warning(error)
    W <- p <- NA
  } else {
    W <- t(R) %*% solve(SIG) %*% R
    p <- 1 - stats::pchisq(W, df = nrow(H))
  }

  ## term
  nam_B <- names(coef(object))
  terms_sep <- lapply(1:nrow(H), \(i) paste(paste(nam_B[which(H[i,]==1)], collapse = " + "), "= 0"))
  terms <- paste(terms_sep, collapse = " AND ")

  ## estimate
  coefficient <- if(nrow(H)==1)R else NA

  ## res
  data.frame(coefficient= coefficient,
             statistic=W, p.value=p, term= terms)
}

if(FALSE){
  reg  <-  lm(freeny)
  H <- cbind(0, diag(2), matrix(0,2,2))
  H2 <- H
  H2[1,1] <- 1
  H_indiv <- matrix(c(0, 1, 0,0,0), nrow=1)

  ##
  my_wald(object = reg, H)
  my_wald(object = reg, H2)
  my_wald(object = reg, H_indiv)

  aod_res <- aod::wald.test(Sigma = vcov(reg), b = coef(reg), Terms = 2:3)
  my_res <- my_wald(object = reg, H)
  all.equal(aod_res$result$chi2[["chi2"]], my_res[["statistic"]])
  all.equal(aod_res$result$chi2[["P"]], my_res[["p.value"]])

  ## test individual
  my_wald(reg, H_indiv)
  all.equal(my_wald(reg, H_indiv)[["statistic"]] |>sqrt(),
            coef(summary(reg))[2,"t value"])

  car::linearHypothesis(reg, H) |> broom::tidy()
}
