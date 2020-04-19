intrnl_data_format <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit") {
  if(any(c(".time", ".treat", ".unit") %in% colnames(data))) stop("Data should not contain variables named either,  '.time, '.treat', or '.unit'")
  data %>%
    rename(.time=!!enquo(time.index),
           .treat=!!enquo(treat),
           .unit=!!enquo(unit.index))
}

intrnl_attr_to_quo <- function(data, att, check=FALSE) {
  this_var <- attr(data, att)
  if(check) print(this_var)
  rlang::ensym(this_var)
}

#' Weights from Chaisemartin and Hautefeuille
#'
#'@template param_all
#'@examples
#'examp_df <- data.frame(unit = rep(c(1, 2), each=3),
#'                       time = rep(1:3, 2),
#'                       treat = c(0, 0, 1, 0, 1, 1))
#'mDid_weights_CH(examp_df, treat = "treat",
#'                time.index = "time")
#'@export
mDid_weights_CH <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit") {

  treat_quo <- rlang::ensym(treat)

  ## rename vars data internally
  data2 <- intrnl_data_format(data, y_var= ensym(y_var),
                              time.index = rlang::ensym(time.index),
                              treat = rlang::ensym(treat_quo),
                              unit.index=rlang::ensym(unit.index))

  ## run reg
  reg <- lfe::felm(.treat~1|.unit+.time, data = data2)

  treat_var <- pull(data, {{treat_quo}})
  if(anyNA(treat_var)) warning("Not ready yet for NAs")
  N_D <- sum(treat_var)
  res <- data %>%
    mutate(residuals = stats::residuals(reg)[,1],
           D_1 = treat_quo!=0,
           nat_weight_mat = {{treat_quo}} /N_D,
           ave_resid_D1 = weighted.mean(.data$residuals[.data$D_1],
                                        w = .data$nat_weight_mat[.data$D_1]),
           weight = .data$nat_weight_mat * .data$residuals /.data$ave_resid_D1) %>%
    select(rlang::ensym(treat),
           rlang::ensym(time.index),
           rlang::ensym(unit.index), .data$weight)

  ##
  attr(res, "class") <-  c("FE_weights_CH", "FE_weights", class(res))
  attr(res, "y_var") <-  y_var
  attr(res, "time.index") <- time.index
  attr(res, "unit.index") <-  unit.index
  attr(res, "treat") <-  treat
  res
}

#'@export
#'@param x Output from \code{mDid_weights_CH}
#'@param \ldots Unused arguments
#'@rdname mDid_weights_CH
print.FE_weights_CH <- function(x, ...) {
  smry <- weights_CH_smry(x)
  cat("Under the common trends assumption, beta estimates a weighted sum of",  smry["n_treat"], "ATTs.\n")


  cat(smry["n_pos"], "ATTs receive a positive weight, and",  smry["n_neg"], "receive a negative weight.\n")
  cat("The sum of the negative weights is equal to", smry["w_neg_mean"], ".\n")

  cat("\nData:\n")
  print.data.frame(head(x))
  invisible(x)
}

#'@export
#'@param by Which kind of plot to draw
#'@rdname mDid_weights_CH
plot.FE_weights_CH <- function(x, by = c("year", "unit"), ...) {

  ## prepare vars
  by <- match.arg(by)
  by_index <- switch(by, year="time.index", unit = "unit.index")
  by_var_quo <- intrnl_attr_to_quo(x, by_index)

  ## summarize
  data_mean <- x %>%
    dplyr::group_by_at(attr(x, by_index)) %>%
    summarise(weight = mean(.data$weight)) %>%
    ungroup()

  ##
  if(by =="year") {
    pl <- data_mean %>%
      ggplot2::ggplot(ggplot2::aes(x = {{by_var_quo}}, y = .data$weight)) +
      ggplot2::geom_line()
  } else {
    pl <- data_mean %>%
      ggplot2::ggplot(aes(x = .data$weight)) +
      ggplot2::geom_density()
  }
  pl
}



#'@export
#'@rdname mDid_weights_CH
weights_CH_smry <- function(data) {
  treat_quo <- intrnl_attr_to_quo(data, "treat")

  ## get only treated data
  data_treat <- data %>%
    filter({{treat_quo}}!=0) %>%
    mutate(is_pos = .data$weight>0)

  n_treat <- nrow(data_treat)
  n_pos <- sum(data_treat$is_pos)
  n_neg <- sum(!data_treat$is_pos)
  w_neg_mean <- sum(data_treat$weight[!data_treat$is_pos])
  c(n_treat=n_treat, n_pos=n_pos, n_neg = n_neg, w_neg_mean=w_neg_mean)

}

if(FALSE) {
  # data(GentzkowData)
  library(multiDiff)
  library(tidyverse)
  library(lfe)
  data_W_CH <- mDid_weights_CH(data=GentzkowData,
                               y_var="prestout",
                               time.index = "year",
                               treat = "numdailies",
                               unit.index="cnty90")
  data_W_CH
  plot(data_W_CH, by = "unit")
  plot(data_W_CH, by = "time")

}

