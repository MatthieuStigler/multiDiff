library(multiDiff)
# library(tidyverse, warn.conflicts=FALSE)
library(dplyr)
library(lfe)


### DATA SIMUL ####
set.seed(123)
data_sim <- sim_dat(N=500, Time = 15) %>%
  mutate(state = cut(unit, 10,
                     labels = paste("state", letters[1:10], sep = "_"))) %>%
  group_by(unit) %>%
  sample_n(size = sample(c(14, 15))) %>%
  ungroup() %>%
  mutate(x = rnorm(n()))

data_sim %>%
  count(unit) %>%
  count(n)


################################
#'## Estimate FELM
################################

reg_FE1_time <- felm(y ~tr|Time, data = data_sim)
reg_FE1_unit <- felm(y ~tr|unit, data = data_sim)
reg_FE2 <- felm(y ~tr|unit+Time, data = data_sim)
reg_FE2_state_Time <- felm(y ~tr|state+Time, data = data_sim)

## covars
reg_FE1_time_X <- felm(y ~tr + x|Time, data = data_sim)
reg_FE1_unit_X <- felm(y ~tr + x|unit, data = data_sim)


################################
#'## Now with FE_decomp
################################

intrnl_ave <-  function(df) with(df, weighted.mean(treat_coef, treat_weight))
intrnl_check <-  function(df, reg) {
  me <- with(df, weighted.mean(treat_coef, treat_weight))
  all.equal(me, coef(reg)[1], check.attributes=FALSE)
}

## TEST  FE1
coefs_FE1_byY <- FE_decompo(data=data_sim,
                         time.index = "Time",
                         fixed_effects = "time",
                         by = "Time")
intrnl_check(coefs_FE1_byY, reg_FE1_time)

coefs_FE1_byN <- FE_decompo(data=data_sim,
                         time.index = "Time",
                         fixed_effects = "unit",
                         by = c("unit", "state"))
intrnl_check(coefs_FE1_byN, reg_FE1_unit)

## TEST FE2, by one var
coefs_FE2_byY <- FE_decompo(data=data_sim,
                         time.index = "Time",
                         fixed_effects = "both",
                         by = "Time")
coefs_FE2_byN <- FE_decompo(data=data_sim,
                            time.index = "Time",
                            fixed_effects = "both",
                            by = "unit")
intrnl_check(coefs_FE2_byY, reg_FE2)
intrnl_check(coefs_FE2_byN, reg_FE2)

## TEST FE1, higher order
coefs_FE1_byS <- FE_decompo(data=data_sim,
                            time.index = "Time",
                            fixed_effects = "unit",
                            by = c("state"))
intrnl_check(coefs_FE1_byS, reg_FE1_unit)

## Compare by state versus average by unit
compr <- coefs_FE1_byN %>%
  group_by(state) %>%
  summarise(treat_coef_byS = weighted.mean(treat_coef, w = treat_weight),
            treat_weight_byS = sum(treat_weight),
            treat_var_byS = weighted.mean(treat_var, w = n_vals),
            treat_var_byS2 = mean(treat_var),
            n_vals_byS  = sum(n_vals)) %>%
  ungroup() %>%
  left_join(coefs_FE1_byS, by = "state")

compr

all.equal(compr$treat_coef_byS, compr$treat_coef)
all.equal(compr$n_vals_byS, compr$n_vals)
all.equal(compr$treat_weight_byS, compr$treat_weight)



################################
#'## 2 vars
################################

coefs_FE1_byN_cov <- FE_decompo(data=data_sim,
                                time.index = "Time",
                                fixed_effects = "unit",
                                by = "unit",
                                covar = "x")
intrnl_check(coefs_FE1_byN_cov, reg_FE1_unit_X)

################################
#'## X contiunous
################################

reg_FE1_time_trX <- felm(y ~x|Time, data = data_sim)
reg_FE1_unit_trX <- felm(y ~x|unit, data = data_sim)

coefs_FE1_byN_trX <- FE_decompo(data=data_sim,
                                treat = "x",
                                time.index = "Time",
                                fixed_effects = "unit",
                                by = "unit")
intrnl_check(coefs_FE1_byN_trX, reg_FE1_unit_trX)

################################
#'## Effect removing a state?
################################

intrnl_check(coefs_FE1_byY, reg_FE1_time)

reg_FE1_time_noY1 <- felm(y ~tr|Time, data = data_sim %>%
                            filter(Time!=1))
intrnl_check(coefs_FE1_byY%>%
               filter(Time!=1), reg_FE1_time_noY1)
