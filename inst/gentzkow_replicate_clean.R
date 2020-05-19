library(plm)
library(lfe)
library(matPkg)
library(multiDiff)
library(tidyverse)

data("GentzkowData")
gent_plm <- pdata.frame(GentzkowData, index = c("cnty90", "styr"))

load("data_raw/GentzkowData.RData")
Gentzkowraw <-  table %>%
  mutate(state_year = paste(st, year, sep="_"),
         cnty90_fac = factor(cnty90),
         styr_fac = factor(styr),
         year_fac = factor(year))


GentzkowData %>%
  count(cnty90)


# Under the common trends assumption, beta estimates a weighted sum of 10378 ATTs.
# 6180 ATTs receive a positive weight, and 4198 receive a negative weight.
# The sum of the negative weights is equal to -.47401318.
# beta is compatible with a DGP where the average of those ATTs is equal to 0,
# while their standard deviation is equal to .00095803.
# beta is compatible with a DGP where those ATTs all are of a different sign than beta,
# while their standard deviation is equal to .00194149.
# file /home/stigler/stata/save_2way_feTR_simple.dta saved
#


################################
#'## Clean data
################################

GentzkowData_c <- GentzkowData %>%
  mutate(state_year = paste(st, year, sep="_"),
         cnty90_fac = factor(cnty90),
         styr_fac = factor(styr),
         year_fac = factor(year))

## there are 683 state-year
GentzkowData_c %>%
  count(styr, state_year)

## there are 16 year
GentzkowData_c %>%
  count(year)

##
683-666

################################
#'## Regressions
################################

stata_FD <- .00259068
stata_FE <- c(-.00113154, -.00112898)

## plm
# plm1 <- plm(changeprestout ~changedailies, data = gent_plm, effect = "twoways")
# plm1

## felm
reg_FE_lfe <- felm(prestout ~numdailies|cnty90 + styr, data = GentzkowData)
reg_FE_lfe

stata_FE

reg_FE_lfe_v2 <- felm(prestout ~numdailies|cnty90_fac + styr_fac + year, data = GentzkowData_c)
reg_FE_lfe_v2
reg_FE_lfe

## super manual
if(FALSE) {
  lm_all <- lm(prestout ~.,
               data = select(Gentzkowraw, prestout, numdailies, matches("styr[0-9]{1,3}"),
                             cnty90_fac, year_fac))

  round(coef(lm_all)["numdailies"], 8)== stata_FE[1]
}


## FD manu
reg_FD_lfe <- felm(changeprestout ~changedailies|styr,
                   data = filter(GentzkowData, mainsample==1))

round(coef(reg_FD_lfe), 8) == stata_FD


## FD
reg_plm_DF <- plm(changeprestout ~changedailies, data = gent_plm, effect = "twoways", model = "fd")

################################
#'## Decomposition
################################

## FE2 is year
data_W_CH <- mDid_weights_CH(data=GentzkowData,
                             y_var="prestout",
                             time.index = "year",
                             treat = "numdailies",
                             unit.index="cnty90", return_details = TRUE)

## FE2 is state-year
data_W_CH_styr <- mDid_weights_CH(data=GentzkowData,
                                  y_var="prestout",
                                  time.index = "styr",
                                  treat = "numdailies",
                                  unit.index="cnty90", return_details = TRUE)

data_W_CH_styr_c <- data_W_CH_styr %>%
  as_tibble() %>%
  left_join(GentzkowData %>%
              select(cnty90, year, styr), by = c("styr", "cnty90"))

data_W_CH_both <- data_W_CH %>%
  as_tibble() %>%
  mutate(type = "year") %>%
  bind_rows(data_W_CH_styr_c %>%
              mutate(type = "styr"))
  # full_join(data_W_CH_styr_c, by = c("numdailies", "year", "cnty90"))

data_W_CH_both

data_W_CH_both %>%
  group_by(type) %>%
  summarise_at(vars(starts_with("wei")), list(non_ne = ~sum(.!=0),
                                              n_pos = ~sum(.>0),
                                              n_neg = ~sum(.<0),
                                              w_neg = ~sum(.[.<0]))) %>%
  as.data.frame()

data_W_CH #  6180+4198: as STATA
data_W_CH_styr#6206+4172: different!!

data_W_CH_styr %>%
  filter(numdailies>0) %>%
  count(sign(weight))

data_W_CH_styr_c %>%
  filter(numdailies>0) %>%
  filter(weight==0)

GentzkowData %>%
  filter(cnty90==10003)

GentzkowData %>%
  filter(st==10)

################################
#'## Investigate
################################

numdailies_count <- GentzkowData %>%
  count(numdailies)

GentzkowData %>%
  filter(changedailies!=0)
sum(GentzkowData$changedailies!=0, na.rm=TRUE)

numdailies_count %>%
  filter(numdailies!=0) %>%
  mat_add_total_row() %>%
  tail

## get weights!?
reg_W_lfe <- felm(numdailies~1|cnty90 + year, data = GentzkowData)

my_res <- GentzkowData %>%
  select(cnty90,  year, numdailies) %>%
  mutate(resids = residuals(reg_W_lfe)[,1],
         D_1 = numdailies!=0) %>%
  rename(G = cnty90, T = year)

## count total: STATA SAYS 10378. OK!
my_res %>%
  count(D_1)

## count pos/neg: STATA SAYS 6180/4198
my_res %>%
  filter(D_1) %>%
  count(sign(resids))


## ave residual
denom <- my_res %>%
  filter(D_1) %>%
  summarise(resid_mean =mean(resids))

## Try add weights
comp_weights <- function(df) {
  if(!all(c("D", "resids") %in% colnames(df))) stop("Probs")
  N_D <- sum(df$D)
  df %>%
    mutate(D_1 = D!=0,
           nat_weight_mat = D /N_D,
           ave_resid_D1 = weighted.mean(resids[D_1], w = nat_weight_mat[D_1]),
           weight_mat = nat_weight_mat * resids /ave_resid_D1)
}

N_D_1 <- sum(my_res$D_1)
N_D_x <- sum(my_res$numdailies)
my_res2 <- my_res %>%
  mutate(D= numdailies) %>%
  comp_weights()

## merge
res_both <- my_res2 %>%
  full_join(res_reg, by = c("G", "T"))

res_both %>%
  filter(abs(weight_mat-weight)>0.00000001)


## weird problem: some have weight zero yet movement in variables!? Was hapening with controls!
res_both %>%
  filter(D_1 & weight==0)

################################
#'## Play with weights
################################

my_res2 %>%
  filter(D_1) %>%
  group_by(T) %>%
  summarise(weight_mat = sum(weight_mat)) %>%
  ungroup() %>%
  ggplot(aes(x = T, y=weight_mat))+
  geom_line()

my_res2 %>%
  filter(D_1) %>%
  group_by(G) %>%
  summarise(weight_mat = sum(weight_mat)) %>%
  ungroup() %>%
  ggplot(aes(x = log(weight_mat)))+
  geom_density()

################################
#'## Super small example
################################
exam_df <- tibble(G = rep(c(1, 2), each=3),
                  T = rep(1:3, 2),
                  D = c(0, 0, 1, 0, 1, 1))
exam_reg <- felm(D~1|G+T, data=exam_df)

exam_df_plus <- exam_df %>%
  mutate(resids = if_else(D==1, residuals(exam_reg)[,1], 0)) %>%
  comp_weights()
exam_df_plus
