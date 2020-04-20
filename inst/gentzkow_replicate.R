ls_now <- ls()
load("/home/matifou/gitReps/my_github/multiDiff/GentzkowData.RData")
res_reg <- haven::read_dta("/home/matifou/Downloads/dataverse_files_gentzkow/save_2way_feTR_simple.dta")
table
ls_now2 <- ls()
ls_now2[!ls_now2%in%ls_now]


https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/EGMLQG/FWKRG5&version=2.0


Reed, W. Robert, 2019, "Replication Data for: "GUEST BLOG at TRN: EIR - Heterogeneity in Two-Way Fixed Effects Models"", https://doi.org/10.7910/DVN/EGMLQG, Harvard Dataverse, V2, UNF:6:r9g/aJRpVkv2rxw3F3J7QQ== [fileUNF]


library(plm)
library(lfe)
library(matPkg)
library(tidyverse)
gent_plm <- pdata.frame(table, index = c("cnty90", "styr"))

table %>%
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

table_c <- table %>%
  mutate(state_year = paste(st, year, sep="_"),
         cnty90_fac = factor(cnty90),
         styr_fac = factor(styr),
         year_fac = factor(year))

## there are 683 state-year
table_c %>%
  count(styr, state_year)

## there are 16 year
table_c %>%
  count(year)

##
683-666

################################
#'## Regressions
################################

stata_FD <- .00259068
stata_FE <- c(-.00113154, -.00112898)

## plm
plm1 <- plm(changeprestout ~changedailies, data = gent_plm, effect = "twoways")
plm1

## felm
reg_FE_lfe <- felm(prestout ~numdailies|cnty90 + styr, data = table)
reg_FE_lfe

stata_FE

reg_FE_lfe_v2 <- felm(prestout ~numdailies|cnty90_fac + styr_fac + year, data = table_c)
reg_FE_lfe_v2
reg_FE_lfe

## super manual
lm_all <- lm(prestout ~.,
               data = select(table_c, prestout, numdailies, matches("styr[0-9]{1,3}"),
                             cnty90_fac, year_fac))

round(coef(lm_all)["numdailies"], 8)== stata_FE[1]



## FD manu
reg_FD_lfe <- felm(changeprestout ~changedailies|styr,
                   data = filter(table, mainsample==1))

round(coef(reg_FD_lfe), 8) == stata_FD


## FD
reg_plm_DF <- plm(changeprestout ~changedailies, data = gent_plm, effect = "twoways", model = "fd")

################################
#'## Investigate
################################

numdailies_count <- table %>%
  count(numdailies)

table %>%
  filter(changedailies!=0)
sum(table$changedailies!=0, na.rm=TRUE)

numdailies_count %>%
  filter(numdailies!=0) %>%
  mat_add_total_row() %>%
  tail

## get weights!?
reg_W_lfe <- felm(numdailies~1|cnty90 + year, data = table)

my_res <- table %>%
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
