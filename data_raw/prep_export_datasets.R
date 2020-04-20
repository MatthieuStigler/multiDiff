library(tidyverse)
library(usethis)

#############################
## Import
#############################

load("data_raw/GentzkowData.RData")


#############################
## Prepare
#############################

## Downloaded form Harvard Dataverse.
# Reed, W. Robert, 2019, "Replication Data for: "GUEST BLOG at TRN: EIR - Heterogeneity in Two-Way Fixed Effects Models"", https://doi.org/10.7910/DVN/EGMLQG, Harvard Dataverse, V2, UNF:6:r9g/aJRpVkv2rxw3F3J7QQ== [fileUNF]
GentzkowData <-  table %>%
  select(-matches("^st[0-9]+"), -matches("^styr[0-9]+"))
# rename(state=st, state_year = styr)



#############################
## export
#############################


use_data(GentzkowData, overwrite = TRUE)
