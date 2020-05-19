library(tidyverse)
library(haven)

data("GentzkowData")
stata_year <- read_dta("inst/stat_output/outputStata_2way_feTR_simple.dta")
stata_styr <- read_dta("inst/stat_output/outputStata_2way_feTR_styr.dta")
stata_cntrl <- read_dta("inst/stat_output/outputStata_2way_feTR_controls.dta")

range(stata_styr$T)

stata_styr_c <- stata_styr %>%
  left_join(GentzkowData %>%
              select(cnty90, year, styr), by = c("T"="styr", "G"="cnty90")) %>%
  rename(styr=T, T=year)

## assemble
stat_all <- rbind(stata_year %>%
                    mutate(type = "year"),
                  stata_styr_c %>%
                    mutate(type = "styr") %>%
                    select(-styr),
                  stata_cntrl %>%
                    mutate(type = "weird"))

stat_all_w <- stat_all %>%
  pivot_wider(names_from = "type", values_from = c("nat_weight", "W", "weight"))

stat_all %>%
  group_by(type) %>%
  summarise_at(vars(starts_with("wei")), list(non_ne = ~sum(.!=0),
                                              n_pos = ~sum(.>0),
                                              n_neg = ~sum(.<0),
                                              w_neg = ~sum(.[.<0]))) %>%
  as.data.frame()
