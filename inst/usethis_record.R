

## from: https://github.com/r-lib/usethis

library(usethis)
path <- "/home/matifou/gitReps/my_github/DiffDiff"
# create_package(path)

## DESC
use_description(fields = list(`Authors@R` = 'person(given="Matthieu", family="Stigler", email = "Matthieu.Stigler@gmail.com", role = c("aut", "cre"))',
                              Title = "Multi-year diff-diff and FE decompositions"))
use_mit_license("Matthieu Stigler")

use_package("tidyr", "Imports")
use_package("broom", "Imports")
use_package("dplyr", "Imports")
use_package("lfe", "Imports")
use_package("tibble", "Imports")
use_package("rlang", "Imports")
use_package("stringr", "Imports")
use_package("magrittr", "Imports")
use_package("purrr", "Imports")
use_package("ggplot2", "Imports")
use_package("tidyselect", "Imports")
use_package("data.table", "Imports")
use_package("collapse", "Imports")

use_package("tidyverse", "Suggests")
# tidyselect


## lifecycle
usethis::use_lifecycle()
usethis::use_lifecycle_badge("experimental")

## for the example
use_roxygen_md()

use_readme_md()


use_git()

## version change
# use_version(which = NULL)
# use_dev_version()


use_build_ignore("data_raw", escape = TRUE)

use_news_md()

use_dev_version()
