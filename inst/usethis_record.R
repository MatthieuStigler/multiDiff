

## from: https://github.com/r-lib/usethis

library(usethis)
path <- "/home/matifou/gitReps/my_github/DiffDiff"
# create_package(path)


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

## DESC
use_description(fields = list(`Authors@R` = 'person(given="Matthieu", family="Stigler", email = "Matthieu.Stigler@gmail.com", role = c("aut", "cre"))'))
use_mit_license("Matthieu Stigler")

## for the example
use_roxygen_md()

use_readme_md()


use_git()



use_build_ignore("data_raw", escape = TRUE)
