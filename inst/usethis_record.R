

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

## for the example
use_roxygen_md()

use_readme_md()


use_git()

use_mit_license("multiDiff")

use_build_ignore("data_raw", escape = TRUE)
