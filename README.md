
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiDiff

This R package implements the multi-period diff-diff as discussed in
Imai and Kim (2019) and Chaisemartin and Hautfeuille (2019).

## Installation

``` r
devtools::install_github("MatthieuStigler/multiDiff")
```

## Example

Main function is `DD()` (name might change\!), with following arguments

  - `y_var` The name of the y variable
  - `treat treatment` The name of the treatment variable
  - `time.index` The name of the *time* dimension variable
  - `unit.index` The name of the *unit* dimension variable
  - `data` The dataset

<!-- end list -->

``` r
library(multiDiff)
suppressMessages(library(tidyverse))
data <- sim_dat(N=100)
DD_out <- DD(data=data)

## Main output is:
knitr::kable(DD_out[1:5,])
```

| time | DiD | treat | control | n\_treat | n\_control | n\_min | miss\_data |  estimate | std.error | statistic |   p.value |    conf.low | conf.high |
| ---: | --: | :---- | :------ | -------: | ---------: | -----: | :--------- | --------: | --------: | --------: | --------: | ----------: | --------: |
|    2 |   1 | 0\_1  | 0\_0    |       22 |         51 |     22 | FALSE      | 1.3412249 | 0.3803347 | 3.5264329 | 0.0007425 |   0.5828590 |  2.099591 |
|    2 |   2 | 0\_1  | 1\_1    |       22 |          9 |      9 | FALSE      | 0.3074666 | 0.5228332 | 0.5880778 | 0.5610323 | \-0.7618474 |  1.376781 |
|    2 |   3 | 1\_0  | 0\_0    |       18 |         51 |     18 | FALSE      | 1.5262981 | 0.4282738 | 3.5638370 | 0.0006792 |   0.6714600 |  2.381136 |
|    2 |   4 | 1\_0  | 1\_1    |       18 |          9 |      9 | FALSE      | 2.5600564 | 0.6125043 | 4.1796545 | 0.0003120 |   1.2985801 |  3.821533 |
|    3 |   1 | 0\_1  | 0\_0    |       39 |        103 |     39 | FALSE      | 0.4408649 | 0.4410531 | 0.9995733 | 0.3211135 | \-0.4394808 |  1.321211 |

Plot the year-by-year treatment:

``` r
DD_out %>% 
  filter(DiD %in% c(1,4)) %>% 
  mutate(case = paste("Treat: ", treat, ", Control: ", control, sep = "")) %>% 
  ggplot(aes(x=time, y = estimate, color = case)) +
  geom_ribbon(aes(ymin = .data$conf.low, 
                    ymax = .data$conf.high,
                  group = case),
              fill = "grey", alpha = I(0.4),
              linetype = 2) +
  geom_line(size = 1) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept =1, linetype = 2) +
  ggtitle("Diff-diff by year and identification case")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Aggregate the results over time:

``` r
DiD_aggreg(x=DD_out, by_DiD = FALSE)
#> # A tibble: 1 x 1
#>   estimate
#>      <dbl>
#> 1    0.951
DiD_aggreg(x=DD_out, by_DiD = TRUE)
#> # A tibble: 2 x 4
#>     DiD treat control estimate
#>   <int> <chr> <chr>      <dbl>
#> 1     1 0_1   0_0        0.886
#> 2     4 1_0   1_1        1.01
```
