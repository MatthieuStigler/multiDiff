
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multiDiff

This R package implements the multi-period diff-diff as discussed in
Imai and Kim (2019) and Chaisemartin and Hautfeuille (2019).

## Installation

``` r
devtools::install_github("MatthieuStigler/multiDiff")
```

## Main functions:

  - `DD()`: period-by-period diff-diff (DiD)
  - `DD_manu()`: DiD the manual way: showing the four means
  - `DD_manu_many()`: period-by-period manual DiD
  - `FE_decompo()`: FE decomposition following Gibbons et al
  - `mDid_weights_CH()`: Weights from Chaisemartin and Hautefeuille
    (2020)

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

| time | DiD | treat | control | n\_treat | n\_control | n\_min | miss\_data |  estimate | std.error | statistic |   p.value |    conf.low | conf.high |    D\_var | n\_vals |
| ---: | --: | :---- | :------ | -------: | ---------: | -----: | :--------- | --------: | --------: | --------: | --------: | ----------: | --------: | --------: | ------: |
|    2 |   1 | 0\_1  | 0\_0    |       17 |         59 |     17 | FALSE      | 1.1809653 | 0.3791100 |  3.115099 | 0.0026162 |   0.4255721 |  1.936359 | 0.0999913 |     152 |
|    2 |   2 | 0\_1  | 1\_1    |       17 |          7 |      7 | FALSE      | 0.6139603 | 0.5640646 |  1.088457 | 0.2881738 | \-0.5558381 |  1.783759 | 0.2335993 |      48 |
|    2 |   3 | 1\_0  | 0\_0    |       17 |         59 |     17 | FALSE      | 1.2824509 | 0.3740449 |  3.428602 | 0.0009947 |   0.5371503 |  2.027752 | 0.0999913 |     152 |
|    2 |   4 | 1\_0  | 1\_1    |       17 |          7 |      7 | FALSE      | 1.8494559 | 0.5329308 |  3.470349 | 0.0021732 |   0.7442250 |  2.954687 | 0.2335993 |      48 |
|    3 |   1 | 0\_1  | 0\_0    |       21 |         55 |     21 | FALSE      | 1.5187666 | 0.3661128 |  4.148357 | 0.0000884 |   0.7892710 |  2.248262 | 0.1198588 |     152 |

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
#> 1    0.983
DiD_aggreg(x=DD_out, by_DiD = TRUE)
#> # A tibble: 2 x 4
#>     DiD treat control estimate
#>   <int> <chr> <chr>      <dbl>
#> 1     1 0_1   0_0        0.877
#> 2     4 1_0   1_1        1.09
```
