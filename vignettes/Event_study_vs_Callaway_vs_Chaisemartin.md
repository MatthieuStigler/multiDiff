    library(multiDiff)

Preview this file with:
[link](https://htmlpreview.github.io/?https://github.com/MatthieuStigler/multiDiff/blob/master/vignettes/Event_study_vs_Callaway_vs_Chaisemartin.html)

# Comparing DiD, event-study, Callaway Sant’Anna and Chaisemartin and Haultfeuille estimates

In summary, in a *non-staggered* case:

1.  a DiD estimate the average *post* periods against all *pre* periods
2.  An event study (ES) estimates individual *post* periods against a
    single *pre* period
3.  The Callaway Sant’Anna (CS) gives the same estimates as an ES when
    using `base_period = "universal"`
4.  The CS `did::aggte` function gives the average of the *post* periods
    against a single *pre* periods
5.  Therefore, the link between DiD and ES/CS is:

-   a full DiD is equal to the average ES when the ES is computed
    against all *pre* periods (argument `trim_low`)
-   a truncated DiD is equal to the average ES when the DiD’s input
    contains only one *pre* period

1.  The Chaisemartin and Haultfeuille (2023) gives the ES coefficients
    (against the -1 period) BUT:

-   coefficients are rounded to `digits=5`

1.  The CH ATE is therefore equal to the EC/CS coefficients.

## example

Simulate data with common treatment (= non-staggered). Treatment happens
at T=5.

    dat_common <- sim_dat_common(timing_treatment = 5:10, as_mdd = TRUE, seed=123)

Estimate the standard DiD, and a truncated DiD, where we remove all
pre-treatment periods except the last one

    reg_DD <- mdd_DD_simple(dat_common)
    reg_DD_trunc <- mdd_DD_simple(dat_common |>subset(Time>=4))

Estimate the ES coefficents

    reg_ES <- mdd_event_study(dat_common)
    reg_ES_trunc <- mdd_event_study(dat_common, trim_low = -1)

Estimate the CS coefficients

    reg_CS <- mdd_CS(dat_common, base_period = "universal")

Estimate the CH coefficient

    reg_CH_24_effct_1 <- mdd_estim_CH(mdd_dat = dat_common, graph_off=TRUE)
    reg_CH_24_effct_6 <- mdd_estim_CH(mdd_dat = dat_common, effects =6, placebo=3, graph_off=TRUE)

## verify the relationships

### The full DiD is the same as the mean ES which considers all pre-periods (`trim_low = -1`)

    all.equal(coef(reg_DD)[[1]],
              mean(coef(reg_ES_trunc)))
    #> [1] TRUE

### a truncated DiD is the same as the mean ES

    all.equal(coef(reg_DD_trunc)[[1]],
              mean(coef(reg_ES)[4:9]))
    #> [1] TRUE

### a truncated DiD is also the same as the `aggte(CS)`

    all.equal(coef(reg_DD_trunc)[[1]],
              suppressWarnings(did::aggte(reg_CS)$overall.att))
    #> [1] TRUE

### Individual ES are equal to individual CS when using `base_period = "universal"`

    all.equal(coef(reg_ES)[4:9],
              reg_CS$att[5:10], check.attributes = FALSE)
    #> [1] TRUE

### Individual ES are equal to individual CS

    all.equal(round(coef(reg_ES)[4:9],5),
              reg_CH_24_effct_6$results$Effects[,"Estimate"], check.attributes = FALSE)
    #> [1] TRUE
