# multiDiff 0.3.7

Published on 2024-10-17.

* Create/Improve `coef`, `vcov` and `tidy` for CF estimators

# multiDiff 0.3.6

Published on 2024-10-15.

* Add new wrapper `mdd_estim_fect()` for the `fect` package

# multiDiff 0.3.5

Published on 2024-10-14.

* Add new wrapper `mdd_estim_CH()` for Chaisemartin Haultfeuille

# multiDiff 0.3.4

Published on 2024-10-07.

* First extract: Add unexported multiDiff:::extract.gsynth
* Suggests texreg
* Fix issues #25 and #26

# multiDiff 0.3.3

Published on 2024-10-03.

* Minor: add argument `panel` to `mdd_CS`

# multiDiff 0.3.2

Published on 2024-09-30.

* Add new `mdd_gsynth` to run the Generalized Synthetic Control Method (Xu 2017)

# multiDiff 0.3.1

Published on 2024-09-22.

* Add new `mdd_CS_manu` to replicate internally Callaway and Sant'Anna (2021)

# multiDiff 0.3.0

Published on 2023-10-10.

* Add new `mdd_CS` to use Callaway and Sant'Anna (2021)

# multiDiff 0.2.9

Published on 2023-10-13.

* `sim_dat_common`: add dynamic effects with new argument `beta_dyn`
* Handle case of cross-sectional regressions

# multiDiff 0.2.8

Published on 2023-09-27.

* Add new argument `as_mdd` in `sim_dat_*` functions
* Add/clean internal functions: Correct `intrnl_add_treat_time_mdd`, add arg `keep_mdd` in `intrnl_add_treat_status_mdd`, add `internal intrnl_back_to_mdd`
* Add new testthat for utilities
* Add explanation in help file on ES coefficient interpretation


# multiDiff 0.2.7

Published on 2023-08-23.

* Add new function `mdd_DD_means22` which will compute separate 2x2 means


# multiDiff 0.2.6

Published on 2023-08-19.

* Add argument `by_treat_period` in `mdd_group_means`, cf #17 https://github.com/MatthieuStigler/multiDiff/issues/17
* Small fixes in documentation

# multiDiff 0.2.5

Published on 2023-08-03.

* Add `coef` and `tidy` methods for `mdd_synthdid`

# multiDiff 0.2.4

* Return mdd_dat_slot with `mdd_DD_simple`

# multiDiff 0.2.3

* Add new `mdd_test_placebo` to run a placebo test on the pre-period
* Add new `mdd_synthdid` for synthetic diff-diff
* `mdd_group_means`: add argument `weights`
* Internal: add testthat snapshots

# multiDiff 0.2.2 

Published on 2023-02-06.

* Use now internal *my_wald* for event study test
* fix warning with missing var

# multiDiff 0.2.1

* fix issue#12: minor bug with weird data


* Add function `mdd_data_format`, with `plot` and `print` methods
* Add function `sim_dat_common`
* Add function `mdd_test_pre_trend_means`
* Add function `mdd_test_pre_trend_event`
* Add function `mdd_group_means`
* Pass argument `conf.int` to `plot(mdd_dat)`

# multiDiff 0.1.3

* Add function `mdd_DD_simple`
* Add function `mdd_event_study`

# multiDiff 0.1.2

* Add function `test_pre_trend` to test for parallel trends
* Fix integer issue with `lag_group` (issue #10)

# multiDiff 0.1.1

* New function `FE_decompo()`: Gibbons et al. weights for fixed effects.
* Function `lag_group()` now handles multiple variables and lags, and correclty handles NAs.

# multiDiff 0.1.0

* Main functions: `DD` , `DD_manu()`.
* Added a `NEWS.md` file to track changes to the package.
