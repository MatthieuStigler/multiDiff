# multiDiff 0.2.2.90000

* Add new `mdd_synthdid` for synthetic diff-diff
* Internal: add testthat snapshots

# multiDiff 0.2.2 (2023-02-06)

* Use now internal *my_wald* for event study test
* fix warning with missing var

# multiDiff 0.2.1

* fix issue#12: minor bug with weird data

# multiDiff xxxx

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
