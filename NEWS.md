# crssplot 0.0.2.9001

- Added `connect_historical` parameters to `scens_plot_cloud()`. When this parameter is `TRUE`, then the historical data and projected data are "connected" for plotting purposes. 
- Fixed bug in the "publish" portion that was preventing `create_results_package()` to run properly when there were more than one plot_groups without the publish key. 
- Fixed `scens_plot_cloud()` so that "Historical" is always the last entry in the legend.
- `scens_plot_*()` functions modified so that `scenarios` argument sets the order that scenarios show up in legend.
- `scens_plot_range()` and `scens_plot_cloud()` updated so that legends always show up in the same order.
- Updated historical Powell and Mead data used by `create_results_package()`

# crssplot 0.0.2

*Released April 25, 2021*

- Added initial working "publish" feature that will create PowerPoint file containing specific results that are routinely published, i.e., provided to stakeholders. 
- Updated Mead and Powell historical elevation data that are used by the cloud plots in the "standard results".

# crssplot 0.0.1

*Released April 1, 2021*

Initial beta version. Main user functions are:

- create_results_package("path/to/yml/file.yml") - this creates the "stanard" CRSS figures
- `scens_plot_*()` to create figures that compare scenarios
  - fill in * with `probs`, `range`, `cloud`, or `boxplot`
- `var_plot_trace_scatter()` scatter plot for single variable
- `vars_plot_*` to create figures that compare variables
  - fill in * with `heatmap` or `probs`
  