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
  