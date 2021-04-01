# crssplot 0.0.1

*Released April 1, 2021*

Initial beta version. Main user functions are:

- create_results_package("path/to/yml/file.yml") - this creates the "stanard" CRSS figures
- `scens_plot_*()` to create figures that compare scenarios
  - fill in * with `probs`, `range`, `cloud`, or `boxplot`
- `var_plot_trace_scatter()` scatter plot for single variable
- `vars_plot_*` to create figures that compare variables
  - fill in * with `heatmap` or `probs`
  