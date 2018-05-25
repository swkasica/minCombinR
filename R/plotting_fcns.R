#Helper function to check if the input string is one of the valid strings possible
check_valid_str <- function(str_in, valid_options) {
  if (!(str_in %in% valid_options)) {
    stop(paste(str_in, " is not a valid input.
               valid inputs include: ",
               "'", paste(valid_options, collapse = "', '"), "'",
               sep = "")
         )
  }
}

#'Plot a basic chart type
#'
#'See alternative documentation for input and return values
#'See alternative documentation for examples
#'
#'@export
plot_chart <- function(chart_type, data, x_var, y_var, fill, group, value) {
  all_chart_types <-  c("bar_chart", "stacked_bar_chart")
  check_valid_str(chart_type, all_chart_types)
  switch(chart_type,
         #Common Statistical Chart Types
         "bar_chart" = plot_bar_chart(data, x_var, y_var),
         "stacked_bar_chart" = plot_stacked_bar_chart(data, x_var, fill),
         "divergent_bar_chart" = plot_divergent_bar_chart(data),
         "line_chart" = plot_line_chart(data, x_var, y_var),
         "heat_map" = plot_heatmap(data, x_var, y_var, fill),
         "density_plot" = plot_density_chart(data, x_var, y_var),
         "scatter_plot" = plot_scatter(data, x_var, y_var),
         "pie_chart" = plot_pie_chart(data, group, value))
}

#'Combine multiple charts together
#'
#'@export
combine_charts <- function(combo_type, chart1, facet_by) {
  all_combo_types <- c("small_multiples")
  check_valid_str(combo_type, all_combo_types)
  switch(combo_type,
         "small_multiples" = combine_small_multiples(chart1, facet_by))
}
