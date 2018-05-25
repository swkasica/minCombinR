#Helper function to check if the input string, chart_type, is one of the valid chart types possible
check_valid_chart_type <- function(chart_type) {
  all_chart_types <-  c("bar_chart", "stacked_bar_chart")
  if (!(chart_type %in% all_chart_types)) {
    stop(paste(chart_type, " is not a valid chart_type.
               chart_type can be one of: ",
               "'", paste(all_chart_types, collapse = "', '"), "'",
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
  check_valid_chart_type(chart_type)
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
