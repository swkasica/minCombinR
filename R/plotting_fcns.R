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

#'Plot a simple chart type
#'
#'This function will create a single chart object that can be passed into render_charts() to render.
#'
#'See alternative documentation for input and return values
#'See alternative documentation for examples
#'
#'@export
plot_simple <- function(chart_type, data, x_var, y_var, facet_by, fill, group, value) {
  all_chart_types <-  c("bar",
                        "stacked_bar",
                        "divergent_bar",
                        "line",
                        "heat_map",
                        "density",
                        "scatter",
                        "pie",
                        "choropleth")
  check_valid_str(chart_type, all_chart_types)
  switch(chart_type,
         #Common Statistical Chart Types
         "bar" = plot_bar_chart(data, x_var, y_var, facet_by),
         "stacked_bar" = plot_stacked_bar_chart(data, x_var, fill, facet_by),
         "divergent_bar" = plot_divergent_bar_chart(data, facet_by),
         "line" = plot_line_chart(data, x_var, y_var, group,facet_by),
         "heat_map" = plot_heatmap(data, facet_by),
         "density" = plot_density_chart(data, x_var, y_var, facet_by),
         "scatter" = plot_scatter(data, x_var, y_var, facet_by),
         "pie" = plot_pie_chart(data, group, facet_by)

         #Chart types to try some data sets
         )
}

#'Many types general plot
#'
#'@export
plot_many_types_general <- function(...) {
  args_list <- list(...)
  lapply(args_list, function(x) {do.call(plot_simple, x)})
}

#'Render charts --> We have called this combine_charts but maybe render_charts is a better name?
#'
#'@export
render_charts <- function(charts_list) {

  charts_list <- lapply(charts_list, function(x) {
    if('gg' %in% class(x)) {
      ggplotify::as.grob(x)
    }else {
      ggplotify::as.grob(x[[1]])
    }
  })
  cowplot::plot_grid(plotlist = charts_list)
}


#' #'Combine multiple charts together
#' #'
#' #'facet_by is for small_multiples
#' #'ncol and nrow is for many types general
#' #'
#' #'@export
#' combine_charts <- function(combo_type, charts) {
#'   all_combo_types <- c("small_multiples", "many_types_general")
#'   check_valid_str(combo_type, all_combo_types)
#'   switch(combo_type,
#'          "small_multiples" = combine_many_types_general(charts),
#'          "many_types_general" = combine_many_types_general(charts))
#' }
