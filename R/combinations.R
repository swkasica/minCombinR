# Helper Functions for Chart combinations

# gg_chart = a ggchart
# facet_var = a string or a vector of strings
# that are the column names in gg_chart data to facet by
combine_small_multiples <- function(gg_chart, facet_var) {
  if ("gg" %in% class(gg_chart)) {
    gg_chart + facet_wrap(facet_var)
  }
}

#' Combine charts with format: many types general
#charts order matters (will fill in from upper left to bottom right)
combine_many_types_general <- function(charts, nrows, ncols) {

  figure <- multipanelfigure::multi_panel_figure(width = "auto", height = "auto", rows = nrows, columns = ncols)

  lapply(1:length(charts), function(x) {
    if (class(charts[[x]]) == "data.frame") {
      figure <<- figure %<>% multipanelfigure::fill_panel(multipanelfigure::capture_base_plot(charts[[x]]))
    } else {
      figure <<- figure %<>% multipanelfigure::fill_panel(charts[[x]])
    }
  })

  figure
}
