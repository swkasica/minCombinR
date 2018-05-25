# Helper Functions for Chart combinations

# gg_chart = a ggchart
# facet_var = a string or a vector of strings
# that are the column names in gg_chart data to facet by
combine_small_multiples <- function(gg_chart, facet_var) {
  if ("ggplot" %in% class(gg_chart)) {
    gg_chart + facet_wrap(facet_var)
  }
}
