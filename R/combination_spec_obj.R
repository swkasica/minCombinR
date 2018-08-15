
# ----- Combination Specification Class Definition -----

ComboSpec <- R6::R6Class(classname = "ComboSpec", public = list(
  combo_type = NULL,
  chart_spec_obj_list = NA, #list of ChartSpecs'
  facet_by = character(0),
  link_var = character(0),
  link_mark_type = character(0),
  alignment = character(0),
  common_var = character(0),
  order = NULL,

  # --- Methods ---
  set_small_multiples = function(facet_by, chart_specs, x_limits, y_limits) {
    self$facet_by = facet_by
    self$chart_spec_obj_list = chart_specs$set_limits(x_limits, y_limits)
  },

  #TODO: type checking!!!
  initialize = function(combo_type, chart_spec_obj_list, facet_by, link_var, link_mark_type, alignment, common_var, order) {
    self$combo_type <- combo_type
    if (combo_type == "small_multiple") stopifnot(length(chart_spec_obj_list) == 1)
    self$chart_spec_obj_list <- chart_spec_obj_list
    self$facet_by <- facet_by
    self$link_var <- link_var
    self$link_mark_type <- link_mark_type
    self$alignment <- alignment
    self$common_var <- common_var
    self$order <- order
  }
))
