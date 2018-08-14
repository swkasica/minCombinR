
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

  #TODO: type checking!!!
  initialize = function(combo_type, chart_spec_obj_list, facet_by, link_var, link_mark_type, alignment, common_var, order) {
    self$combo_type <- combo_type
    self$chart_spec_obj_list <- chart_spec_obj_list
    self$facet_by <- facet_by
    self$link_var <- link_var
    self$link_mark_type <- link_mark_type
    self$alignment <- alignment
    self$common_var <- common_var
    self$order <- order
  }
))
