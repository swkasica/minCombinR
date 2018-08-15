
# ---- BaseSpec Class Definition ----
#TODO: change the values of NULL and NA? will always call through specify_base so maybe restructure so
#       specify_base is creating new fields based on the user inputs...
#        - this way there would not be a bunch of empty fields!
BaseSpec <- R6::R6Class(classname = "BaseSpec", public = list(
  chart_type = NULL,
  data = NA, #TODO change this to the correct source later!!!
  x = NULL,
  y = NULL, #could also be 1...
  z = NULL,
  stack_by = NULL,
  fill = NULL,
  group = NULL,
  title = NULL,
  path = NULL,
  category = NULL,
  cluster_vars = NULL, #A vector of characters for a dendro
  tip_var = NULL, #A vector for the tip var column in data,for a dendro
  comparisons = NA, # A list of comparison objects for genomic map
  # --- bar ---
  layout = NULL,
  proportional = FALSE, # !!! logical
  reference_vector = NULL, # A vector of characters
  reference_var = NULL,
  # --- stream ---
  key = NULL,
  value = NULL,
  date = NULL,
  # --- timeline ---
  start = NULL,
  end = NULL,
  names = NULL,
  events = NULL,
  # --- table ---
  rownames = NULL,
  # --- geographic map ---
  lat_var = NULL,
  long_var = NULL,
  # --- node link ---
  directed = FALSE, #!!! logical


  # ---- Combinations that are not initialized but set by ComboSpec Object changes ----
  # --- small multiples and composite ---
  x_limits = NULL, #TODO: change to list()??
  y_limits = NULL, #TODO: can be vector of characters or numerics ... maybe change to list?
  data_subset = NULL,
  # --- composite ---
  flip_coord = FALSE, #logical
  rm_y_labels = FALSE, #logical
  rm_x_labels = FALSE, #logical
  # --- many types linked ---
  colour_var = NULL,
  colour_scale = NA, # A vector of characters with associated var name and colour !!!
  link_mark_type = NULL,


  # -- reencoded marks --
  reencodements = list(), # list

  # ---------- methods ----------
  reencode = function(reencode_var, mark_type = "default", channel = "colour") {
    self$reencodements <- append(self$reencodements,
                                 list(c(reencode_var = reencode_var,
                                        mark_type = mark_type,
                                        channel = channel)))
    invisible(self)
  },

  set_limits = function(x_limits, y_limits) {
    self$x_limits <- x_limits
    #TODO: fix in future
    #self$y_limits <- y_limits
  },

  # ---------- initializer ----------
  #TODO: checks for all inputs OR make validation methods for all chart types?
  # -- types can be found in Learning/test_oop/testR6oop
  initialize = function(chart_type, data, x, y, z,
                        stack_by, fill, group, title, path, category,
                        # --- dendro ---
                        cluster_vars, #A vector of characters for a dendro
                        tip_var, #A vector for the tip var column in data,for a dendro
                        comparisons, # A list of comparison objects for genomic map
                        # --- bar ---
                        layout, proportional,
                        reference_vector, reference_var,
                        # --- stream ---
                        key, value, date,
                        # --- timeline ---
                        start, end, names, events,
                        # --- table ---
                        rownames,
                        # --- geographic map ---
                        lat_var, long_var,
                        # --- node link ---
                        directed) {

    #TODO: either check for all valid inputs here or in individual render chart fcns using a validation method
    stopifnot(is.character(chart_type), chart_type %in% all_chart_types)
    stopifnot(is.character(x))

    self$chart_type <- chart_type
    self$data <- data
    self$x <- x
    self$y <- y
    self$z <- z
    self$stack_by <- stack_by
    self$fill <- fill
    self$group <- group
    self$title <- title
    self$path <- path
    self$category <- category
    # --- dendro ---
    self$cluster_vars <- cluster_vars #A vector of characters for a dendro
    self$tip_var <- tip_var #A vector for the tip var column in data,for a dendro
    self$comparisons <- comparisons # A list of comparison objects for genomic map
    # --- bar ---
    self$layout <- layout
    self$proportional <- proportional
    self$reference_vector <- reference_vector
    self$reference_var <- reference_var
    # --- stream ---
    self$key <- key
    self$value <- value
    self$date <- date
    # --- timeline ---
    self$start <- start
    self$end <- end
    self$names <- names
    self$events <- events
    # --- table ---
    self$rownames <- rownames
    # --- geographic map ---
    self$lat_var <- lat_var
    self$long_var <- long_var
    # --- node link ---
    self$directed <- directed
  }
))


# methods = list(
#   initialize = function() {
#     data <<- data.frame()
#     x_limits <<- NA
#     y_limits <<- NA
#     flip_coord <<- FALSE
#     rm_y_labels <<- FALSE
#     colour_var <<- character(0)
#     colour_scale <<- NA
#     link_mark_type <<- character(0)
#     reencodements <<- list()
#   }
# )

# #Set the default values for the reencodement vars
# prototype = list(reencode_var=character(0),
#                  mark_type=character(0),
#                  channel=character(0))



