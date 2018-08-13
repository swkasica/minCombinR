
#All S4 functions live in the methods package.
#methods is always available when running interactively but not in bash mode.
require(methods)

# ---- BaseSpec Class Definition ----
.BaseSpec <- setClass(Class = "BaseSpec",
                      slots = c(chart_type = "character",
                                data = "ANY", #TODO check that data is of the correct source later.
                                x = "character",
                                y = "character",
                                z = "character",
                                stack_by = "character",
                                fill = "character",
                                group = "character",
                                title = "character",
                                path = "character",
                                category = "character",
                                cluster_vars = "character", #A vector of characters for a dendro
                                tip_var = "character", #A vector for the tip var column in data,for a dendro
                                comparisons = "list", # A list of comparison objects for genomic map
                                # --- bar ---
                                layout = "character",
                                proportional = "logical", # !!!
                                reference_vector = "character", # A vector of characters
                                reference_var = "character",
                                # --- stream ---
                                key = "character",
                                value = "character",
                                date = "character",
                                # --- timeline ---
                                start = "character",
                                end = "character",
                                names = "character",
                                events = "character",
                                # --- table ---
                                rownames = "character",
                                # --- geographic map ---
                                lat_var = "character",
                                long_var = "character",
                                # --- node link ---
                                directed = "logical", #!!!
                                # --- small multiples and composite ---
                                x_limits = "ANY",
                                y_limits = "ANY", #TODO: can be vector of characters or numerics
                                # --- composite ---
                                flip_coord = "logical",
                                rm_y_labels = "logical",
                                rm_x_labels = "logical",
                                # --- many types linked ---
                                colour_var = "character",
                                colour_scale = "character", # A vector of characters with associated var name and colour
                                link_mark_type = "character",
                                # -- reencoded marks --
                                reencode_var = "character",
                                mark_type = "character",
                                channel = "character"),

                      #Set the default values for the reencodement vars
                      prototype = list(reencode_var=character(0),
                                       mark_type=character(0),
                                       channel=character(0))

)

