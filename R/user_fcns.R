#General TODO's
# Note: more specific TODO's are on appropriate functions
#TODO: Allow the user to override the defaults (something with ..., although not sure how to do it yet)
#TODO: decide on standard input when using factor(var)... do we include the functionality or does the user have to
#TODO: fix problem with get() where the most recent dataset is used? (see common_stat_examples)

#In input functions, make sure the following are good:
#TODO: MAKE ERROR MESSAGES WHEN THE USER DOENS'T INPUT THINGS THAT ARE ALLOWED. INCLUDING IF THEY HAVEN'T DECLARED THEIR BASE CHARTS YET!!
#TODO: Consider data frames with NA rows.

#' Specify requirements to make a base chart
#'
#' @param chart_type
#'
#' @export
specify_base <- function(chart_type, data, x, y, z, ...) {
  all_chart_types <-  c(#common statistical
    "bar", "line", #"stacked_bar","divergent_bar",
    "heat_map","heatmap", "density", "scatter", "pie", "venn",
    "histogram","pdf", "boxplot","box_plot", "swarm",
    #relational
    "node_link", "chord",
    #temporal
    "stream", "timeline",
    #spatial
    "geographic_map", "choropleth", "interior_map",
    #other
    "table", "category_stripe", "image",
    #genomic
    "phylogenetic_tree", "dendrogram", "clonal_tree",
    "linear_genomic_map", "radial_genomic_map", "alignment"
  )
  check_valid_str(chart_type, all_chart_types)

  # return(as.character(match.call()))
  return(match.call())
}

#TODO: currently only allows for one combo_type. Account for complex combos later.
# Note: I am not returning match.call() here because the ... only returns the name of the elements called (not the result)
#Important note: for composite combo with a tree, you need to include all of the node tips in one of the variables that you are aligning to
# For example: if you have a tree that has tip nodes A,B,C,D and you want to align it to a bar chart's x axis, then the
# var for the x axis must have values A,B,C,D also.
#' Specify a combination
#'
#' #'
#' @export
specify_combo <- function(combo_type, ..., facet_by=NA, link_var=NA, link_mark_type="default",
                          alignment = NA, common_var=NA, order=NA) {
  lo_specs <- list(...)
  names_prefix <- "base"
  names_suffix <- seq(1:length(lo_specs))
  base_names <- paste(names_prefix, names_suffix, sep = "_")
  names(lo_specs) <- base_names
  lo_specs <- c(lo_specs, combo_type = combo_type)
  if (combo_type == "small_multiple") {
    lo_specs <- c(lo_specs, facet_by = facet_by)
  } else if (combo_type == "many_types_linked") {
    lo_specs <- c(link_var = link_var, link_mark_type = link_mark_type, lo_specs)
  } else if (combo_type == "composite") {
    lo_specs <- c(alignment = alignment, common_var = common_var, order = order, lo_specs)
  }
  return(lo_specs)
}

# TODO: Outlined in transition doc.
#' @export
add_mark <- function(base_spec, mark_type = 'default') {
  #TODO: implement for all chart types (only done map so far)
  #TODO: add options for mark types (only have default for map so far)
  if (mark_type != 'default') {
    stop("currently added marks only works for default mark types")
  }
  #TODO: add ability to add more than one mark type
  base_spec$add_mark <- mark_type

  return(base_spec)
}

#TODO Currently only works with the default mark type and the colour channel!!!
#' Specify a reencoded mark
#' @param mark_type A string that specifies the type of mark to reencode. default depends on the type of chart. Possible strings include: 'default', 'area', 'line', 'point' and 'text'
#' @param channel_type A string that specifies the type of channel to reencode. Default is 'colour'. Possible strings include: 'colour' , size', 'shape', 'texture', 'font_face'
#' @export
specify_reencoding <- function(base_specification, reencode_var, mark_type='default', channel='colour', override = FALSE) {

  #Set the variables for reencoded marks!:

  no_default_reencoding <- list("heatmap", "heat_map", "density", "table", "category_stripe", "image")

  #Could have this as a setter method if using R6 in the specify reencoding user fcn
  if (mark_type == "default") {
    if (channel == "color" || channel == "colour") {
      base_specification$default_colour_var <- reencode_var
    } else {
      #could have this check in the setter method if using R6 in the specify reencoding user fcn
      stop("Have not implemented channels that are not colour for reencoded marks yet!")
    }
  } else {

    stop("currently only works with default mark types")

  }

  #TODO: error messages if reencode something twice.
  # if (!is.null(reencodement)) {
    # lapply(reencodement, function(reencode_specs) {
      # reencode_var <- reencode_specs[["reencode_var"]]
      # mark_type <- reencode_specs[["mark_type"]]
      # channel <- reencode_specs[["channel"]]

    # })
  # }

  # base_specification$reencodement <- append(base_specification$reencodement,
  #                                           list(c(reencode_var = reencode_var,
  #                                                  mark_type = mark_type,
  #                                                  channel = channel)))

  # base_specification$colour_var <- colour_var

  return(base_specification)
}

#TODO: decide on shorter names for chart_types and combinations (ex. "many_types_linked" --> "linked")
#' Plot
#'
#' @export
plot <- function(specs) {

  #No combination!
  if(class(specs) == "call") {
    spec_list <- as.list(specs)
    spec_list <- spec_list[-1]
    #Print this here *for now* to make it easy to see what the specs are
    print('spec_list')
    print(spec_list)
    spec_plot <- do.call(plot_simple, args = spec_list)
    return(arrange_plots(chart_list = list(spec_plot)))
  }

  else {

    #TODO:should be able to wrap calls around specify_base() so this should be not specific to specify_base()!!!
    base_calls <- specs[sapply(1:length(specs),
                               function(x) {
                                 specs[[x]][1] == "specify_base()" && !is.na(as.list(specs[[x]][1]))
                               })]

    #View multiple plots in a single view
    if (specs$combo_type == "small_multiple") {
      base_specs <- as.list(specs$'base_1')
      base_specs[[1]] <- NULL
      return(do.call(plot_small_multiples, c(base_specs, specs$facet_by)))
    }

    if (specs$combo_type == "many_types_general") {
      base_specs <- lapply(base_calls, function(x) {
        x[[1]] <- NULL
        as.list(x)
      })
      return(do.call(plot_many_types_general, base_specs))
    }

    if(specs$combo_type == "many_types_linked") {
      base_specs <- lapply(base_calls, function(x) {
        x[[1]] <- NULL
        as.list(x)
      })
      return(do.call(plot_many_linked, c(link_var = specs$link_var, link_mark_type = specs$link_mark_type, base_specs)))
    }

    if (specs$combo_type == "composite") {
      base_specs <- lapply(base_calls, function(x) {
        x[[1]] <- NULL
        as.list(x)
      })
      return(do.call(plot_composite, c(base_specs, alignment = specs$alignment, common_var = specs$common_var, order = specs$order)))
    }

  }

}
