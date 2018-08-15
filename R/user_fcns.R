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
    "histogram","pdf", "boxplot","box_plot","violin", "swarm",
    #relational
    "node_link", "flow_diagram",
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

#TODO Currently only works with the default mark type and the colour channel!!!
#' Specify a reencoded mark
#' @param mark_type A string that specifies the type of mark to reencode. default depends on the type of chart. Possible strings include: 'default', 'area', 'line', 'point' and 'text'
#' @param channel_type A string that specifies the type of channel to reencode. Default is 'colour'. Possible strings include: 'colour' , size', 'shape', 'texture', 'font_face'
#' @export
specify_reencodement <- function(base_specification, reencode_var, mark_type='default', channel='colour', override = FALSE) {

  #Set the variables for reencoded marks!:

  no_default_reencodement <- list("heatmap", "heat_map", "density", "pie")

  #Could have this as a setter method if using R6 in the specify reencodement user fcn
  if (mark_type == "default") {
    if (channel == "color" || channel == "colour") {
      if (!is.null(base_specification$colour_var) && override == FALSE) {
        stop(paste("The colour channel for mark type: ", mark_type, " has already been set.
                     To override the previous colour channel, use override == TRUE in the reencodement.
                       ie. in reencode(..., override = TRUE)"))
      } else {
        base_specification$colour_var <- reencode_var
      }
    } else {
      #could have this check in the setter method if using R6 in the specify reencodement user fcn
      stop("Have not implemented channels that are not colour for reencoded marks yet!")
    }
  } else {

    stop("currently only works with default mark types!!!")

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
    spec_list <- spec_list[spec_list != "specify_base"]
    print(spec_list)
    spec_plot <- do.call(plot_simple, spec_list)
    return(arrange_plots(list(spec_plot)))
    # return(do.call(plot_simple, spec_list))
  }

  #TODO: clean
  else {
    #Found a better way to do this so commented out but may be useful later.
    #Find all combinations (currently only allowed one combo type so commented out)
    # combo_call <- specs[sapply(1:length(specs), function(x) {specs[[x]][1] == "specify_combo()"})]
    #Find the base calls for each of the charts in a combination
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
      return(do.call(plot_composite, c(alignment = specs$alignment, common_var = specs$common_var, order = specs$order, base_specs)))
    }

  }

}
