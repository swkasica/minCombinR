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
specify_base <- function(chart_type, data=NA, x=character(0), y=character(0), z=character(0),
                         stack_by=character(0), fill=character(0), group=character(0), title=character(0), path=character(0),
                         category=character(0),
                         cluster_vars=c(character(0)), #A vector of characters for a dendro
                         tip_var=c(character(0)), #A vector for the tip var column in data,for a dendro
                         comparisons=list(), # A list of comparison objects for genomic map
                         # --- bar ---
                         layout=character(0), proportional=NA,
                         reference_vector=character(0), reference_var=character(0),
                         # --- stream ---
                         key=character(0), value=character(0), date=character(0),
                         # --- timeline ---
                         start=character(0), end=character(0),
                         names=character(0), events=character(0),
                         # --- table ---
                         rownames=character(0),
                         # --- geographic map ---
                         lat_var=character(0), long_var=character(0),
                         # --- node link ---
                         directed=NA) {

  all_chart_types <-  c("bar", "line", #"stacked_bar","divergent_bar",
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
                        "linear_genomic_map", "radial_genomic_map", "alignment")
  check_valid_str(chart_type, all_chart_types)

  base_spec <- .BaseSpec(chart_type=chart_type,
                         data=data, x=x, y=y, z=z,
                         stack_by=stack_by, fill=fill, group=group,
                         title=title, path=path, category=category,
                         cluster_vars=cluster_vars, #A vector of characters for a dendro
                         tip_var=tip_var, #A vector for the tip var column in data,for a dendro
                         comparisons=comparisons, # A list of comparison objects for genomic map
                         # --- bar ---
                         layout=layout, proportional=proportional,
                         reference_vector=reference_vector, reference_var=reference_var,
                         # --- stream ---
                         key=key, value=value, date=date,
                         # --- timeline ---
                         start=start, end=end, names=names, events=events,
                         # --- table ---
                         rownames=rownames,
                         # --- geographic map ---
                         lat_var=lat_var, long_var=long_var,
                         # --- node link ---
                         directed=directed)
  return(base_spec)

  # return(as.character(match.call()))
  # return(match.call())
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

#' Specify a reencoded mark
#' @param mark_type A string that specifies the type of mark to reencode. default depends on the type of chart. Possible strings include: 'default', 'area', 'line', 'point' and 'text'
#' @param channel_type A string that specifies the type of channel to reencode. Default is 'colour'. Possible strings include: 'colour' , size', 'shape', 'texture', 'font_face'
specify_reencodement <- function(base_specification, reencode_var, mark_type='default', channel='colour') {

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

