#TODO: alphabetize?
all_chart_types <-  c(#common statistical
  "bar", "divergent_bar", "line", #"stack_by_bar",
  "heat_map", "heatmap", "density", "scatter", "pie", "venn",
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
  ####TODO: include unrooted tree, composition plot, miscellany?, sankey and sequence logo plot!
)

#TODO: include unrooted_tree, composition_plot, sankey and miscellany(?)
not_spatially_alignable <- c("pie", "venn", "node_link", "image")
#TODO: chart types to add to matrix: violin, radial genomic map, linear genomic map
#TODO: chart types to implement (in matrix but not in package): sequence_logo_plot, radial_phylogenetic_tree?, gel_image?, relative vs absolute streamgraph??

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

#Get consistent variables (called limits in ggplot) for all of the specified charts
#Note - specified_charts is a list of charts, with each chart only requiring fields chart_type and data
#Note- The var_name has to be the same type in all the specified_charts
#TODO: var_name can only be "var" or "as.factor(var)"
#TODO: Consider statistical transformations inherent to the base charts! The limits will need to change in these cases. This is important.
get_limits <- function(specified_charts, var_name) {
  limits <- list()
  sapply(specified_charts, function(chart) {
    ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)

    #TODO: using grep to remove as.factor() so I can get column... see if there is a better way to do this.
    if (grepl("^as.factor\\({1}|\\){1}$", var_name)) {
      #If the var_name is as.factor, will remove this.
      #TODO: maybe come up with a better way to handle this!
      var_name <- gsub("^as.factor\\({1}|\\){1}$", "", var_name)
      unique_vars <- unique(ref_data[[var_name]])
      limits <<- unique(c(limits, as.vector(unique_vars)))
    } else if (is.numeric(ref_data[[var_name]])) {
      unique_vars <- unique(ref_data[[var_name]])
      min <- min(unique_vars)
      max <- max(unique_vars)
      limits <<- c(limits, min, max)
    } else {
      unique_vars <- unique(ref_data[[var_name]])
      limits <<- unique(c(limits, as.vector(unique_vars)))
    }
  })

  if (is.numeric(limits) && !grepl("^as.factor\\({1}&\\){1}$", var_name)) {
    min <- min(limits)
    max <- max(limits)
    limits <- c(min,max)
  }

  return(limits)
}

#Old method:
#
#     #For continuous variables
#   #TODO: To see the types_of_variables_for_charts go to: Dropbox\gevitR\scratch\shannah\Organization
#   #Gets the limits for a continuous var_name
#   if(is.numeric(specified_charts[[1]]$data[[var_name]])) {
#     min_lim <- sapply(specified_charts, function(chart) {
#       ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
#       unique_links <- unique(ref_data[[var_name]])
#       min(unique_links)
#     })
#
#     max_lim <- sapply(specified_charts, function(chart) {
#       ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
#       unique_links <- unique(ref_data[[var_name]])
#       max(unique_links)
#     })
#
#     min_lim <- min(min_lim)
#     max_lim <- max(max_lim)
#     print('min lim')
#     print(min_lim)
#     print('max lim')
#     print(max_lim)
#     limits <- c(min_lim, max_lim)
#   }
#   #For discrete variables
#   #Gets all possible var_names
#   #TODO: consider using a hash
#   else {
#     limits <- list()
#     lapply(specified_charts, function(chart) {
#       ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
#       unique_vars <- unique(ref_data[[var_name]])
#       limits <<- unique(c(limits, as.vector(unique_vars)))
#     })
#     #TODO: move this somewhere else.
#     #Convert numeric var_name to discrete
#     # tmp_dat <- get(as.character(specified_charts[[1]]$data))
#     # if (class(tmp_dat[[var_name]]) == "numeric") {
#     #   var_name <- paste0("factor(", var_name, ")")
#     # }
#   }


#'Plot a base chart type
#'
#'This function will create a single chart object that can be passed into layout_charts() to layout.
#'
#'See alternative documentation for input and return values
#'See alternative documentation for examples
#'
#'TODO: currently, the user must name each of the args that are optional if they don't have them in the right order
#'
#'@export
plot_simple <- function(chart_type, data, x=NA, y=NA, z=NA, stack_by=NA, fill=NA, group=NA, title=NA,
                        path, category, cluster_vars=NA, tip_var=NA, comparisons,
                        #For table
                        rownames=NA,
                        #FOR COMPOSITE (only implemented for a few chart types)
                        flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
                        #FOR MANY TYPES LINKED
                        colour_var=NA, colour_scale=NA,
                        #FOR SMALL MULTIPLES
                        x_limits=NA, y_limits=NA) {
  check_valid_str(chart_type, all_chart_types)
  switch(chart_type,
         #Common Stat Chart Types
         "bar" = plot_bar_chart(data, x, y, stack_by, title,
                                flip_coord, rm_y_labels, rm_x_labels, colour_var, colour_scale, x_limits, y_limits),
         # "stacked_bar" = plot_stacked_bar_chart(data, x, fill, title, colour_var, colour_scale),
         "divergent_bar" = plot_divergent_bar_chart(data, title, colour_var, colour_scale, x_limits, y_limits),
         "line" = plot_line_chart(data, x, y, group, title, colour_var, colour_scale, x_limits, y_limits),
         "heat_map" = plot_heatmap(data, x, y, z, title, colour_var, colour_scale, x_limits, y_limits),
         "heatmap" = plot_heatmap(data, x, y, z, title, colour_var, colour_scale, x_limits, y_limits),
         "density" = plot_density_chart(data, x, y, title, colour_var, colour_scale, x_limits, y_limits),
         "scatter" = plot_scatter(data, x, y, title, colour_var, colour_scale, x_limits, y_limits),
         "pie" = plot_pie_chart(data, x, title, colour_var, colour_scale, x_limits),
         "histogram" = plot_histogram(data, x, title, colour_var, colour_scale, x_limits),
         "pdf" = plot_pdf(data, x, title, colour_var, colour_scale, x_limits),
         "boxplot" = plot_boxplot(data, x, y, title, flip_coord, rm_y_labels, rm_x_labels, colour_var, colour_scale, x_limits, y_limits),
         "box_plot" = plot_boxplot(data, x, y, title, flip_coord, rm_y_labels, rm_x_labels, colour_var, colour_scale, x_limits, y_limits),
         "violin" = plot_violinplot(data, x, y, title, colour_var, colour_scale, x_limits, y_limits),
         "swarm" = plot_swarm_plot(data, x, y, title, colour_var, colour_scale, x_limits, y_limits),

        #TODO: many types linked and composite for non-common_stat_chart_types (and non ggplot2)
         #Relational
         "node_link" = plot_node_link(data, directed),
         "flow_diagram" = plot_flow_diagram(data), #TODO

         #Temporal
         #"stream_graph" = plot_streamgraph(data, key, value, date), #TODO: change param names
         #"timeline" = plot_timeline(data, stack_by, start, end, names, phase), #TODO: change input for stack_by

         #Spatial
         #"geographic_map" = plot_geographic_map(lat, long), #TODO: change input (see examples_obsandGenotype)
         "choropleth" = plot_choropleth(data, fill), #TODO: change input (see examples_obsandGenotype)
         "interior_map" = plot_image(path), #TODO: maybe change if you use the magick package

         #Other
         "table" = plot_table(data, flip_coord, rownames),
         "category_stripe" = plot_category_stripe(data, x, category),
         "image" = plot_image(path), #TODO: maybe change if you use the magick package

         #genomic
         "phylogenetic_tree" = plot_phylo_tree(path), #path is a path to a nwk_file
         "dendrogram" = plot_dendro(data, tip_var, cluster_vars),
         "clonal" = plot_clonal_tree(path, group),
         "linear_genomic_map" = plot_linear_genome_map_from_df(data, comparisons), #TODO:
         "radial_genomic_map" = NULL, #TODO: determine typical input
         "alignment" = plot_image(path) #TODO: will this be a table or an image in most cases?
         )
}

#'Many types general plot
#'
#'@param ... Any number of lists of arguments to generate a plot
#'
#'@export
plot_many_types_general <- function(...) {
  args_list <- list(...)
  all_plots <- lapply(args_list, function(x) {do.call(plot_simple, x)})
  layout_plots(all_plots, labels = "AUTO")
}

#TODO: Make the same x-axis and y-axis ranges.
#TODO: If there is colour, make sure the colour_scale is the same for each of the facets!
#TODO: only works on common statistical functions currently!
#TODO: maybe change input to ... so it's consistent with the others
#' Small multiples
#'
#'@param chart_type A string indicating type of chart to generate. Options are:
#'@param data A data frame
#'@param x optional for: ... required for: ...
#'@param y optional for: ... required for: ...
#'@param fill optional: ...
#'@param group optional: ... not applciable for : ...
#'@param facet_by
#'
#'@export
plot_small_multiples <- function(chart_type, data, facet_by, x, y=NA, z=NA, fill=NA, group=NA) {
  chart_specs <- list(chart_type = chart_type, data = deparse(substitute(data)))

  x_limits <- unlist(get_limits(list(chart_specs), x))
  if(!is.na(y)) {
    y_limits <- unlist(get_limits(list(chart_specs), y))
  }
  print(x_limits)
  print('y_limits')
  print(y_limits)

  #TODO: maybe just use facet_wrap for ggplot charts? (statistical chart types)
  #Create a list of data subsets according to the facetting variable
  facet_dat <- lapply(unique(data[[facet_by]]),
                      function(x) {dplyr::filter_(data, paste(facet_by, "==", quote(x)))})
  #Create a list of plots for each of the facet_dat subsets
  all_plots <- lapply(facet_dat,
                      function(sub_dat) gevitR::plot_simple(chart_type = chart_type,
                                                            data = select(sub_dat, -facet_by),
                                                            x = x,
                                                            y = y,
                                                            z = z,
                                                            fill = fill,
                                                            group = group,
                                                            x_limits = x_limits))
  layout_plots(all_plots, labels = "AUTO")
}

#
infer_x <- function(chart_args) {

  #Note: Will never align on y axis of dendrogram
  if (chart_args$chart_type == "dendrogram") {
    #TODO: Note dependency on tip_var name
    if ("tip_var" %in% names(chart_args)) {
      return(chart_args$tip_var)
    } else {
      #TODO: !!!
      stop("This is a case where the tip_var is not defined because it is the rownames...")
    }
  }

  #All other cases
  else {
    return(chart_args$x)
  }

}

infer_y <- function(chart_args) {
  if (chart_args$chart_type == "table") {
    #TODO: for now, I am assuming the comp variable is the first column (this can be changed later and reoordered but requires extra checks)
    colnames(get(as.character(chart_args$data)))[1]
  }

  else {
    return(chart_args$y)
  }
}

#New algorithm developed with Ana to check if composites are combinable:
#TODO: remove export... just putting here for testing!
#Currently returns nothing... will return errors for charts that are not combinable.
#'@export
check_combinable_composite <- function(chart_args_list) {
  chart_types <- lapply(chart_args_list, function(chart_args) {chart_args$chart_type})
  alignable_mat <- composite_matrix

  # ~Are charts in 'spatially alignable' category?
  lapply(chart_args_list, function(chart_args) {
    if (chart_args$chart_type %in% not_spatially_alignable) {
      stop(paste("Chart type:", chart_args$chart_type, "is not spatially alignable.
                 You might try combining using linkage instead of composite."))
    }
  })

  #For each chart combination,
  lapply(1:(length(chart_args_list) - 1), function(n) {
    lapply((n + 1):length(chart_args_list), function(m) {

      #Get the chart arguments and axes for reuse
      chartn <- chart_args_list[[n]]
      chartm <- chart_args_list[[m]]
      chart_axes <- c(infer_x(chartn), infer_y(chartn), infer_x(chartm), infer_y(chartm))

      # ~Are charts spatially alignable?
      if (alignable_mat[chart_types[[m]], chart_types[[n]]][1] != 1) {
        stop(paste("Chart type:", as.character(chart_types[n]), "and", as.character(chart_types[m]),
                   "cannot be spatially combined through composites."))
      }

      #Do the charts have the same var name for any axes combination?  (x+x, x+y or y+y)
      #If yes, skip
      if (!anyDuplicated(chart_axes)) {
        #Do the charts have the same data frame?
        if (chartm$data == chartn$data) {
          #Do the charts have the same range? if yes, skip
          #TODO:
          stop("Haven't implemented this case yet!!! TODO")
        }
        #Charts have different data frames and don't have any axes combination the same
        stop(paste("Charts",
                   "are not spatially alignable because they do not have the common variable names on their x or y axes.",
                   "Please change the x or y axes of the chart specifications to have the same variable name."))
      }
    })
  })
}

#New algorithm for composites developed with Ana:
#TODO: remove @export... just used for testing
#'@export
plot_composite <- function(..., alignment = NA) {
  chart_args_list <- list(...)

  #Charts are spatially alignable if it passes through here (otherwise will report error and stop)
  check_combinable_composite(chart_args_list)

  #For now, I am assuming all of the charts have an x and y variable as input
  #TODO: infer the x and y variables from charts that have a different name for these!
  all_x_vars <- unlist(sapply(chart_args_list, function(chart_args) {infer_x(chart_args)}))
  all_y_vars <- unlist(sapply(chart_args_list, function(chart_args) {infer_y(chart_args)}))

  all_vars <- c(all_x_vars, all_y_vars)
  all_vars[sapply(all_vars, is.null)] <- NULL

  #Determine the common variable name
  comp_var <- names(table(all_vars)[table(all_vars) > (length(chart_args_list) - 1)])[1] #If there is more than one comp_var, then just combine on the first one (x).

  #TODO: remove assertion later
  #Assert the comp_var is not null for my sake in implementing
  if (is.null(comp_var) || is.na(comp_var)) {
    stop('composite variable is not common. This is an error in the code. Please report.')
  }

  #Decide on alignment if not already given
  if (is.na(alignment)) {
    x_var_count <- sum(all_x_vars == comp_var)
    y_var_count <- sum(all_y_vars == comp_var)
    if (y_var_count == 0) {
      alignment <- 'vertical'
    }
    if (x_var_count == 0) {
      alignment <- 'horizontal'
    }
    if (x_var_count >= y_var_count) {
      alignment <- 'vertical'
    } else {
      alignment <- 'horizontal'
    }
  }

  # Is the alignment vertical?
  if (alignment == 'vertical' || alignment == 'v') {
    #For each chart, does the x_axis have the common var?
    lo_plots <- lapply(chart_args_list, function(chart_args) {
      y_arg <- infer_y(chart_args)

      #If no, rotate
      if (!is.null(y_arg) && y_arg == comp_var) {
        do.call(plot_simple, c(chart_args, flip_coord=TRUE))
      }

      #If yes, do not rotate
      else {
        do.call(plot_simple, chart_args)
      }
      })

    #Plot vertically in a grid
    cowplot::plot_grid(plotlist = lo_plots, ncol = 1, align = "v")

  #Is the alignment horizontal?
  } else if (alignment == 'horizontal' || alignment == 'h') {

    #Generate each chart accordingly (with rotations if necessary)
    lo_plots <- lapply(chart_args_list, function(chart_args) {
      y_arg <- infer_y(chart_args)
      if (!is.null(y_arg) && y_arg == comp_var) {
        do.call(plot_simple, c(chart_args))
        } else {
          do.call(plot_simple, c(chart_args, flip_coord=TRUE))
          }
      })
    #Plot horizontally in a grid
    cowplot::plot_grid(plotlist = lo_plots, nrow = 1, align = "h")
  }
  #Other alignments (not implemented overlay)
  #TODO: implement overlay option
  else {
    stop('overlay has not been implemented yet!')
  }

}

#TODO: THIS ASSUMES THAT AT LEAST ONE AXIS IS THE EXACT SAME FOR THE CHARTS BEING COMBINED.
#TODO: handle reordering for x axis and y axis
#TODO: handle range adjustments (discrete and continuous)
#TODO: move the charts closer together somehow
#TODO: make rotate and alignment(?...) computed rather than specified
#TODO: allow more than 2 charts
#TODO: currently only works with ggplots !!! change for each chart_type that isn't derived from ggplot2
#TODO: consider case where the geoms are in the exact same chart (ex. hist and line together)
#TODO: add boolean variable to add charts on top of each other.
#TODO: implement options for adding charts on top of each other rather than aligned 'v' or 'h'
#TODO: if one of the charts have a legend, make sure they align properly still.
#TODO: only works on common statistical functions currently!
#'Composite
#'
# plot_composite <- function(alignment = NA, rotate1 = F, rotate2 = F, ...) {
#   args_list <- list(...)
#   #TODO: allow for more than 2 charts
#   chart1_args <- args_list[[1]]
#   chart2_args <- args_list[[2]]

  #Automatic rotations
  #TODO:Find the axis that are the same (get from base call)

  #When user specifies the alignment and rotations!
  # if (alignment == "v" || alignment == "vertical") {
  #   plot1 <- do.call(plot_simple, c(chart1_args, flip_coord=rotate1, rm_y_labels=FALSE, rm_x_labels=TRUE))
  #   plot2 <- do.call(plot_simple, c(chart2_args, flip_coord=rotate2))
  #   cowplot::plot_grid(plot1, plot2, ncol = 1, align = "hv")
  # } else if (alignment == "h" || alignment == "horizontal") {
  #   plot1 <- do.call(plot_simple, c(chart1_args, flip_coord=rotate1))
  #   plot2 <- do.call(plot_simple, c(chart2_args, flip_coord=rotate2, rm_y_labels=TRUE, rm_x_labels=FALSE))
  #   cowplot::plot_grid(plot1, plot2, nrow = 1, align = "hv")
  # } else if (alignment == "o" || alignment == "overlay") {
  #   #TODO: only considering plots made with both x and y values
  #   #TODO: Currently only works on charts with the same data frame as input
  #   #TODO: Currently only works for plots that scales are ggplot(data, aes_string(x,y))
  #   #TODO: Currently only implemented for common_stat chart types
  #   data <- rbind(get(as.character(chart1_args$data)), get(as.character(chart2_args$data)))
  #   if (chart1_args$x == chart2_args$x) {
  #     if (chart1_args$y == chart2_args$y) {
  #       return(ggplot(data, aes_string(chart1_args$x, chart1_args$y)) +
  #         get_geom(chart1_args$chart_type) +
  #         get_geom(chart2_args$chart_type))
  #     }
  #     #TODO: dual axes
  #   } else {
  #     stop("Must have the same x axis for composite overlay (current implementation)")
  #   }
  # } else {
  #   stop('Alignment can be one of "h"/"horizontal", "v"/"vertical" or "o"/"overlay"')
  # }
  #TODO: also consider overlaying option (req'd: one axis the same!! (also consider rotations...))

  #This fixes the distance between the plots problem but isn't using cowplot (uses grid and ggplotGrob) which might not work for non-ggplots.
  # library(grid)
  # grid.newpage()
  # grid.draw(rbind(ggplotGrob(simple_bar), ggplotGrob(simple_box), size = "last"))
# }


#Gets a geom
#It would be a mistake to generate the charts from an entirely different function if it is composite overlay.
#I should come up with a better way to do this!
# get_geom <- function(chart_type) {
#   overlay_chart_types <-  c(#common statistical
#     "bar", "divergent_bar", "line", #"stack_by_bar", "heat_map", "heatmap",
#     "density", "scatter", #"pie", "venn",
#     "histogram","pdf", "boxplot","box_plot","violin", "swarm"
#   )
#   check_valid_str(chart_type, overlay_chart_types)
#   switch(chart_type,
#          #Common Stat Chart Types
#          "bar" = geom_bar(stat = "identity"), #TODO:assuming no stacked bar for now
#          # "divergent_bar" = plot_divergent_bar_chart(data, title, colour_var, colour_scale),
#          "line" = geom_line(), #TODO: assuming no groups
#          # "heat_map" = plot_heatmap(data, x, y, z, title, colour_var, colour_scale),
#          # "heatmap" = plot_heatmap(data, x, y, z, title, colour_var, colour_scale),
#          "density" = stat_density_2d(aes(fill = ..level..), geom = "polygon"),
#          "scatter" = geom_point(),
#          # "pie" = plot_pie_chart(data, x, title, colour_var, colour_scale),
#          # "histogram" = geom_histogram(), #TODO: Note: histogram doesn't have y axis in data (is computed)
#          # "pdf" = plot_pdf(data, x, title, colour_var, colour_scale),
#          "boxplot" = geom_boxplot(),
#          "box_plot" = geom_boxplot(),
#          "violin" = geom_violin(),
#          "swarm" = ggbeeswarm::geom_beeswarm()
#   )
# }

#TODO: allow for linking in other ways than color (WHAT OTHER WAYS?)
#TODO: decide on color palette for different types of data (discrete and continuous) (DO IN MEETING)
#TODO: use common color scale legend (link_var legend) [do this in the future but not yet]
#TODO: add manual colour options to non-common_statistical charts (DO ONCE HAVE BASE CHARTS NAILED DOWN)
#TODO: only works on common statistical functions currently!
#TODO: make sure that statistical transformations on colour_limits are performed if the colour_scale is transformed when generating chart.
#'Many Types Linked
#'
plot_many_linked <- function(link_var, link_by="colour", ...) {
  specified_charts <- list(...)
  if (link_by == "colour" || link_by == "color") {
    limits <- get_limits(specified_charts, link_var)
    # colour_var <- colour_info$var_name
    # limits <- colour_info$limits

    #Gets the colour match for a discrete scale_var
    if (!is.numeric(limits)) {
      get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      colour_limits <- get_palette(length(limits))
      names(colour_limits) <- limits
    } else {
      colour_limits <- limits
    }

    plots <- lapply(specified_charts, function(chart) {
      do.call(plot_simple, args = c(chart, list(colour_var = link_var, colour_scale = colour_limits)))
    })

    gevitR::layout_plots(plots, labels = "AUTO")
  }
}

#'Layout plots
#'
#'@param chart_list A list of charts
#'
#'@export
layout_plots <- function(chart_list, labels = NULL, shared_legend=FALSE) {

  chart_list <- lapply(chart_list, function(chart) {
    if('gg' %in% class(chart)) {
      ggplotify::as.grob(chart)
    } else {
      ggplotify::as.grob(chart)
    }
  })

  #This is currently not being used because creating a shared legend for many_linked and small_multiples
  #    may be more useful later. Right now, it is easier to make sure the fcns are doing waht they should
  #    by looking at the legend for each chart.
  if (shared_legend == TRUE) {
    legend <- cowplot::get_legend(chart_list[[1]])
    return(cowplot::plot_grid(plotlist = chart_list, labels = labels, legend = legend))
  }

  cowplot::plot_grid(plotlist = chart_list, labels = labels)
}

