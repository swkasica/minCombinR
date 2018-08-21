#TODO: alphabetize?
all_chart_types <-  c(#common statistical
  "bar", "line", #"stack_by_bar", "divergent_bar",
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

master_chart_types <- c(
  "timeline", "histogram", "pdf", "flow_diagram",
  "stream", "geographic_map", "choropleth", "interior_map",
  "dendrogram", "phylogenetic_tree", #"alignment", #(alignment is just an image)
  "clonal_tree", "density_plot" #sequence_logo_plot" #(gel_image is just an image)
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
#TODO: var_name can be "var" or "as.factor(var)" where var is the variable name
get_limits <- function(specified_charts, var_name) {
  limits <- c()
  sapply(specified_charts, function(chart) {
    ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)

    #TODO: using grep to remove as.factor() so I can get column... see if there is a better way to do this.
    if (grepl("^as.factor\\({1}|\\){1}$", var_name)) {
      #If the var_name is as.factor, will remove this.
      #TODO: maybe come up with a better way to handle this!
      var_name <- gsub("^as.factor\\({1}|\\){1}$", "", var_name)
      unique_vars <- unique(ref_data[[var_name]])
      limits <<- unique(c(limits, as.vector(unique_vars)))
      print(limits)
    } else if (is.numeric(ref_data[[var_name]])) {
      unique_vars <- unique(ref_data[[var_name]])

      #!!!
      #Exception: If the chart type is a bar chart, the min MUST be 0 for the y-axis
      # Note: we are restricting the bar chart to only having a discrete axis.
      if(chart$chart_type == "bar") {
        min <- 0
      } else {
        min <- min(unique_vars)
      }
      max <- max(unique_vars)
      limits <<- c(limits, min, max)
    } else {
      unique_vars <- unique(ref_data[[var_name]])
      limits <<- unique(c(limits, as.vector(unique_vars)))
    }
  })

  if (is.numeric(limits) && !grepl("^as.factor\\({1}|\\){1}$", var_name)) {
    min <- min(limits)
    max <- max(limits)
    limits <- c(min,max)
  }
  print(limits)
  return(limits)
}

#'Plot a base chart type
#'
#'This function will create a single chart object that can be passed into render_charts() to render.
#'
#'See alternative documentation for input and return values
#'See alternative documentation for examples
#'
#'TODO: currently, the user must name each of the args that are optional if they don't have them in the right order
#'
#'@export
plot_simple <- function(chart_type, data, x=NA, y=NA, z=NA, stack_by=NA, fill=NA, group=NA, title=NA,
                        path, category, cluster_vars=NULL, tip_var=NULL, comparisons,
                        #For bar
                        layout="default", proportional = FALSE, reference_vector, reference_var,
                        #For stream
                        key, value, date,
                        #For timeline
                        start=NA, end=NA, names=NA, events=NA,
                        #For table
                        rownames=NA,
                        #For geographic map
                        lat_var=NA, long_var=NA,
                        #For dendro reencodements,
                        labels=NULL,
                        labels_col_var=NULL, labels_col_values=NULL, labels_col_palette=NULL, labels_size=NULL,
                        leaf_col_var=NULL, leaf_col_palette=NULL,
                        #For phylogenetic tree reencodements
                        branch_col_var=NULL, branch_col_palette=NULL,
                        #For node link
                        directed=FALSE,
                        #FOR COMPOSITE (only implemented for a few chart types)
                        flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
                        #FOR MANY TYPES LINKED
                        colour_var=NULL, colour_scale=NA, colour_mark_type=NA,
                        #FOR SMALL MULTIPLES and composite
                        x_limits=NA, y_limits=NA) {
  check_valid_str(chart_type, all_chart_types)
  switch(chart_type,
         #Common Stat Chart Types
         "bar" = render_bar_chart(data, x, y, stack_by, layout, proportional,
                                  reference_vector, reference_var, title,
                                  flip_coord, rm_y_labels, rm_x_labels,
                                  colour_var, colour_scale, x_limits, y_limits),
         # "stacked_bar" = render_stacked_bar_chart(data, x, fill, title, colour_var, colour_scale),
         # "divergent_bar" = render_divergent_bar_chart(data, title, colour_var, colour_scale, x_limits, y_limits),
         "line" = render_line_chart(data, x, y, group, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "heat_map" = render_heatmap(data, x, y, z, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "heatmap" = render_heatmap(data, x, y, z, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "density" = render_density_chart(data, x, y, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "scatter" = render_scatter(data, x, y, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "pie" = render_pie_chart(data, x, title, colour_var, colour_scale),
         "histogram" = render_histogram(data, x, title, colour_var, colour_scale, x_limits),
         "pdf" = render_pdf(data, x, title, colour_var, colour_scale, x_limits, flip_coord),
         "boxplot" = render_boxplot(data, x, y, title, rm_y_labels, rm_x_labels, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "box_plot" = render_boxplot(data, x, y, title, rm_y_labels, rm_x_labels, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "violin" = render_violinplot(data, x, y, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),
         "swarm" = render_swarm_plot(data, x, y, title, colour_var, colour_scale, x_limits, y_limits, flip_coord),

         #TODO: many types linked and composite for non-common_stat_chart_types (and non ggplot2)
         #Relational
         "node_link" = render_node_link(data, directed, colour_var),
         "flow_diagram" = render_flow_diagram(data), #TODO

         #Temporal
         "stream" = render_streamgraph(data, key, value, date), #TODO: change param names
         "timeline" = render_timeline(data, start, end, names, events, colour_var, colour_scale),

         #Spatial
         "geographic_map" = render_geographic_map(data, lat_var, long_var),
         "choropleth" = render_choropleth(data, lat_var, long_var, fill, group, flip_coord), #TODO: change input (see examples_obsandGenotype)
         "interior_map" = render_image(path), #TODO: maybe change if you use the magick package

         #Other
         "table" = render_table(data, flip_coord, rownames),
         "category_stripe" = render_category_stripe(data, x, category, x_limits),
         "image" = render_image(path), #TODO: maybe change if you use the magick package

         #genomic
         "phylogenetic_tree" = render_phylo_tree(data, x_limits, y_limits, flip_coord,
                                                 branch_col_var, branch_col_palette), #path is a path to a nwk_file
         "dendrogram" = render_dendro(data, labels,
                                      labels_col_var, labels_col_values, labels_col_palette, labels_size,
                                      leaf_col_var, leaf_col_palette,
                                      tip_var, cluster_vars),
         "clonal_tree" = render_clonal_tree(path, group, x_limits, y_limits, flip_coord),
         "linear_genomic_map" = render_linear_genome_map_from_df(data, comparisons), #TODO:
         "radial_genomic_map" = NULL, #TODO: determine typical input
         "alignment" = render_image(path) #TODO: will this be a table or an image in most cases?
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
  arrange_plots(all_plots, labels = "AUTO")
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
  arrange_plots(all_plots, labels = "AUTO")
}

#Gets the values of the x axis from the chart in the chart_args input.
infer_x <- function(chart_args) {
  #TODO: note dependency on variable names!

  #Note: Will never align on y axis of dendrogram
  if (chart_args$chart_type == "dendrogram") {
    if ("tip_var" %in% names(chart_args)) {
      return(chart_args$tip_var)
    } else {
      #TODO: !!!
      stop("TODO: This is a case where the tip_var is not defined because it is the rownames...")
      # return(rownames(get(as.character(chart_args$data))))
    }
  } else if (chart_args$chart_type == "choropleth") {
    return(chart_args$lat_var)
  } else if (chart_args$chart_type == "timeline") {
    return(chart_args$date)
  }

  #All other cases
  else {
    print('!!!')
    print(chart_args$x)
    return(chart_args$x)
  }

}

#Gets the values of the y axis
infer_y <- function(chart_args) {
  if (chart_args$chart_type == "table") {
    #TODO: for now, I am assuming the comp variable is the first column (this can be changed later and reoordered but requires extra checks)
    colnames(get(as.character(chart_args$data)))[1]
  } else if (chart_args$chart_type == "choropleth") {
    return(chart_args$long_var)
  } else if (chart_args$chart_type == "phylogenetic_tree") {
    stop("TODO: This is a case where the tip_var is not defined in the data frame from nwk file and so have to have different way of determining if the same var")
  } else if (chart_args$chart_type == "timeline") {
    return(chart_args$names)
  }
  else {
    return(chart_args$y)
  }
}

get_order <- function(chart_args_list, common_var) {

  get_order_from_chart <- function(chart_args) {
    chart_type <- chart_args$chart_type

    #Phylogenetic tree tip ordering - always aligns on the y axis so don't need to know common_var
    if (chart_type == "phylogenetic_tree") {
      #TODO: fortify might be deprecated in the future!!! Find a new fcn for this in in the broom package (wasn't initially easy)
      tree_dat  <- fortify(treeio::read.newick(chart_args$path))
      tree_tips <- subset(tree_dat, isTip)
      return(tree_tips$label[order(tree_tips$y, decreasing=TRUE)])
    }
    #Dendrogram tree tip ordering - always aligns on the y axis so don't need to know common_var
    #TODO: This is repeating some code from render_dendro - see if there is a way to get the tip var without having to access the data.
    else if (chart_type == "dendrogram") {
      tip_var <- as.character(chart_args$tip_var)
      cluster_vars <- as.character(chart_args$cluster_vars)
      data <- get(as.character(chart_args$data))

      if (!is.na(tip_var) && !is.na(cluster_vars)) {
        data <- unique(data %>%
                         group_by_(tip_var) %>%
                         select(c(tip_var, cluster_vars)))
        #Could set rownames using tibble package instead of base but it's not worth the extra dependency
        data <- as.data.frame(data)
        rownames(data) <- data[ , tip_var]
        data[ , tip_var] <- NULL
      }

      #TODO: What if they are manually clustering or want a different clustering option?
      clust_data <- data %>%
        scale() %>%
        dist() %>%
        hclust(method = "ward.D2")
      clust_dendro <- as.dendrogram(clust_data)
      return(as.vector(ggdendro::dendro_data(clust_dendro)$labels[["label"]]))
    } else {
      #Otherwise return the order that is found in the data frame.
      return(chart_args$data[[common_var]])
    }
  }

  master_ordering <- c()

  for (chart_args in chart_args_list) {
    chart_type <- chart_args$chart_type
    if (chart_type %in% master_chart_types) {
      if (is.null(master_ordering)) {
        #Set the master ordering to the order of the chart
        master_ordering <- get_order_from_chart(chart_args)
      } else {
        #Does the chart have the same ordering as the master ordering?
        #Note: This also requires that the ordering has the same length
        if (!identical(get_order_from_chart(chart_args), master_ordering)) {
          stop ("These charts are not combinable by composite because both of them
                have a fixed ordering that are not the same. Instead, try combining
                these charts using many_types_linked.")
        }
        }
        }
      }

  #If there are no master charts, return the first charts order
  if (is.null(master_ordering)) {
    ref_data <- get(as.character(chart_args_list[[1]]$data))
    master_ordering <- as.vector(ref_data[[common_var]])
  }

  return(master_ordering)
    }

#New algorithm developed with Ana to check if composites are combinable:
#Returns nothing... will return errors for charts that are not combinable.
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
        #Charts have different data frames and don't have any axes combination the same
        stop(paste("Charts",
                   "are not spatially alignable because they do not have the common variable names on their x or y axes.",
                   "Please change the x or y axes of the chart specifications to have the same variable name."))
      }
    })
  })
  }

#TODO: remove labels that are on the common axis for all charts but the last - will do once we have done some testing as it will make testing easier
#TODO: move legends so they don't disrupt the alignment in cowplot!!!
plot_composite <- function(..., alignment=NA, common_var=NA, order=NA) {
  chart_args_list <- list(...)

  #Charts are spatially alignable if it passes through here (otherwise will report error and stop)
  check_combinable_composite(chart_args_list)

  #TODO: finish the infer functions for the charts in composite_todo
  all_x_vars <- unlist(sapply(chart_args_list, function(chart_args) {infer_x(chart_args)}))
  all_y_vars <- unlist(sapply(chart_args_list, function(chart_args) {infer_y(chart_args)}))

  all_vars <- c(all_x_vars, all_y_vars)
  all_vars[sapply(all_vars, is.null)] <- NULL

  #Determine the common variable name if not already specified
  #TODO: return an error if there is no common_var
  if (is.na(common_var)) {
    common_var <- names(table(all_vars)[table(all_vars) > (length(chart_args_list) - 1)])[1] #If there is more than one common_var, then just combine on the first one (x).
  }

  #TODO: remove assertion later
  #Assert the common_var is not null for my sake in implementing
  if (is.null(common_var) || is.na(common_var)) {
    stop('composite variable is not common. This is an error in the code. Please report.')
  }

  #Set the global limits
  limits <- get_limits(chart_args_list, common_var)

  #TODO: ?? put the below if statement into get_order() function!!
  #Make sure all categorical variables have the same order for the common_var
  if (!is.numeric(limits)) {
    #If order is not specified by user
    if (is.na(order)) {
      #TODO: don't have this as a helper function (it's not very long)
      order <- get_order(chart_args_list, common_var)
    }
    #Add any values that are in limits that are not in order!
    lapply(limits, function(limit_val) {
      if (!(limit_val %in% order)) {
        order <<- c(order, limit_val)
      }
    })

    #reorder the limits to have the correct ordering.
    limits <- limits[order(match(limits,order))]
  }

  #Decide on global alignment (if not already given)
  if (is.na(alignment)) {
    x_var_count <- sum(all_x_vars == common_var)
    y_var_count <- sum(all_y_vars == common_var)
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

  # Is the global alignment vertical?
  if (alignment == 'vertical' || alignment == 'v') {
    #For each chart, does the x_axis have the common var?
    lo_plots <- lapply(chart_args_list, function(chart_args) {
      y_arg <- infer_y(chart_args)

      #If no, rotate
      if (!is.null(y_arg) && y_arg == common_var) {
        do.call(plot_simple, args = c(chart_args, list(flip_coord = TRUE))) #, y_limits=unlist(limits), rm_x_labels=TRUE)))
      }

      #If yes, do not rotate
      else {
        do.call(plot_simple, args = c(chart_args, list(x_limits=unlist(limits)))) #, rm_x_labels=TRUE)))
      }
    })

    #arrange vertically
    cowplot::plot_grid(plotlist = lo_plots, ncol = 1, align = "v")

    #Is the alignment horizontal?
  } else if (alignment == 'horizontal' || alignment == 'h') {

    #Generate each chart accordingly (with rotations if necessary)
    lo_plots <- lapply(chart_args_list, function(chart_args) {
      y_arg <- infer_y(chart_args)
      if (!is.null(y_arg) && y_arg == common_var) {
        do.call(plot_simple, args = c(chart_args, list(y_limits=unlist(limits)))) #, rm_y_labels=TRUE)))
      } else {
        do.call(plot_simple, args = c(chart_args, list(flip_coord = TRUE, x_limits=unlist(limits)))) #, rm_y_labels=TRUE)))
      }
    })
    #Arrange horizontally
    cowplot::plot_grid(plotlist = lo_plots, nrow = 1, align = "h")
  }
  #Other alignments (not implemented overlay)
  #TODO: implement overlay option
  else if (alignment == 'overlay' || alignment == 'o') {
    # stop('overlay has not been implemented yet!')
  }
}

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
#          # "divergent_bar" = render_divergent_bar_chart(data, title, colour_var, colour_scale),
#          "line" = geom_line(), #TODO: assuming no groups
#          # "heat_map" = render_heatmap(data, x, y, z, title, colour_var, colour_scale),
#          # "heatmap" = render_heatmap(data, x, y, z, title, colour_var, colour_scale),
#          "density" = stat_density_2d(aes(fill = ..level..), geom = "polygon"),
#          "scatter" = geom_point(),
#          # "pie" = render_pie_chart(data, x, title, colour_var, colour_scale),
#          # "histogram" = geom_histogram(), #TODO: Note: histogram doesn't have y axis in data (is computed)
#          # "pdf" = render_pdf(data, x, title, colour_var, colour_scale),
#          "boxplot" = geom_boxplot(),
#          "box_plot" = geom_boxplot(),
#          "violin" = geom_violin(),
#          "swarm" = ggbeeswarm::geom_beeswarm()
#   )
# }

#TODO: allow for linking in other ways than color (WHAT OTHER WAYS?)
#TODO: decide on color palette for different types of data (discrete and continuous) (DO IN MEETING)
#TODO: use common color scale legend (link_var legend) [do this in the future but not yet]
#TODO: add manual colour options to non-common_statistical charts
#TODO: only works on common statistical functions currently!
#TODO: make sure that statistical transformations on colour_limits are performed if the colour_scale is transformed when generating chart.
#'Many Types Linked
#'
plot_many_linked <- function(link_var, link_mark_type="default", ...) {
  specified_charts <- list(...)
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
    do.call(plot_simple, args = c(chart, list(colour_var = link_var,
                                              colour_scale = colour_limits,
                                              colour_mark_type = link_mark_type)))
  })

  arrange_plots(plots, labels = "AUTO")
}

#'render plots
#'
#'@param chart_list A list of charts
#'
#'@export
#TODO: TEST THESE!!! THESE HAVE NOT BEEN TESTED
#TODO: Make a list of all of the charts and how they are converted into a grob in a spreadsheet!!!
arrange_plots <- function(chart_list, labels = NULL, shared_legend=FALSE) {

  chart_list <- lapply(chart_list, function(chart) {
    if('gg' %in% class(chart)) {
      ggplotify::as.grob(chart)
    } else if ('data.frame' %in% class(chart)){
      multipanelfigure::capture_base_plot(chart)
    } else if ('htmlwidget' %in% class(chart)) {
      grid::grid.grabExpr(print(chart))
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
