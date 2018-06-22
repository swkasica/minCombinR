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

#This is used for many_types_linked and small_multiple (which is why I have it as a helper function)
get_colour_scales <- function(specified_charts, colour_var) {

  #TODO: To see the types_of_variables_for_charts go to: Dropbox\gevitR\scratch\shannah\Organization
  #Gets the limits (colour_scale) for a continuous colour_var
  if(specified_charts[[1]]$chart_type == "heat_map") {
    min_lim <- sapply(specified_charts, function(chart) {
      ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
      unique_link_var <- unique(ref_data[[colour_var]])
      min(unique_link_var)
    })

    max_lim <- sapply(specified_charts, function(chart) {
      ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
      unique_link_var <- unique(ref_data[[colour_var]])
      max(unique_link_var)
    })

    min_lim <- min(min_lim)
    max_lim <- max(max_lim)
    colour_scale <- c(min_lim, max_lim)
  }
  #Gets the colour-var-link (colour_scale) for a discrete colour_var
  else {
    print("!!!")
    colour_to_var_link <- data.frame()
    #TODO: instead of the longest, make sure you get ALL of the possible values (could be the case that the longest doesn't include everything!!!)
    lapply(specified_charts, function(chart) {
      ref_data <- get(as.character(chart$data)) #TODO: some sort of try catch here to make sure the linking variable can be found (for each chart)
      unique_link_var <- unique(ref_data[[colour_var]])
      if (length(unique_link_var) > length(colour_to_var_link)) {
        colour_to_var_link <<- unique_link_var
      }
    })
    #Gets the colour match for a discrete colour_var
    get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    colour_scale <- get_palette(length(colour_to_var_link))
    names(colour_scale) <- colour_to_var_link

    #Convert numeric colour_var to discrete
    tmp_dat <- get(as.character(specified_charts[[1]]$data))
    if (class(tmp_dat[[colour_var]]) == "numeric") {
      colour_var <- paste0("factor(", colour_var, ")")
    }
  }
  return(list(colour_var, colour_scale))
}

#'Plot a simple chart type
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
                        path, category, cluster_var, comparisons,
                        #FOR COMPOSITE
                        flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
                        #FOR MANY TYPES LINKED
                        colour_var=NA, colour_scale=NA) {
  #TODO: alphabetize?
  all_chart_types <-  c(#common statistical
                        "bar", "divergent_bar", "line", #"stack_by_bar",
                        "heat_map", "density", "scatter", "pie", "venn",
                        "histogram","pdf", "boxplot","violin", "swarm",
                        #relational
                        "node_link", "flow_diagram",
                        #temporal
                        "stream_graph", "timeline",
                        #spatial
                        "geographic_map", "choropleth", "interior_map",
                        #other
                        "table", "category_stripe", "image",
                        #genomic
                        "phylogenetic_tree", "dendrogram", "clonal_tree",
                        "linear_genomic_map", "radial_genomic_map", "alignment"
                        )
  check_valid_str(chart_type, all_chart_types)
  switch(chart_type,
         #Common Stat Chart Types
         "bar" = plot_bar_chart(data, x, y, stack_by, title, flip_coord, rm_y_labels, rm_x_labels, colour_var, colour_scale),
         # "stacked_bar" = plot_stacked_bar_chart(data, x, fill, title, colour_var, colour_scale),
         "divergent_bar" = plot_divergent_bar_chart(data, title, colour_var, colour_scale),
         "line" = plot_line_chart(data, x, y, group, title, colour_var, colour_scale),
         "heat_map" = plot_heatmap(data, x, y, z, title, colour_var, colour_scale),
         "density" = plot_density_chart(data, x, y, title, colour_var, colour_scale),
         "scatter" = plot_scatter(data, x, y, title, colour_var, colour_scale),
         "pie" = plot_pie_chart(data, x, title, colour_var, colour_scale),
         "histogram" = plot_histogram(data, x, title, colour_var, colour_scale),
         "pdf" = plot_pdf(data, x, title, colour_var, colour_scale),
         "boxplot" = plot_boxplot(data, x, y, title, flip_coord, rm_y_labels, rm_x_labels, colour_var, colour_scale),
         "violin" = plot_violinplot(data, x, y, title, colour_var, colour_scale),
         "swarm" = plot_swarm_plot(data, x, y, title, colour_var, colour_scale),

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
         "table" = plot_table(data),
         "category_stripe" = plot_category_stripe(data, category),
         "image" = plot_image(path), #TODO: maybe change if you use the magick package

         #genomic
         "phylogenetic_tree" = plot_phylo_tree(path), #path is a path to a nwk_file
         "dendrogram" = plot_dendro(data, cluster_var),
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
  layout_plots(all_plots)
}

#TODO: DISCUSS IF SAME X_AXIS AND Y_AXIS SCALES IS NEEDED... Maybe just continuous value
#TODO: If there is colour, make sure the colour_scale is the same for each of the facets.s?
#TODO: decide if input is a list of args or the args as implemented currently
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
plot_small_multiples <- function(chart_type, data, facet_by, x=NA, y=NA, z=NA, fill=NA, group=NA) {

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
                                                            group = group))
  layout_plots(all_plots)
}

#TODO: Currently assuming data is coming from the exact same source
# : AKA: this means no reordering or range adjustments required for x-axis
#TODO: move the charts closer together somehow
#TODO: handle reordering
#TODO: handle range adjustments (discrete and continuous)
#TODO: make rotate and alignment(?...) computed rather than specified
#TODO: allow more than 2 charts
#TODO: currently only works with ggplots !!! change for each chart_type that isn't derived from ggplot2
#TODO: consider case where the geoms are in the exact same chart (ex. hist and line together)
#'Composite
#'
plot_composite <- function(plot1_args, plot2_args, alignment = 'v', rotate1 = F, rotate2 = F) {
  if (alignment == 'v') {
    plot1 <- do.call(gevitR::plot_simple, c(plot1_args, flip_coord=rotate1, rm_y_labels=F, rm_x_labels=T))
    plot2 <- do.call(gevitR::plot_simple, c(plot2_args, flip_coord=rotate2))
    cowplot::plot_grid(plot1, plot2, ncol = 1, align = "hv")
  } else if (alignment == 'h') {
    plot1 <- do.call(gevitR::plot_simple, c(plot1_args, flip_coord=rotate1))
    plot2 <- do.call(gevitR::plot_simple, c(plot2_args, flip_coord=rotate2, rm_y_labels=T, rm_x_labels=F))
    cowplot::plot_grid(plot1, plot2, nrow = 1, align = "hv")
  } else {
    stop('Alignment can be one of "h" (horizontal) and "v" (vertical)')
  }

  #This fixes the distance between the plots problem but isn't using cowplot (uses grid and ggplotGrob) which might not work for non-ggplots.
  # library(grid)
  # grid.newpage()
  # grid.draw(rbind(ggplotGrob(simple_bar), ggplotGrob(simple_box), size = "last"))
}

#TODO: allow for linking in other ways than color
#TODO: decide on color palette for different types of data (discrete and continuous)
#TODO: use common color scale legend
#TODO: add manual colour options to non-statistical and non-ggplot basic charts
#'Many Types Linked
#'
plot_many_linked <- function(link_var, link_by="colour", ...) {
  specified_charts <- list(...)
  if (link_by == "colour" || link_by == "color") {
    colour_info <- get_colour_scales(specified_charts, link_var)

    plots <- lapply(specified_charts, function(chart) {
      do.call(plot_simple, args = c(chart, list(colour_var = colour_info[[1]], colour_scale = colour_info[[2]])))
    })
    gevitR::layout_plots(plots)
  }
}

#'Layout plots
#'
#'@param chart_list A list of charts
#'
#'@export
layout_plots <- function(chart_list) {

  chart_list <- lapply(chart_list, function(chart) {
    if('gg' %in% class(chart)) {
      ggplotify::as.grob(chart)
    }else {
      ggplotify::as.grob(chart)
    }
  })
  cowplot::plot_grid(plotlist = chart_list, labels = "AUTO")
}

