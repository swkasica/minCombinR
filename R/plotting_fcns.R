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
plot_simple <- function(chart_type, data, x_var=NA, y_var=NA, fill=NA, group=NA, title=NA, breaks=NA,
                        path, category, cluster_var, comparisons) {
  #TODO: alphabetize?
  all_chart_types <-  c(#common statistical
                        "bar", "stacked_bar","divergent_bar", "line",
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
         #Common Statistical Chart Types
         "bar" = plot_bar_chart(data, x_var, y_var, title),
         "stacked_bar" = plot_stacked_bar_chart(data, x_var, fill, title),
         "divergent_bar" = plot_divergent_bar_chart(data, title),
         "line" = plot_line_chart(data, x_var, y_var, group, title),
         "heat_map" = plot_heatmap(data, title, breaks),
         "density" = plot_density_chart(data, x_var, y_var, title),
         "scatter" = plot_scatter(data, x_var, y_var, title),
         "pie" = plot_pie_chart(data, group, title),
         "histogram" = plot_histogram(data, x_var, binwidth, title),
         "pdf" = plot_pdf(data, x_var, title),
         "boxplot" = plot_boxplot(data, x_var, y_var, title),
         "violin" = plot_violinplot(data, x_var, y_var, title),
         "swarm" = plot_swarm_plot(data, x_var, y_var, title),

         #Relational
         "node_link" = plot_node_link(data, directed),
         "flow_diagram" = plot_flow_diagram(data), #TODO

         #Temporal
         #"stream_graph" = plot_streamgraph(data, key, value, date), #TODO: change param names
         #"timeline" = plot_timeline(data, stacked, start, end, names, phase), #TODO: change input for stacked

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
  lapply(args_list, function(x) {do.call(plot_simple, x)})
}

#' Small multiples
#'
#'@param chart_type A string indicating type of chart to generate. Options are:
#'@param data A data frame
#'@param x_var optional for: ... required for: ...
#'@param y_var optional for: ... required for: ...
#'@param fill optional: ...
#'@param group optional: ... not applciable for : ...
#'@param facet_by
#'
#'@export
plot_small_multiples <- function(chart_type, data, facet_by, x_var=NA, y_var=NA, fill=NA, group=NA) {

  #TODO: DISCUSS IF SAME X_AXIS AND Y_AXIS SCALES IS NEEDED
  #TODO: decide if input is a list of args or the args as implemented currently
  #TODO: pre-calculate breaks for heatmap legend

  #Create a list of data subsets according to the facetting variable
  facet_dat <- lapply(unique(data[[facet_by]]),
                      function(x) {dplyr::filter_(data, paste(facet_by, "==", quote(x)))})

  #Create a list of plots for each of the facet_dat subsets
  all_plots <- lapply(facet_dat,
                      function(x) gevitR::plot_simple(chart_type = chart_type,
                                                      data = select(x, -facet_by),
                                                      x_var = x_var,
                                                      y_var = y_var,
                                                      fill = fill,
                                                      group = group,
                                                      breaks = seq(0, 9, by = 1)))
  all_plots
}

#'Layout plots
#'
#'@param chart_list A list of charts
#'
#'@export
layout_plots <- function(chart_list) {

  chart_list <- lapply(chart_list, function(x) {
    if('gg' %in% class(x)) {
      ggplotify::as.grob(x)
    }else {
      ggplotify::as.grob(x)
    }
  })
  cowplot::plot_grid(plotlist = chart_list, labels = "AUTO")
}

