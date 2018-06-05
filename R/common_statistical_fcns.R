
#'@import ggplot2
#'@import magrittr
#'@importFrom graphics plot
NULL

#Standard Bar Chart
plot_bar_chart <- function(data, x, y, facet_by) {
  if (missing(y)) {
    gg_chart <- ggplot(data, aes_(x=as.name(x)), aes(y=..count..)) + geom_bar()
  }
  else {
    gg_chart <- ggplot(data, aes_(x=as.name(x), y=as.name(y))) + geom_bar(stat="identity")
  }

  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }

  gg_chart
}

# Stacked Bar chart
# Notes :
# - Fill is the relevant variable, but confusing terminology / context
# - set default colour scale to something else?
plot_stacked_bar_chart <- function(data, x, fill, facet_by) {
  gg_chart <- ggplot(data, aes_string(x=x)) +
    geom_bar(aes_string(fill=fill), position="fill")

  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Line Chart
# Note  - added group features to resolve an error
plot_line_chart <- function(data, x, y, group,facet_by) {
  if(missing(group)){
    gg_chart <- ggplot(data, aes_string(x, y,group=1)) + geom_line()
  }else{
    gg_chart <- ggplot(data, aes_string(x=x, y=y,group=group)) + geom_line(aes_string(colour = group))
  }

  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Heatmap
# create_hm is a helper function for plot_heatmap...
# can place in plot_heatmap function but would like to have it out for now otherwise it's too confusing
# Note - would be good to discuss further, this is a note quite what most people need in a heatmap. Made modes

#@param x data frame for plotting
#@param facet logical
create_hm <- function(dat, x, y,fill) {
  # if (!missing(x) && !missing(y)) {
  #   data_as_df <- dat %>% dplyr::group_by_(x, y) %>% dplyr::tally()
  #   data_as_mat <- reshape2::dcast(data_as_df, paste(y, "~", x), value.var="n")
  #   data_as_mat[is.na(data_as_mat)] <- 0
  #   rownames(data_as_mat) <- data_as_mat[[y]]
  #   dat <- data_as_mat %>% dplyr::select(-y)
  # }

  tmp<-data.matrix(dat,rownames.force = TRUE)

  hm <- pheatmap::pheatmap(
    mat               = dat,
    cluster_rows      = FALSE,
    cluster_cols      = FALSE,
    fontsize_row      = 8,
    fontsize_col      = 8,
    main              = "Heatmap Title")
  hm$gtable
}

#' Plot a heatmap
#' @param data A data frame
#' @param x optional categorical variable for x-axis if quantitative value attribute has not been calculated in data
#' @param y optional categorical variable for y-axis. If given (along with x_col), will calculate the number of occurences with x_var.
plot_heatmap <- function(data, x, y, facet_by){
  if(missing(x) && !missing(y) || !missing(x) && missing(y)) {
    stop("You must input BOTH or NEITHER a x_var and y_var.")
  }
  if (missing(x) && !missing(facet_by)){
    stop("The feature facet_by has not been implemented for a matrix input yet!!!")
  }

  if(missing(facet_by)) {
    if(missing(x) && missing(y)) {
      create_hm(data)
    } else {
      create_hm(data,x,y)
    }
  } else {
    facet_dat <- lapply(unique(data[[facet_by]]), function(x) {dplyr::filter(data, group == x)})
    all_plots <- lapply(facet_dat, FUN=create_hm, x, y)
    combine_many_types_general(all_plots)
  }
}

# Divergent Bar chart
# Note - bar chart might not be categorical in all cases, can also be a continous value
#        consider expanding functionality.
plot_divergent_bar_chart <- function(data, facet_by) {
  likert_data <- likert::likert(data)
  plot(likert_data)
}

# Density chart
plot_density_chart <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Scatter plot
plot_scatter <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point()
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Pie chart
# Note  - instead of group, consider just using x_var like other functions?
plot_pie_chart <- function(data, group,facet_by) {

  #due to summarization step, need to group by the facet too in order for this to work
  #might also want to make these frequencies
  if (!missing(facet_by)) {
    data <- data %>%
      count_(c(group,facet_by))%>%
      group_by_(facet_by)%>%
      mutate(freq = n/sum(n))

    gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=group)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0)+
      facet_wrap(facet_by)
  }else{
    data <- data%>%
      count_(group)%>%
      mutate(freq = n/sum(n))

    gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=group)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0)
  }

  gg_chart
}

# Venn Diagrams
# TODO: change input to (data, category, scope) [see examples_obsandGenotype]
plot_venn <- function(num_circles, area1, area2, area3, cross_area, overlap12, overlap23, overlap13, overlap123, category_names, facet_by) {
  if (num_circles == 2) {
    grid::grid.newpage()
    VennDiagram::draw.pairwise.venn(area1, area2, cross_area, category_names)
  } else if (num_circles == 3) {
    grid::grid.newpage()
    VennDiagram::draw.triple.venn(area1=area1, area2=area2, area3=area3,
                                  n12=overlap12, n23=overlap23, n13=overlap13, n123=overlap123,
                                  category = category_names)
  }

}

# Histogram
#TODO: decide to remove or keep binwidth
plot_histogram <- function(data, x, binwidth, facet_by) {
  if (missing(binwidth)) {
    gg_chart <- ggplot(data, aes_string(x)) + geom_histogram()
  }
  else {
    gg_chart <- ggplot(data, aes_string(x)) + geom_histogram(binwidth = binwidth)
  }
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Probability Density Function
plot_pdf <- function(data, x, facet_by) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_density(kernel = "gaussian")
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Boxplot
plot_boxplot <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_boxplot()
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Violin Plot
plot_violinplot <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_violin()
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Swarm Plot
plot_swarm_plot <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x, y)) + ggbeeswarm::geom_beeswarm()
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}
