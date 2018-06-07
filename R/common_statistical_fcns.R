
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
plot_line_chart <- function(data, x, y, group, facet_by) {
  if(missing(group)){
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = 1)) + geom_line()
  }else{
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = group)) + geom_line(aes_string(colour = group))
  }

  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Heatmap
# In this function, I am assuming the data input is a matrix with numeric values in the cells and a column for facetting
plot_heatmap <- function(data, facet_by) {
  create_hm <- function(dat, title) {
    if (missing(title)) {
      title = NA
    }
    pheatmap::pheatmap(
      mat               = dat,
      cluster_rows      = FALSE,
      cluster_cols      = FALSE,
      fontsize_row      = 8,
      fontsize_col      = 8,
      main              = title)
    # hm$gtable
  }

  if (!missing(facet_by)) {
    facet_dat <- lapply(unique(data[[facet_by]]),
                        function(x) {dplyr::filter_(data, paste(facet_by, "==", quote(x)))})
    all_plots <- lapply(facet_dat,
                        function(x) create_hm(select(x, -one_of(facet_by)), unique(x[[facet_by]])))
  } else {
    all_plots <- list(create_hm(data))
  }
  return(all_plots)

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
plot_pie_chart <- function(data, group, facet_by) {

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
# TODO: add facetting option
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
