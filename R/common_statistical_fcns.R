
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
plot_stacked_bar_chart <- function(data, x, fill, facet_by) {
  gg_chart <- ggplot(data, aes_string(x=x)) +
    geom_bar(aes(fill=fill), position="fill")

  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Line Chart
plot_line_chart <- function(data, x, y, facet_by) {
  gg_chart <- ggplot(data, aes_string(x, y)) + geom_line()
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Heatmap
# facet_plot is a helper function for plot_heatmap...
# can place in plot_heatmap function but would like to have it out for now otherwise it's too confusing
create_hm <- function(x, fill) {
  if (missing(fill)) {
    data_as_df <- x %>% dplyr::group_by_("x_col", "y_col") %>% dplyr::tally()
    data_as_mat <- reshape2::dcast(data_as_df, paste("y_col", "~", "x_col"), value.var="n")
    data_as_mat[is.na(data_as_mat)] <- 0
    rownames(data_as_mat) <- data_as_mat$"y_col"
    my_data <- data_as_mat %>% dplyr::select(-"y_col")
  }

  hm <- pheatmap::pheatmap(
    mat               = my_data,
    cluster_rows      = FALSE,
    cluster_cols      = FALSE,
    fontsize_row      = 8,
    fontsize_col      = 8,
    main              = "Heatmap Title")
  hm$gtable
}

plot_heatmap <- function(data, x, y, fill, facet_by){
  if (!missing(fill) && !missing(facet_by)){
    stop("The feature fill and facet_by has not been implemented !!!")
  }
  if(missing(facet_by)) {
    create_hm(data, fill)
  }
  else {
    facet_dat <- lapply(unique(data[[facet_by]]), function(x) {print(filter(data, group == x))})
    all_plots <- lapply(facet_dat, FUN=create_hm)
    plot_many_types_general(all_plots)
  }
}

# Divergent Bar chart
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
plot_pie_chart <- function(data, group, value, facet_by) {
  gg_chart <- ggplot(data, aes(x="", y=value, fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0)
  if (!missing(facet_by)) {
    gg_chart <- gg_chart + facet_wrap(facet_by)
  }
  gg_chart
}

# Venn Diagrams
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
