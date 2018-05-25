
#'@import ggplot2
#'@import magrittr
#'@importFrom graphics plot
NULL

#Standard Bar Chart
plot_bar_chart <- function(data, x, y) {
  if (missing(y)) {
    ggplot(data, aes(x=x, y=..count..)) + geom_bar()
  }
  else {
    ggplot(data, aes(x=x, y=y)) + geom_bar(stat="identity")
  }
}

# Stacked Bar chart
plot_stacked_bar_chart <- function(data, x, fill) {
  ggplot(data, aes(x=x)) +
    geom_bar(aes(fill=fill), position="fill")
}

# Line Chart
plot_line_chart <- function(data, x, y) {
  ggplot(data, aes(x, y)) + geom_line()
}

# Heatmap
plot_heatmap <- function(data, x, y, fill) {

  if (missing(fill)) {
    data_as_df <- data %>% dplyr::group_by(data$x, data$y) %>% dplyr::tally()
    data_as_mat <- reshape2::dcast(data_as_df, y ~ x, value.var="n")

    # If there are no occurences of x,y together, then make their value 0 in the matrix. I do not forsee a problem with this, since it is only done when the fill value is count and the count of no occurences together is 0.
    data_as_mat[is.na(data_as_mat)] <- 0
    rownames(data_as_mat) <- data_as_mat$y
    my_data <- data_as_mat %>% dplyr::select(-y)
  }

  pheatmap::pheatmap(
    mat               = my_data,
    cluster_rows      = FALSE,
    cluster_cols      = FALSE,
    # color           = inferno(10),
    # border_color    = NA,
    # show_colnames   = FALSE,
    # show_rownames   = FALSE,
    fontsize_row      = 8,
    fontsize_col      = 8,
    main              = "Heatmap Title"
  )
}

# Divergent Bar chart
plot_divergent_bar_chart <- function(data) {
  likert_data <- likert::likert(data)
  plot(likert_data)
}

# Density chart
plot_density_chart <- function(data, x, y) {
  ggplot(data, aes(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")
}

# Scatter plot
plot_scatter <- function(data, x, y) {
  ggplot(data, aes(x=x, y=y)) +
    geom_point()
}

# Pie chart
plot_pie_chart <- function(data, group, value) {
  ggplot(data, aes(x="", y=value, fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0)
}

# Venn Diagrams
plot_venn <- function(num_circles, area1, area2, area3, cross_area, overlap12, overlap23, overlap13, overlap123, category_names) {
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
plot_histogram <- function(data, x, binwidth) {
  if (missing(binwidth)) {
    ggplot(data, aes(x)) + geom_histogram()
  }
  else {
    ggplot(data, aes(x)) + geom_histogram(binwidth = binwidth)
  }
}

# Probability Density Function
plot_pdf <- function(data, x) {
  ggplot(data, aes(x)) + geom_density(kernel = "gaussian")
}

# Boxplot
plot_boxplot <- function(data, x, y) {
  ggplot(data, aes(x,y)) + geom_boxplot()
}

# Violin Plot
plot_violinplot <- function(data, x, y) {
  ggplot(data, aes(x,y)) + geom_violin()
}

# Swarm Plot
plot_swarm_plot <- function(data, x, y) {
  ggplot(data, aes(x, y)) + ggbeeswarm::geom_beeswarm()
}
