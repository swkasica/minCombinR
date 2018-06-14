
#'@import ggplot2
#'@import magrittr
#'@importFrom graphics plot
NULL

#Standard Bar Chart
#TODO: Do we want option for grouping bars?
plot_bar_chart <- function(data, x, y, title) {
  if (is.na(y)) {
    gg_chart <-
      ggplot(data, aes_(x=as.name(x)), aes(y=..count..)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  else {
    gg_chart <-
      ggplot(data, aes_(x=as.name(x), y=as.name(y))) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Stacked Bar chart
# Notes :
# - Fill is the relevant variable, but confusing terminology / context
# - set default colour scale to something else?
plot_stacked_bar_chart <- function(data, x, fill, title) {
  gg_chart <- ggplot(data, aes_string(x=x)) +
    geom_bar(aes_string(fill=fill), position="fill")

  if (!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Line Chart
# Note  - added group features to resolve an error
plot_line_chart <- function(data, x, y, group, title) {
  if(is.na(group)){
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = 1)) + geom_line()
  } else {
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = group)) + geom_line(aes_string(colour = group))
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Heatmap
# In this function, I am assuming the data input is a matrix with numeric values in the cells and a column for facetting
#TODO: shorten the if statements for breaks (mostly using breaks for small multiples)
plot_heatmap <- function(data, title, breaks=NA) {
  print(breaks)
  if (is.na(breaks)) {
    hm <- pheatmap::pheatmap(
      mat               = data,
      cluster_rows      = FALSE,
      cluster_cols      = FALSE,
      fontsize_row      = 8,
      fontsize_col      = 8,
      main              = title)
    return(hm$gtable)
  } else {
    hm <- pheatmap::pheatmap(
      mat               = data,
      cluster_rows      = FALSE,
      cluster_cols      = FALSE,
      fontsize_row      = 8,
      fontsize_col      = 8,
      breaks            = breaks,
      color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(length(breaks)),
      main              = title)
    return(hm$gtable)
  }

    # all_plots <- lapply(facet_dat,
    #                     function(x) create_hm(select(x, -one_of(facet_by)), unique(x[[facet_by]])))

}

# Divergent Bar chart
# Note - bar chart might not be categorical in all cases, can also be a continous value
#        consider expanding functionality.
plot_divergent_bar_chart <- function(data, title) {
  #TODO: ask Ana is she has time to change this or if I should
  #TODO: add title option
  likert_data <- likert::likert(data)
  plot(likert_data)
}

# Density chart
plot_density_chart <- function(data, x, y, title) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Scatter plot
# TODO: include geom_jitter?
plot_scatter <- function(data, x, y, title) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Pie chart
# Note  - instead of group, consider just using x_var like other functions?
plot_pie_chart <- function(data, group, title) {

  #due to summarization step, need to group by the facet too in order for this to work
  #might also want to make these frequencies
  # if (!is.na(facet_by)) {
  #   data <- data %>%
  #     count_(c(group,facet_by))%>%
  #     group_by_(facet_by)%>%
  #     mutate(freq = n/sum(n))
  #
  #   gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=group)) +
  #     geom_bar(width = 1, stat = "identity") +
  #     coord_polar("y", start=0)+
  #     facet_wrap(facet_by)
  # }else{
  data <- data%>%
    count_(group)%>%
    mutate(freq = n/sum(n))

  gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0)
  # }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Venn Diagrams
# TODO: change input to (data, category, scope) [see examples_obsandGenotype]
# TODO: show Ana changed input and then implement for 3 circles
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
#TODO: decide to remove or keep binwidth
plot_histogram <- function(data, x, binwidth, title) {
  if (is.na(binwidth)) {
    gg_chart <- ggplot(data, aes_string(x)) + geom_histogram()
  }
  else {
    gg_chart <- ggplot(data, aes_string(x)) + geom_histogram(binwidth = binwidth)
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Probability Density Function
plot_pdf <- function(data, x, title) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_density(kernel = "gaussian")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Boxplot
plot_boxplot <- function(data, x, y, title) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_boxplot()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Violin Plot
plot_violinplot <- function(data, x, y, title) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_violin()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}

# Swarm Plot
plot_swarm_plot <- function(data, x, y, title) {
  gg_chart <- ggplot(data, aes_string(x, y)) + ggbeeswarm::geom_beeswarm()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  gg_chart
}
