
#'@import ggplot2
#'@import magrittr
#'@importFrom graphics plot
NULL

#TODO: create a default axis labels rotation based on windows size.
#TODO: clean the ggplot code to reduce repetitive code (ex. colour_scale)?

#Standard Bar Chart or Stacked Bar Chart (using stack_by)
#TODO: Do we want option for grouping bars?
#TODO: Do we want position="fill" or "stacked" stacked bar chart?
#NOTE: I combined this with plot_stacked_bar chart (AP can use stack_by if they want to make a stacked bar chart)
plot_bar_chart <- function(data, x, y=NA, stack_by=NA, title=NA,
                           flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
                           colour_var=NA, colour_scale=NA) {

  #TODO: Add a colour_var and if it is present, when making the ggplot, stacked or colour with the colour_var
  #In this case, the programmer can't specify a stack_by and a linking_var.
  #Most of the time, they really mean to just have a linking_var so this will be the default.
  if(!is.na(stack_by) && !is.na(colour_var)) {
    warning("stack_by is masked by link_var in stacked bar chart")
    gg_chart <- ggplot(data, aes_string(x=x)) +
      geom_bar(aes_string(fill=colour_var), position="fill") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if(!is.na(stack_by) && is.na(colour_var)) {
    gg_chart <- ggplot(data, aes_string(x=x)) +
      geom_bar(aes_string(fill=stack_by), position="fill") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else if (is.na(y)) {
    gg_chart <-
      ggplot(data, aes_(x=as.name(x)),
             aes(y=..count..)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    gg_chart <-
      ggplot(data, aes_(x = as.name(x), y = as.name(y))) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }

  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(rm_y_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart %+% aes_string(fill = colour_var)
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Note- is now part of plot_bar_chart
# Stacked Bar chart
# plot_stacked_bar_chart <- function(data, x, fill=NA, title=NA, colour_var=NA, colour_scale=NA) {

#   gg_chart <- ggplot(data, aes_string(x=x)) +
#     geom_bar(aes_string(fill=fill), position="fill")
#
#   if (!is.na(title)) {
#     gg_chart <- gg_chart + ggtitle(title)
#   }
#
#   if(!is.na(colour_scale)[1]) {
#     gg_chart <- gg_chart +
#       scale_fill_manual(name = colour_var, values = colour_scale)
#   }
#
#   gg_chart
# }

# Line Chart
plot_line_chart <- function(data, x, y, group, title, colour_var=NA, colour_scale=NA) {
  if(is.na(group)){
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = 1)) + geom_line()
  } else {
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = group)) + geom_line(aes_string(colour = group))
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = colour_var)
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Heatmap
# In this function, I am assuming the data input is a matrix with numeric values in the cells and a column for facetting
#TODO: Discuss what to do for NA values (will just not have a tile right now.)
plot_heatmap <- function(data, x, y, z, title, colour_var=NA, colour_scale=NA) {
    gg_chart <- ggplot(data, aes_string(x, y, fill = z)) +
      geom_tile()

  #To scale colour (called from many_types_linked and small_multiple)
  if (!is.na(colour_scale)[1]) {
    if (colour_var != z && !(is.na(colour_var))) {
      warning("z is masking link_var because link_var and z have to be the same for a heat_map when linking with colour")
    }
    get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))
    colr_pal <- get_palette(abs(diff(colour_scale)))
    gg_chart <- gg_chart +
      scale_fill_gradientn(colours = colr_pal, limits = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart

  #Keeping this here in case... for now will use ggplot to be consistent.
  # if (is.na(breaks)) {
  #   hm <- pheatmap::pheatmap(
  #     mat               = data,
  #     cluster_rows      = FALSE,
  #     cluster_cols      = FALSE,
  #     fontsize_row      = 8,
  #     fontsize_col      = 8,
  #     main              = title)
  #   return(hm$gtable)
  # } else {
  #   hm <- pheatmap::pheatmap(
  #     mat               = data,
  #     cluster_rows      = FALSE,
  #     cluster_cols      = FALSE,
  #     fontsize_row      = 8,
  #     fontsize_col      = 8,
  #     color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(length(breaks)),
  #     breaks            = breaks,
  #     main              = title)
  #   return(hm$gtable)
  # }

}

# Divergent Bar chart
# Note - bar chart might not be categorical in all cases, can also be a continous value
#        consider expanding functionality.
plot_divergent_bar_chart <- function(data, title, colour_var=NA, colour_scale=NA) {
  #TODO: ask Ana is she has time to change this or if I should
  #TODO: add title option
  likert_data <- likert::likert(data)
  plot(likert_data)
}

# Density chart
#TODO: Test with real dataset to see what you want this to do
#TODO: allow many_linked with colour_scale and colour_var depending on what you decide with dataset
plot_density_chart <- function(data, x, y, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }


  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = ..level..)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Scatter plot
# TODO: include geom_jitter?
plot_scatter <- function(data, x, y, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = colour_var)
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Pie chart
plot_pie_chart <- function(data, x, title, colour_var=NA, colour_scale=NA) {

  #due to summarization step, need to group by the facet too in order for this to work
  #might also want to make these frequencies
  # if (!is.na(facet_by)) {
  #   data <- data %>%
  #     count_(c(x,facet_by))%>%
  #     group_by_(facet_by)%>%
  #     mutate(freq = n/sum(n))
  #
  #   gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=x)) +
  #     geom_bar(width = 1, stat = "identity") +
  #     coord_polar("y", start=0)+
  #     facet_wrap(facet_by)
  # }else{
  data <- data %>%
    count_(x) %>%
    mutate(freq = n/sum(n))

  gg_chart <- ggplot(data, aes_string(x=shQuote(""), y="freq", fill=x)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0)
  # }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
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
#TODO: decide to add binwidth... should probably
plot_histogram <- function(data, x, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_histogram()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = colour_var)
    #Add manual colour scale
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Probability Density Function
plot_pdf <- function(data, x, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_density(kernel = "gaussian")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = colour_var)
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Boxplot
plot_boxplot <- function(data, x, y, title, flip_coord=F, rm_y_labels=F, rm_x_labels=F, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_boxplot()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }

  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(rm_y_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = colour_var)
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Violin Plot
plot_violinplot <- function(data, x, y, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_violin()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }


  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = colour_var)
    gg_chart <- gg_chart +
      scale_fill_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}

# Swarm Plot
plot_swarm_plot <- function(data, x, y, title, colour_var=NA, colour_scale=NA) {
  gg_chart <- ggplot(data, aes_string(x, y)) + ggbeeswarm::geom_beeswarm()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(color = colour_var)
    gg_chart <- gg_chart +
      scale_colour_manual(name = colour_var, values = colour_scale)
      # theme(legend.position = "none")
  }

  gg_chart
}
