#'@import ggplot2
#'@import magrittr
#'@importFrom graphics plot
NULL

#TODO: create a default axis labels rotation based on windows size.
#TODO: clean the ggplot code to reduce repetitive code (ex. colour_scale)?
#TODO: change colour_scale to colour_limits

#Standard Bar Chart or Stacked Bar Chart (using stack_by)
#TODO: Do we want option for grouping bars?
#TODO: Decide on if we should split this into render_bar and render_divergent and render_stacked to make it easier for the user

#TODO: if colour_var, then change stack_by to colour_var and warn user
#TODO: smart error messages for weird input values
#TODO: error messages for stack_by and no reference vector
#TODO: Maybe make this shorter by making generic cases and splitting up ggplot2 functions

#NOTE: I combined this with render_stacked_bar chart (AP can use stack_by if they want to make a stacked bar chart)
#var types - x = D, y = C
render_bar_chart <- function(data, x, y=NA, stack_by=NA, layout="default",
                             proportional=FALSE,
                             reference_vector, reference_var, title=NA,
                             flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
                             default_colour_var=NULL, colour_scale=NA,
                             x_limits=NA, y_limits=NA) {

  gg_chart <- if (layout == "divergent") {
    if (is.na(stack_by)) {
      #CASE 1: Divergent bar chart (waterfall)
      #If reference_var and reference_Vector is missing, I assume the data frame values are already set up in the proper format (split into positive and negative values.)
      if (!missing(reference_var) && !missing(reference_vector)) {
        data[[y]] <- ifelse(data[[reference_var]] %in% reference_vector, -data[[y]], data[[y]])
      }
      data <- dplyr::arrange(data, desc(value))
      ggchart <- ggplot(data, aes_string(x=x, y=y)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(limits = data[[x]])
      # coord_cartesian(ylim = c(-40, 40))
      # scale_y_continuous(expression(log[10](italic("LDA score"))),
      #                  breaks = -6:6, limits = c(-6, 6)) +
    } else {
      if (proportional == TRUE) {
        # CASE 7: Stacked divergent proportional bar chart
        if (missing(reference_vector)) {
          stop("Missing required input: reference_vector. This is required when specifying a bar chart that is stacked,
               divergent and proportional")
        }
        ggchart <- ggplot(data,
                          aes_string(x = x,
                                     y = ifelse(data[[stack_by]] %in%
                                                  reference_vector,
                                                -data[[y]],
                                                data[[y]]),
                                     fill = stack_by)) +
          geom_col(position="fill") +
          coord_flip()
        } else {
          #CASE 2: Stacked divergent bar
          if (missing(reference_vector)) {
            stop("Missing required input: reference_vector. This is required when specifying a bar chart that is stacked and
                 divergent")
          }
          ggchart <- ggplot(data,
                            aes_string(x = x,
                                       y = ifelse(data[[stack_by]] %in%
                                                    reference_vector,
                                                  -data[[y]],
                                                  data[[y]]),
                                       fill = stack_by)) +
            geom_col() +
            coord_flip()
          }
    }
  } else if (layout == "default") {

    if (is.na(y) && is.na(stack_by)) {
      #CASE 3: Bar Chart with y as count (geom_bar)
      gg_chart <-
        ggplot(data, aes_string(x=x)) +
        geom_bar()
    } else {
      if (isTRUE(proportional) | !is.na(y) & y==1) {
        #CASE 6: Stacked proportional bar chart
        gg_chart <- ggplot(data, aes_string(x=x, y=1, fill=stack_by)) +
          geom_bar(stat="identity", position="fill") +
          scale_y_continuous(labels = scales::percent_format())
      } else {
        if(!is.na(stack_by)) {
          if (is.na(y) | y==1) {
            #CASE 8: Stacked bar chart with fill as stack_by and y as count
            gg_chart <- ggplot(data, aes_string(x=x)) +
              geom_bar(aes_string(fill=stack_by))
          } else {
            #CASE 5: Stacked bar chart with fill as stack_by
            gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
              geom_col(aes_string(fill=stack_by))
          }
        } else {
          #CASE 4: Bar Chart (geom_col)
          gg_chart <-
            ggplot(data, aes_string(x = x, y = y)) +
            geom_col()
          # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        }
      }
    }
  } else {
    #CASE when divergent is not TRUE or FALSE (false is default)
    stop("When specifying a bar chart, layout input must be: 'default' or 'divergent'.")
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

  if(!is.null(default_colour_var)) {
    #TODO: I added width=0.9 here because of a case with x as time, the columns overlap... and this fixes the problem.
    #   BUT it might be unneeded and maybe annoying in other cases so may have to change this later to a case basis
    gg_chart <- gg_chart %+% geom_bar(aes_string(fill = default_colour_var), width = 0.9)

    if(!is.na(colour_scale)[1]) {
      gg_chart <- gg_chart +
        scale_fill_manual(name = default_colour_var, values = colour_scale)
      # theme(legend.position = "none")
    }
  }


  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
  }
  gg_chart
}



#BELOW IS THE OLD BAR CHART BEFORE ADDING DIVERGENT!!!
# render_bar_chart <- function(data, x, y=NA, stack_by=NA, title=NA,
#                            flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
#                            colour_var=NULL, colour_scale=NA,
#                            x_limits=NA, y_limits=NA) {
#
#   #TODO: Add a colour_var and if it is present, when making the ggplot, stacked or colour with the colour_var
#   #In this case, the programmer can't specify a stack_by and a linking_var.
#   #Most of the time, they really mean to just have a linking_var so this will be the default.
#   if(!is.na(stack_by) && !is.na(colour_var)) {
#     warning("stack_by is masked by link_var in stacked bar chart")
#     gg_chart <- ggplot(data, aes_string(x=x)) +
#       geom_bar(aes_string(fill=colour_var), position="fill") +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#   } else if(!is.na(stack_by) && is.na(colour_var)) {
#     gg_chart <- ggplot(data, aes_string(x=x)) +
#       geom_bar(aes_string(fill=stack_by), position="fill") +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#   } else if (is.na(y)) {
#     gg_chart <-
#       ggplot(data, aes_string(x=x)) +
#       geom_bar() +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#   } else {
#     gg_chart <-
#       ggplot(data, aes_string(x = x, y = y)) +
#       geom_col() +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#   }
#
#   if(!is.na(title)) {
#     gg_chart <- gg_chart + ggtitle(title)
#   }
#
#   if(flip_coord) {
#     gg_chart <- gg_chart + coord_flip()
#   }
#
#   if(rm_x_labels) {
#     gg_chart <- gg_chart +
#       theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
#   }
#
#   if(rm_y_labels) {
#     gg_chart <- gg_chart +
#       theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
#   }
#
#   if(!is.na(colour_scale)[1]) {
#     gg_chart <- gg_chart %+% aes_string(fill = colour_var)
#     gg_chart <- gg_chart +
#       scale_fill_manual(name = colour_var, values = colour_scale)
#       # theme(legend.position = "none")
#   }
#
#   if(!is.na(x_limits)[1]) {
#     gg_chart <- gg_chart + xlim(x_limits)
#   }
#
#   if(!is.na(y_limits)[1]) {
#     gg_chart <- gg_chart + ylim(y_limits)
#   }
#
#   gg_chart
# }

# Note- is now part of render_bar_chart
# Stacked Bar chart
# render_stacked_bar_chart <- function(data, x, fill=NA, title=NA, colour_var=NULL, colour_scale=NA) {

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
# x and y normally continuous but can have discrete (bivariate)
render_line_chart <- function(data, x, y, group, title, default_colour_var=NULL, colour_scale=NA,
                              x_limits=NA, y_limits=NA, flip_coord=FALSE) {
  if(is.na(group)){
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = 1)) + geom_line()
  } else {
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = group)) + geom_line(aes_string(colour = group))
  }

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = default_colour_var)
  }

  if (!is.na(colour_scale)[1]) {
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
  }


  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }

  # if(rm_x_labels) {
  #   gg_chart <- gg_chart +
  #     theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  # }
  #
  # if(rm_y_labels) {
  #   gg_chart <- gg_chart +
  #     theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  # }

  #TODO: make this a generic function that changes the default_colour_var !!!
  # if(!is.null(reencodement)) {
  #
  #   lapply(reencodement, function(reencode_specs) {
  #     reencode_var <- reencode_specs[["reencode_var"]]
  #     mark_type <- reencode_specs[["mark_type"]]
  #     channel <- reencode_specs[["channel"]]
  #
  #     #Could have this as a setter method
  #     if (mark_type == "default") {
  #       mark_type <- "area"
  #     }
  #
  #     #could have this check in the setter method
  #     if (channel != "colour") {
  #       "have not implemented channels that are not colour for reencoded marks yet!"
  #     }
  #
  #     gg_chart <<- gg_chart %+% aes_string(color=reencode_var)
  #
  #   })
  # }

  gg_chart
}

# Heatmap
# x and y always discrete
#TODO: Discuss what to do for NA values (will just not have a tile right now.)
render_heatmap <- function(data, x, y, z, title, default_colour_var=NULL, colour_scale=NA,
                           x_limits=NA, y_limits=NA, flip_coord=FALSE,
                           rm_x_labels=FALSE, rm_y_labels=FALSE) {
  gg_chart <- ggplot(data, aes_string(x, y, fill = z)) +
    geom_tile() +
    theme(legend.position="bottom")

  #To scale colour (called from many_types_linked and small_multiple)
  if (!is.na(colour_scale)[1]) {
    if (default_colour_var != z && !(is.null(default_colour_var))) {
      warning("z is masking link_var because link_var and z have to be the same for a heat_map when linking with colour")
    }
    get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))
    colr_pal <- get_palette(abs(diff(colour_scale)))
    gg_chart <- gg_chart +
      scale_fill_gradientn(colours = colr_pal, limits = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
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

# # Divergent Bar chart
# # Note - bar chart might not be categorical in all cases, can also be a continous value
# #        consider expanding functionality.
# render_divergent_bar_chart <- function(data, title, default_colour_var=NULL, colour_scale=NA) {
#   #TODO: ask Ana is she has time to change this or if I should
#   #TODO: add title option
#   likert_data <- likert::likert(data)
#   plot(likert_data)
# }

# Density chart
#TODO: Test with real dataset to see what you want this to do
#TODO: allow many_linked with colour_scale and default_colour_var depending on what you decide with dataset
#x & y always continuous
render_density_chart <- function(data, x, y, title, default_colour_var=NULL, colour_scale=NA,
                                 x_limits=NA, y_limits=NA, flip_coord=FALSE,
                                 rm_x_labels=FALSE, rm_y_labels=FALSE) {
  gg_chart <- ggplot(data, aes_string(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = ..level..)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
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

  gg_chart
}

# Scatter plot
# TODO: include geom_jitter?
render_scatter <- function(data, x, y, title, default_colour_var=NULL, colour_scale=NA,
                           x_limits=NA, y_limits=NA, flip_coord=FALSE,
                           rm_x_labels=FALSE, rm_y_labels=FALSE) {

  gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
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

  gg_chart
}

# Pie chart
render_pie_chart <- function(data, x, title, default_colour_var=NULL, colour_scale=NA) {

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

  if(!is.null(default_colour_var)) {
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  gg_chart
}

# Venn Diagrams
# TODO: change input to (data, category, scope) [see examples_obsandGenotype]
# TODO: show Ana changed input and then implement for 3 circles
render_venn <- function(num_circles, area1, area2, area3, cross_area, overlap12, overlap23, overlap13, overlap123, category_names) {
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
render_histogram <- function(data, x, title, default_colour_var=NULL, colour_scale=NA,
                             x_limits=NA, flip_coord=FALSE, rm_x_labels=FALSE) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_histogram()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    #Add manual colour scale
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }

  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  gg_chart
}

# Probability Density Function
render_pdf <- function(data, x, title, default_colour_var=NULL, colour_scale=NA,
                       x_limits=NA, flip_coord=FALSE, rm_x_labels=FALSE) {
  gg_chart <- ggplot(data, aes_string(x)) + geom_density(kernel = "gaussian")

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }
  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  gg_chart
}

# Boxplot
render_boxplot <- function(data, x, y, title, rm_y_labels=F, rm_x_labels=F,
                           default_colour_var=NULL, colour_scale=NA,
                           x_limits=NA, y_limits=NA, flip_coord=FALSE) {
  gg_chart <- ggplot(data, aes_string(x,y)) + geom_boxplot()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(rm_y_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
  }

  if(flip_coord) {
    gg_chart <- gg_chart + coord_flip()
  }

  gg_chart
}

#Not a part of GEViT
# # Violin Plot
# render_violinplot <- function(data, x, y, title, default_colour_var=NULL, colour_scale=NA,
#                               x_limits=NA, y_limits=NA, flip_coord=FALSE,
#                               rm_x_labels=FALSE, rm_y_labels=FALSE) {
#   gg_chart <- ggplot(data, aes_string(x,y)) + geom_violin()
#
#   if(!is.na(title)) {
#     gg_chart <- gg_chart + ggtitle(title)
#   }
#
#   if(!is.null(default_colour_var)) {
#     #Add colour variable
#     gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
#   }
#
#   if(!is.na(colour_scale)[1]) {
#     gg_chart <- gg_chart +
#       scale_fill_manual(name = default_colour_var, values = colour_scale)
#     # theme(legend.position = "none")
#   }
#
#   if(!is.na(x_limits)[1]) {
#     gg_chart <- gg_chart + xlim(x_limits)
#   }
#
#   if(!is.na(y_limits)[1]) {
#     gg_chart <- gg_chart + ylim(y_limits)
#   }
#
#   if(flip_coord) {
#     gg_chart <- gg_chart + coord_flip()
#   }
#
#   if(rm_x_labels) {
#     gg_chart <- gg_chart +
#       theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
#   }
#
#   if(rm_y_labels) {
#     gg_chart <- gg_chart +
#       theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
#   }
#
#   gg_chart
# }

# Swarm Plot
render_swarm_plot <- function(data, x, y, title, default_colour_var=NULL, colour_scale=NA,
                              x_limits=NA, y_limits=NA, flip_coord=FALSE,
                              rm_x_labels=FALSE, rm_y_labels=FALSE) {
  gg_chart <- ggplot(data, aes_string(x, y)) + ggbeeswarm::geom_beeswarm()

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
  }

  if(!is.null(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  #TODO: put this inside of if(!is.null(default_colour_var)) check for all of the charts!!!
  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  if(!is.na(y_limits)[1]) {
    gg_chart <- gg_chart + ylim(y_limits)
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

  gg_chart
}
