#' Rendering a bar chart
#' @title render_bar_chart
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @param ...
#'
#' @return
render_bar<- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #generate the chart
  gg_chart <- if (layout == "divergent") {
    #CASE 1: Divergent bar chart (waterfall)
    if (is.na(stack_by)) {
      #If reference_var and reference_Vector is missing,
      #Assume the data frame values are already set up in the proper format (split into positive and negative values.)
      if (!missing(reference_var) && !missing(reference_vector)) {
        data[[y]] <- ifelse(data[[reference_var]] %in% reference_vector, -data[[y]], data[[y]])
      }

      data <- dplyr::arrange(data, desc(value))

      ggchart <- ggplot(data, aes_string(x=x, y=y)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(limits = data[[x]])

    } else {
      if (proportional == TRUE) {
        # CASE 7: Stacked divergent proportional bar chart
        if (missing(reference_vector)) {
          stop("Missing required input: reference_vector. This is required when specifying a bar chart that is stacked,
               divergent and proportional")
        }

        ggchart <- ggplot(data,
                          aes_string(x = x,
                                     y = ifelse(data[[stack_by]] %in% reference_vector,
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
                                       y = ifelse(data[[stack_by]] %in% reference_vector,
                                                  -data[[y]],
                                                  data[[y]]),
                                       fill = stack_by)) +
            geom_col() + #main different from above above case
            coord_flip()
          }
    }
  } else if (layout == "default") {

    if (is.na(y) && is.na(stack_by)) {
      #CASE 3: Bar Chart with y as count (geom_bar)
      gg_chart <- ggplot(data, aes_string(x=x)) + geom_bar()
    } else {
      if (proportional | !is.na(y) & y==1) {
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

  if(!is.na(default_colour_var)) {
    #TODO: I added width=0.9 here because of a case with x as time, the columns overlap... and this fixes the problem.
    #   BUT it might be unneeded and maybe annoying in other cases so may have to change this later to a case basis
    gg_chart <- gg_chart %+% geom_bar(aes_string(fill = default_colour_var), width = 0.9)

    if(!is.na(colour_scale)[1]) {
      gg_chart <- gg_chart +
        scale_fill_manual(name = default_colour_var, values = colour_scale)
    }
  }


  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits,
                                     scale_y_cont = scale_y_cont,
                                     rm_x_labels = rm_x_labels,
                                     rm_y_labels = rm_y_labels)

  #return the faithful ggplot object
  return(gg_chart)
}


#' Render a pie chart
#' @title render_pie_chart
#' @import ggplot2
#' @import dplyr
#' @param ...
#'
#' @return
render_pie <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  data <- data %>%
    dplyr::count_(x) %>%
    dplyr::mutate(freq = n/sum(n))

  gg_chart <- ggplot2::ggplot(data, aes_string(x=shQuote(""), y="freq", fill=x)) +
    ggplot2::geom_bar(width = 1, stat = "identity") +
    ggplot2::coord_polar("y", start=0)

  if(!is.na(default_colour_var)) {
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title)

  return(gg_chart)
}



#' Rendering a line chart
#' @title render_line_chart
#' @param ...
#'
#' @import ggplot2
#' @return
#'
#' @examples
render_line <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  if(is.na(group)){
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = 1)) + geom_line()
  } else {
    gg_chart <- ggplot(data, aes_string(x = x, y = y, group = group)) +
      geom_line(aes_string(colour = group))
  }

  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = default_colour_var)
  }

  if (!is.na(colour_scale)[1]) {
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
  }


  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits)

  return(gg_chart)
}

#' Rendering a scatter plot
#'
#' @title render_scatter
#' @param ...
#'
#' @return
render_scatter <- function(...) {
  spec_list<-list(...)


  if(names(spec_list)[1] == ""){
    spec_list<-spec_list[-1]
  }

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  if(is.na(tree_dat)) {
    gg_chart <- ggplot(data, aes_string(x=x, y=y)) +
      geom_point()
  } else {
    tmp<-dplyr::filter(tree_dat,isTip == TRUE)
    gg_chart <- ggplot(tmp, aes_string(x=x, y='y')) +
      geom_point()
  }

  if(!is.na(tree_dat)) {
    #TODO: return warnings of overriding in this case
    gg_chart <- gg_chart +
      scale_x_discrete(na.translate=FALSE) +
      scale_y_continuous(breaks = sort(tree_dat$y),
                         labels = levels(tree_dat$id))+
      theme_bw()+
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(0,0,0,0),"points"),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle=90))

  }

  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(colour = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    #Scale colour variable
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits)
  return(gg_chart)
}


#' Render Histogram
#' @title render_histogram
#' @param ...
#'
#' @return

render_histogram<- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot(data, aes_string(x=x)) +
    geom_histogram()


  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    #Add manual colour scale
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     x_limits=x_limits,
                                     rm_x_labels= rm_x_labels)

  gg_chart
}

#' Render 1D probability density functions
#' @title render_1D_density
#' @param ...
#'
#' @return
render_1D_density <- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot(data, aes_string(x)) + geom_density(kernel = "gaussian")

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     x_limits=x_limits,
                                     rm_x_labels= rm_x_labels)


  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
  }

  return(gg_chart)
}

#' Rendering a box chart
#'
#' @title render_boxplot
#' @param ...
#'
#' @return
render_boxplot <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot2::ggplot(data = data, aes_string(x=x,y=y)) +
    geom_boxplot()

  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits,
                                     rm_x_labels= rm_x_labels,
                                     rm_y_labels=  rm_y_labels)

  gg_chart
}

#'Rendering a swarm plot
#'
#' @title render_swarm_plot
#' @param ...
#'
#' @return
render_swarm_plot <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot(data, aes_string(x, y)) + ggbeeswarm::geom_beeswarm()

  if(!is.na(default_colour_var)) {
    #Add colour variable
    gg_chart <- gg_chart %+% aes_string(fill = default_colour_var)
  }

  #TODO: put this inside of if(!is.na(default_colour_var)) check for all of the charts!!!
  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_colour_manual(name = default_colour_var, values = colour_scale)
    # theme(legend.position = "none")
  }


  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits,
                                     rm_x_labels= rm_x_labels,
                                     rm_y_labels=  rm_y_labels)

  gg_chart
}


#***************
# HELPER FUNCTION
#' Title
#'
#' @param gg_chart
#' @param title
#' @param x_limits
#' @param y_limits
#' @param flip_coord
#' @param scale_y_cont
#' @param rm_x_labels
#' @param rm_y_labels
#'
#' @return modified gg_chart
#'
#' @examples
common_stats_aesethetics<-function(gg_chart=NA,
                                   title=NA,
                                   x_limits=NA,
                                   y_limits=NA,
                                   flip_coord = FALSE,
                                   scale_y_cont = NA,
                                   rm_x_labels = FALSE,
                                   rm_y_labels = FALSE){

  if(!is.na(title)) {
    gg_chart <- gg_chart + ggtitle(title)
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

  if(!is.na(scale_y_cont)) {
    gg_chart <- gg_chart + scale_y_continuous(scale_y_cont)
  }


  if(rm_x_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  if(rm_y_labels) {
    gg_chart <- gg_chart +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  return(gg_chart)
}

