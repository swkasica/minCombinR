#' Render Heatmap
#' @title render_heatmap
#' @param ...
#'
#' @return
render_heatmap <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }

  gg_chart <- ggplot(data, aes_string(x, y, fill = z)) +
    geom_tile() +
    theme(legend.position="bottom")

  #To scale colour (called from many_types_linked and small_multiple)
  if (!is.na(colour_scale)[1]) {
    if (default_colour_var != z && !(is.na(default_colour_var))) {
      warning("z is masking link_var because link_var and z have to be the same for a heat_map when linking with colour")
    }
    get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))
    colr_pal <- get_palette(abs(diff(colour_scale)))
    gg_chart <- gg_chart +
      scale_fill_gradientn(colours = colr_pal, limits = colour_scale)
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


#' Render 2D density plot
#' @title render_2D_density_chart
#' @param ...
#'
#' @return
render_2D_density <- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot(data, aes_string(x=x, y=y) ) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon")

  if(!is.na(colour_scale)[1]) {
    gg_chart <- gg_chart +
      scale_fill_manual(name = default_colour_var, values = ..level..)
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits,
                                     rm_x_labels= rm_x_labels,
                                     rm_y_labels=  rm_y_labels)

  return(gg_chart)
}


render_category_stripe <- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  gg_chart <- ggplot(data, aes_string(x=x, y=shQuote("categories"), fill=category)) +
    geom_bin2d() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.background = element_blank(),
          legend.position = "none")


  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     x_limits=x_limits)
  gg_chart
}
