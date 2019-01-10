#' Take specifications and render a single chart
#'
#' @title plot_simple
#' @param ...
#'
#' @return
plot_simple<-function(...){

  spec_list<-list(...)

  #check, has the user specified anything that overwrites the default parameters?
  default_specs<-gevitr_env$plot_param_defaults
  over_write_default_idx<-match(names(spec_list),names(default_specs))

  #if yes, then use user's specification and not default
  if(length(over_write_default_idx)>0){
    default_specs[over_write_default_idx]<-NULL
    default_specs<-base::Filter(Negate(is.null), default_specs)
  }

  spec_list<-append(spec_list,default_specs)

  #check if user has provided a gevitR object or not
  data<-spec_list[["data"]]

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }


  #now check if data is a gevitR object
  if(!is.data.frame(data)  && class(data) == "gevitDataObj"){
    tmp<-data@data[[1]]
    #if the user hasn't specific any metadata,
    #check if there's some already associated with the object
    if(is.na(spec_list[["metadata"]]) & length(data@data)>1){
      metadata<-ifelse(!is.null(data@data[["metadata"]]),data@data[["metadata"]],NA)
      spec_list[["metadata"]]<-metadata
    }
    data<-tmp
  }

  spec_list[["data"]]<-data
  #call the rendering functions to make a single chart
  chart<-switch(spec_list[["chart_type"]],
         #common statistical charts types
         "bar" = do.call(render_bar,args = spec_list),
         "pie" = do.call(render_pie,args = spec_list),
         "line" = do.call(render_line,args = spec_list),
         "scatter" = do.call(render_scatter,args = spec_list,envir = parent.frame()),
         "histogram" = do.call(render_histogram,args = spec_list),
         "pdf" = do.call(render_1D_density,args = spec_list),
         "boxplot" = do.call(render_boxplot,args = spec_list),
         "swarmplot" = do.call(render_swarm_plot,args = spec_list),
         #colour charts types
         "heatmap" = do.call(render_heatmap,args = spec_list),
         "category stripe" = do.call(render_category_stripe,args = spec_list),
         "density" = do.call(render_2D_density,args = spec_list),
         #tree chart types
         "phylogenetic tree" = do.call(render_phylogenetic_tree,args = spec_list),
         "dendrogram" = do.call(render_dendrogram,args = spec_list),
         #relational chart types
         "node-link" = do.call(render_node_link,args = spec_list),
         "chord" = do.call(render_chord,args = spec_list),
         #spatial chart types - to revise
          "choropleth" = do.call(render_choropleth,args = spec_list),
          "geographic map"= do.call(render_geographic_map,args = spec_list),
         #temporal chart types - to implement
         #genomic chart types - to implement
         #other char types  - to implement

          NULL)

    return(chart)
}

#'Many types general plot
#'
#'@param ... Any number of lists of arguments to generate a plot
#'@return plots to display
plot_many_types_general <- function(...) {
  args_list <- list(...)
  #all_plots <- lapply(args_list, function(x) {do.call(plot_simple, x)})
  combo_plots<-arrange_plots(args_list, labels = "AUTO")

  return(combo_plots)
}

#' Title
#'
#' @param chart_type
#' @param data
#' @param facet_by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_small_multiples <- function(...) {

  spec_list<-list(...)#don't know why I have to do this

  #getting the data into a workable form
  #check if user has provided a gevitR object or not
  data<-spec_list[["data"]]

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }


  #now check if data is a gevitR object
  if(!is.data.frame(data)  && class(data) == "gevitDataObj"){
    data_type<-data@type
    data<-data@data[[1]]
  }else if(is.data.frame(data)){
    data_type<-"table"
  }else{
    data_type<-NA
  }


  #Only tabular data can be meaningfully subsetted
  #other data types, cannot be, and the whole original
  #chart must be shown, but only with specific subsets
  #of the data on it, which for other charts types
  #means that some metadta must be associated with it
  #that is subsetable
  if(data_type == "table"){
    #make sure the data has the same points in x and y
    #then send it off

    all_data_plot<-do.call(plot_simple,spec_list,envir=parent.frame())
    all_data_plot_info<-ggplot2::ggplot_build(all_data_plot)
    all_data_plot_scales<-all_data_plot_info$layout$panel_params[[1]]

    #generate charts for each subgroup
    facet_var<-spec_list$facet_by

    all_plots<-c()
    for(grpItem in unique(data[,facet_var])){
      tmp<-data %>% dplyr::filter_(.dots = paste0(facet_var, "=='", grpItem, "'"))
      spec_list$data<-tmp
      spec_list$x_limits<-all_data_plot_scales$x.range
      spec_list$y_limits<-all_data_plot_scales$y.range
      spec_list$title<-grpItem
      all_plots[[grpItem]]<-do.call(plot_simple,args=spec_list,envir = parent.frame())
    }

    combo_plots<-arrange_plots(all_plots)
    return(combo_plots)
  }else{
    print("TO DO")
  }
}


#'Helper function to arrange plots for displaying
#'@title arrange_plots
#'@param chart_list A list of charts
arrange_plots <- function(chart_list, labels = NULL, ...) {

  chart_list <- lapply(chart_list, function(chart) {
    if ('ggtree' %in% class(chart)) {
      cowplot::plot_to_gtable(chart)
    } else if('gg' %in% class(chart)) {
      cowplot::plot_to_gtable(chart)
      # ggplotify::as.grob(chart)
    } else if ('data.frame' %in% class(chart)){
      multipanelfigure::capture_base_plot(chart)
    } else if ('htmlwidget' %in% class(chart)) {
      grid::grid.grabExpr(print(chart))
    } else {
      chart
    }
  })

  #NOTES: in order to add a shared legend, pass in shared_legend = TRUE in the ...
  combo<-cowplot::plot_grid(plotlist = chart_list, labels = labels, axis="tblr", ...)
  return(combo)
}



