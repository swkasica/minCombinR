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

  #getting rid of an empty element in the spec list
  if(names(spec_list)[1] == ""){
    spec_list<-spec_list[-1]
  }

  #call the rendering functions to make a single chart
  chart<-switch(spec_list[["chart_type"]],
         #common statistical charts types
         "bar" = do.call(render_bar,args = spec_list,envir = parent.frame()),
         "pie" = do.call(render_pie,args = spec_list,envir = parent.frame()),
         "line" = do.call(render_line,args = spec_list,envir = parent.frame()),
         "scatter" = do.call(render_scatter,args = spec_list,envir = parent.frame()),
         "histogram" = do.call(render_histogram,args = spec_list,envir = parent.frame()),
         "pdf" = do.call(render_1D_density,args = spec_list,envir = parent.frame()),
         "boxplot" = do.call(render_boxplot,args = spec_list,envir = parent.frame()),
         "swarmplot" = do.call(render_swarm_plot,args = spec_list,envir = parent.frame()),
         #colour charts types
         "heatmap" = do.call(render_heatmap,args = spec_list,envir = parent.frame()),
         "category stripe" = do.call(render_category_stripe,args = spec_list,envir = parent.frame()),
         "density" = do.call(render_2D_density,args = spec_list,envir = parent.frame()),
         #tree chart types
         "phylogenetic tree" = do.call(render_phylogenetic_tree,args = spec_list,envir = parent.frame()),
         "dendrogram" = do.call(render_dendrogram,args = spec_list,envir = parent.frame()),
         #relational chart types
         "node-link" = do.call(render_node_link,args = spec_list,envir = parent.frame()),
         "chord" = do.call(render_chord,args = spec_list,envir = parent.frame()),
         #spatial chart types - to revise
          "choropleth" = do.call(render_choropleth,args = spec_list,envir = parent.frame()),
          "geographic map"= do.call(render_geographic_map,args = spec_list,envir = parent.frame()),
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

#' Plot small multiples
#' @title plot_small_multiples
#' @param ...
#'
#' @return
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
    data<-data@data

    if(!is.null(data$metadata)){
      metadata<-data$metadata
    }

    data<-data[[1]]

  }else if(is.data.frame(data)){
    data_type<-"table"
    metadata<-ifelse(is.na(spec_list$metadata),NA,spec_list$metadata)
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
    spec_list<-ggplot_scale_info(all_data_plot,spec_list)


    #generate charts for each subgroup
    facet_var<-spec_list$facet_by

    all_plots<-c()
    for(grpItem in unique(data[,facet_var])){
      tmp<-data %>% dplyr::filter_(.dots = paste0(facet_var, "=='", grpItem, "'"))

      spec_list$data<-tmp
      spec_list$title<-grpItem
      all_plots[[grpItem]]<-do.call(plot_simple,args=spec_list,envir = parent.frame())
    }

    combo_plots<-arrange_plots(all_plots)
  }else{
    #For non-tabular data types, you must provide some additional metadata
    if(is.null(metadata)){
      stop("To make a small multiple for this chart you need to provide additional metadata")
    }

    if(!(spec_list$chart_type %in% c("phylogenetic tree"))){
      stop("Small multiples have not yet been implemented for you chart type")
    }

    #make a simple plot, but indicate that it's for a combination
    spec_list$combo<-"small multiples"
    #generate charts for each subgroup
    facet_var<-spec_list$facet_by
    all_plots<-c()

    for(grpItem in unique(metadata[,facet_var])){
      #works for phylo tree, but need to test on others
      meta_sub<-metadata %>%
        dplyr::mutate_(.dots = paste0("show_var = ifelse(",facet_var," =='", grpItem,"','",grpItem,"','Other')"))

      #a small cheat
      colnames(meta_sub)<-c(head(colnames(meta_sub),-1),"show_var")

      spec_list$data<-data
      spec_list$metadata<-meta_sub
      spec_list$title<-grpItem

      #just call a simple plot
      all_plots[[grpItem]]<-do.call(plot_simple,args=spec_list,envir = parent.frame())
    }
    combo_plots<-arrange_plots(all_plots)
  }
  return(combo_plots)
}

#' Helper function to extract x and y scales from ggplot entity
#' @title ggplot_scale_info
#' @param chart
#'
ggplot_scale_info<-function(chart = NULL,spec_list = NULL){
  chart_info<-ggplot2::ggplot_build(chart)

  x_scale<-chart_info$layout$panel_scales_x[[1]]
  y_scale<-chart_info$layout$panel_scales_y[[1]]


  #x axis
  if("ScaleContinuous" %in% class(x_scale)){
    spec_list$x_limits<-x_scale$range$range
  }else if("ScaleDiscrete" %in% class(x_scale)){
    spec_list$x_labels <- x_scale$range$range
  }

  #y axis
  if("ScaleContinuous" %in% class(y_scale)){
    spec_list$y_limits<-y_scale$range$range
  }else if("ScaleDiscrete" %in% class(y_scale)){
    spec_list$y_labels <- y_scale$range$range
  }

  return(spec_list)

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



