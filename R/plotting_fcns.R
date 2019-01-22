#' Take specifications and render a single chart
#'
#' @title plot_simple
#' @param ...
#'
#' @return
plot_simple<-function(...){

  spec_list<-list(...)

  #getting rid of an empty element in the spec list
  if(names(spec_list)[1] == ""){
    spec_list<-spec_list[-1]
  }


  #check, has the user specified anything that overwrites the default parameters?
  default_specs<-gevitr_env$plot_param_defaults
  over_write_default_idx<-match(names(spec_list),names(default_specs))
  over_write_default_idx<-over_write_default_idx[!is.na(over_write_default_idx)]

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

  spec_list<-list(...)

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
      meta_sub$show_var<-factor(meta_sub$show_var,levels=c(grpItem,"Other"))

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


plot_composite<-function(...){
  spec_list<-list(...)

  chart_info <- spec_list$chart_info %>%
    dplyr::mutate(isLead = ifelse(chart_type %in% gevitr_env$master_chart_types,TRUE,FALSE)) %>%
    dplyr::arrange(desc(isLead))

  # CHART ORDER
  # order charts, beginning with lead chart in initital position
  # data frame is already ordered from the arrange step
  # all specifications are valid with only one lead chart
  # all charts must align on a common variable
  chart_order<-chart_info$chart_name


  # FLIP COORD
  #Make sure that all chart co-ordinates are flipped to match
  #the lead chart - this will depend upon alignment direction
  #default is horizontal (align all the y-axis)
  align_dir<-"h"

  if(!is.null(spec_list$alignment)){
    align_dir<-ifelse(tolower(spec_list$alignment) %in% c("h","v"),
                      spec_list$alignment,
                      "h")
  }

  if(align_dir == "h"){
    chart_info<- chart_info %>%
      mutate(flip_coord = ifelse(y == spec_list$common_var | is.na(y),FALSE,
                                ifelse(grepl("gevitR_checkID",x),
                                       FALSE,TRUE)))
  }else{
    chart_info<- chart_info %>%
      mutate(flip_coord = ifelse(x == spec_list$common_var | is.na(x),FALSE,
                                ifelse(grepl("gevitR_checkID",x),
                                       FALSE,TRUE)))
  }

  # Generate individual charts - these will be modified and re-ordered
  all_plots<-c()
  for(chart in chart_order){

     baseSpecs<-get(chart,envir = globalenv())
     tmp_info<-dplyr::filter(chart_info,chart_name == chart)

     #modifying chart specifications
     baseSpecs$flip_coord<-tmp_info$flip_coord

     #if support chart
     if(!tmp_info$isLead){
       if(align_dir == "h"){
         baseSpecs$rm_y_labels<-TRUE
       }else{
         baseSpecs$rm_x_labels<-TRUE
       }
     }

     #generate the single chart
     tmp<-do.call(plot_simple,args=baseSpecs,envir = parent.frame())
     all_plots[[chart]]<-list(plotItem = tmp) #need to store as list
  }

  combo_plots<-arrange_plots(chart_list = all_plots,align_dir = align_dir)

  return(combo_plots)

}

#' Helped function to extract axis information
#'
#' @param chart_list
#'
#' @return axis infor

get_axis_info<-function(chart_list=NULL){

  chart<-chart_list[[1]]$plotItem
  chart_class<-class(chart)

  if("ggtree" %in% chart_chart_class){
    #trees are different, although they have continous y
    #the important thing is to actually match of their tip labels
    #those tip labels are not continous variables
    chart_info<-filter(chart$data,isTip = TRUE)
    y_axis<-chart_info$label
    y_axis_cat<-NULL
    x_axis<-chart_info$x
  } else if("ggplot" %in% chart_class){
    chart_info<-ggplot_build(chart)
  }else{
    print("Not currently supported")
  }
}


#'Helper function to arrange plots for displaying
#'@title arrange_plots
#'@param chart_list A list of charts
arrange_plots <- function(chart_list, labels = NULL,align_dir=NULL, ...) {

  chart_list <- lapply(chart_list, function(chart) {
    chart<-if(class(chart) == "list") chart[[1]] else chart
    chart_class<-class(chart)

    if ('ggtree' %in% chart_class) {
      cowplot::plot_to_gtable(chart)
    } else if('gg' %in% chart_class) {
      cowplot::plot_to_gtable(chart)
      # ggplotify::as.grob(chart)
    } else if ('data.frame' %in% chart_class){
      multipanelfigure::capture_base_plot(chart)
    } else if ('htmlwidget' %in% chart_class) {
      grid::grid.grabExpr(print(chart))
    } else {
      chart
    }
  })


  #NOTES: in order to add a shared legend, pass in shared_legend = TRUE in the ...
  if(!is.null(align_dir)){
    if(align_dir == "h"){
      combo<-cowplot::plot_grid(plotlist = chart_list, labels = labels, nrow=1,axis="tblr", ...)
    }else if(algin_dir == "v"){

    }
  }else{
    combo<-cowplot::plot_grid(plotlist = chart_list, ncol=1,labels = labels, axis="tblr", ...)
  }

  return(combo)
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


