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
         "scatter" = do.call(render_scatter,args = spec_list),
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
