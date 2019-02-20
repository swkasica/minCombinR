#a method to get data out of objects - eventually put this in gevitR class
#' Helper function to get data out of specific gevitR data type
#'
#' @param item
#'
#' @return
get_raw_data<-function(item = NA){

  if(!class(item) == "gevitDataObj"){
    return(NULL)
  }

  itemData<-item@data
  itemType<-item@type

  #extract the data or metadata into a table / character vector
  dataOut<-switch(itemType,
                  "phyloTree" = as.character(itemData[[1]]$tip.label),
                  "dna" = as.character(names(itemData[[1]])),
                  "spatial" = itemData[[2]],
                  "table" = itemData[[1]],
                  NULL)

  return(dataOut)
}

#amethod to check links between two charts
#' helper function to check link between data items
#'
#' @param item_one
#' @param item_two
#'
#' @return
check_link<-function(item_one = NA,item_two=NA){

  compRes<-NULL


  if(is.null(ncol(item_one)) & is.null(ncol(item_two))){
    #two vectors
    class_one<-class(item_one)
    class_two<-class(item_two)

    item_one<-if(class_one%in%c("factor","character")) as.character(item_one) else item_one
    item_two<-if(class_two %in% c("factor","character")) as.character(item_two) else item_two

    if(class_one == class_two){
      if(class_one %in% c("factor","character")){
        compRes<-dplyr::setequal(item_one,item_two)
      }else{
        #don't match on numerical axes yet, it could be a co-incidence
        #mainly looking to match ids to some ids column
        compRes<-NULL
      }
    }


  }else if(is.null(ncol(item_one)) | is.null(ncol(item_two))){
    #one vector and one data frame
    df_item<-if(is.data.frame(item_one)) item_one else item_two
    vec_item<-if (!is.data.frame(item_one)) item_one else item_two

    compRes<-apply(df_item,2,function(x,comp_item){
      check_link(x,comp_item)
    },comp_item = vec_item)

    compRes<-colnames(df_item)[compRes]
  }else{
    #two data frames - won't every really happen right now
    compRes<-apply(item_one,2,function(x,comp_item){
      check_link(x,comp_item)
    },comp_item= item_two)

    compRes[sapply(compRes,function(x){length(x)==0})] <- NULL


  }

  return(compRes)
}



#' Return Compatible Chart Links
#'
#' @param chart_info
#'
#' @return data frame for compatible charts and their linking variable
return_compatible_chart_link<-function(chart_info = NA,combo_type = NA){

  data_layer<-unique(chart_info$data)
  variable_layer<-unname(unlist(chart_info[,4:ncol(chart_info)]))
  variable_layer<-unique(variable_layer[!is.na(variable_layer)])
  chart_layer<-unique(chart_info$chart_name)

  #build the edges of this graph
  edge_list<-c()

  #add data layer to variable layer
  for(dat in data_layer){
    chart_info_sub<-dplyr::filter(chart_info,data  == dat)

    vars<-unname(unlist(chart_info_sub[,4:ncol(chart_info_sub)]))
    vars<-unique(vars[!is.na(vars)])

    #edge from dataset to all the variables in that dataset
    edge_list<-rbind(edge_list,
                     cbind(rep(dat,length(vars)),vars))

    #get chart_types connected to individual variables
    charts<-unique(chart_info_sub$chart_name)

    for(chart in charts){
      sub_sub_info<-dplyr::filter(chart_info,chart_name  == chart)
      vars<-unname(unlist(sub_sub_info[,4:ncol(sub_sub_info)]))
      vars<-unique(vars[!is.na(vars)])

      edge_list<-rbind(edge_list,
                       cbind(vars,rep(chart,length(vars))))

      #if check id, see if there are other connections
      if(length(vars) == 1 && grepl("gevitr_checkID",vars)){
        #check to see if there are connections to other datasets
        #To do: check in the future if linkvar already worked out
        main_dat<-get(dat,envir = globalenv())

        comp_dats<-setdiff(data_layer,dat)
        for(comp_dat in comp_dats){
          comp_dat_name<-comp_dat
          comp_dat<-get(comp_dat,envir = globalenv())

          if(!is.data.frame(main_dat)){main_dat<-get_raw_data(main_dat)}
          if(!is.data.frame(comp_dat)){comp_dat<-get_raw_data(comp_dat)}

          if(!is.null(main_dat) & !is.null(comp_dat)){
            data_link<-check_link(main_dat,comp_dat)
          }

          if(!is.null(data_link)){
            #data link can be a boolean, character, or list
            if(class(data_link) == "logical"){
              edge_list<-rbind(edge_list,
                               cbind(vars,comp_dat_name)) #make sure its just the id item

            }else if(class(data_link) ==  "character"){
              edge_list<-rbind(edge_list,
                               cbind(data_link,rep(vars,length(data_link))))
            }else{
              print("Currently not supporting df to df link")
            }
          }
        }
      }

    }
  }

  compat_info<-NULL
  if(combo_type %in% c("composite","many_linked")){

  #Create a graph that can be traversed
  chart_graph<-igraph::graph_from_data_frame(edge_list,directed = FALSE)

  #Simple check if there are two charts
  if(length(chart_layer)<3){
    tmp<-igraph::shortest_paths(chart_graph,chart_layer[1],chart_layer[2])

    tmp<-setdiff(igraph::as_ids(tmp$vpath[[1]]),c(data_layer,chart_layer))

    #two objects connected
    if(sum(grepl("gevitr_checkID",tmp)) == length(tmp)){

      item_one<-get(chart_info[1,"data"],envir = globalenv())
      item_two<-get(chart_info[2,"data"],envir = globalenv())

      item_one<-get_raw_data(item_one)
      item_two<-get_raw_data(item_two)

      is_eq<-dplyr::setequal(item_one,item_two)

      if(!is_eq){
        #check if one set is a perfect subset of the other
        if(length(setdiff(item_one,item_two)) == 0 | length(setdiff(item_two,item_one)) == 0)
          compat_info<-data.frame(chart_one = chart_layer[1],
                                  chart_two = chart_layer[2],
                                  link = "gevitR_checkID")
          return(compat_info)
      }else{
        compat_info<-data.frame(chart_one = chart_layer[1],
                                chart_two = chart_layer[2],
                                link = "gevitR_checkID")
        return(compat_info)
      }
    }else{
      tmp<-tmp[which(!grepl("gevitr_checkID",tmp))]
      if(length(tmp) == 1){
        compat_info<-data.frame(chart_one = chart_layer[1],
                                chart_two = chart_layer[2],
                                link = tmp)
      }
    }
  }

  #All possible chart compatbilities
  chart_comp<-t(combn(chart_layer,m=2))

  pathStep<-apply(chart_comp,1,function(x,g){
    tmp<-igraph::shortest_paths(g,x[1],x[2])
    tmp_len<-length(tmp$vpath[[1]])
    if(tmp_len == 3){
      #modify this condition to allow checking nodes
      #charts have a shared axis
      return(igraph::as_ids(tmp$vpath[[1]])[2])
    }else if (tmp_len == 4){
      path<-igraph::as_ids(tmp$vpath[[1]])
      id_check<-grepl("gevitr_checkID",path)
      if(any(id_check)){
        path<-path[!id_check]
        return(path[2])
      }
    }else{
      return(NA)
    }
  },g=chart_graph)

  compat_info<-data.frame(cbind(chart_comp),pathStep, stringsAsFactors = FALSE)
  colnames(compat_info)<-c("chart_one","chart_two","link")

  #For when there should be only one link for all chart types
  #and it has to link across the majority of charts
  link_item<-compat_info %>%
    dplyr::filter(!is.na(link)) %>%
    group_by(link) %>%
    tally() %>%
    top_n(1)

    #compatible charts
    compat_info<-dplyr::filter(compat_info,link == link_item$link)
  }

  return(compat_info)
}


#' Helper function to get axis information from charts
#'
#' @param chart
#' @param align
#'
#' @return
get_axis_info<-function(chart=NULL,align=NULL){

  chart_class<-class(chart)
  label<-NA
  breaks<-NA
  axis_type<-NA
  align_dir<-align

  if("ggtree" %in% chart_class){
    #trees are different, although they have continous y
    #the important thing is to actually match of their tip labels
    #those tip labels are not continous variables
    chart_build<-dplyr::filter(chart$data,isTip == TRUE)
    label <- chart_build$label
    breaks<- chart_build$y
    axis_type = "discrete"
  }else if("ggplot" %in% chart_class){

    chart_build<-ggplot2::ggplot_build(chart)
    if(align == "h"){
      label<- chart_build$layout$panel_params[[1]]$y.labels
      breaks<-chart_build$layout$panel_params[[1]]$y.major_source
    }else if(algin =="v"){
      label<- chart_build$layout$panel_params[[1]]$x.labels
      breaks<-chart_build$layout$panel_params[[1]]$x.major
    }
  }

  return(list(y_labels = label,y_break = breaks))

}
