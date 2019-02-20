#' Render Phylogenetic Tree
#' @title render_phylo_tree
#' @param ...
#' @importFrom ggtree %<+%
#' @return
render_phylogenetic_tree <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())
  data_class<-class(data)

  #Establish the type of data that the user has passed to the function
  if(data_class == "gevitDataObj"){
    #extract data from the gevitr object
    tree <- data@data[[1]]
    metadata <- if(is.null(data@data$metadata)) NA else data@data$metadata
  }else if(data_class == "phylo"){
    #can plot a phylo object only
    tree<-data
    #metadata will default to NA and is loaded into the environment via the list2env command
  }else if(data_class == "character"){
    if(!is.na(metadata) & class(metadata) == "character"){
      tmp<-input_data(file = data,"tree",metadataFile=NULL)
      tree <- tmp@data$tree
      metadata <- tmp@data$metadata
    }else{
      tmp<-input_data(file = data,"tree")
      tree <- tmp@data$tree
      #metadata will default to NA and is loaded into the environment via the list2env command
    }

  }else{
    stop("Unrecognized data type")
  }

  #layout can be default, rooted, rooted radial (or radial), schematic, unrooted, unrooted radial OR any layouts available in ggtree
  #Put the layout variable in the form that ggtree understands
  layout<-if(is.na(layout)) "default" else layout

  layout <- switch(layout,
                   "default" = "rectangular",
                   "rooted" = "rectangular",
                   "rooted_radial" = "circular",
                   "radial" = "circular",
                   "schematic" = "rectangular", #TODO: check this is correct
                   "unrooted" = "slanted",
                   "unrooted_radial" = "fan",
                   NULL)

  if(is.na(layout)){
    warning("Unrecognized tree layout. Defaulting to rectangular lay out. Possible layout types include: rooted, rooted_radial, unrooted, and unrooted_radial" )
    layout<-"rectangular"
  }

  #plot that tree!
  #remove the branch length arugment, that is not correct
  #gg_chart <- ggtree::ggtree(tree,layout = layout) +
  #  ggtree::geom_treescale()

  gg_chart<-ggtree::ggtree(tree,layout = layout)

  if(!(all(is.na(metadata)))){
    gg_chart<-gg_chart %<+% metadata #adding metadata
  }

  if(combo_type == "small_multiple"){
    gg_chart<- gg_chart + #adding metadata
      geom_tippoint(aes(colour=show_var,alpha=show_var)) +
      scale_alpha_manual(values=c(1,0))+
      theme(legend.position = "top")
  }


  #quick overwrite - don't show tip labels if you have more than 50 elements in the tree
  if(length(tree$tip.label)<=50){
    gg_chart <- gg_chart + ggtree::geom_tiplab()
  }

  if(!is.na(color) & !all(is.na(metadata))) {
    gg_chart<-gg_chart %+% aes_string(color = color) + geom_tippoint()
    # if (is.na(colour_scale)) {
    #   colours <- get_colour_palette(data, default_colour_var)
    # } else {
    #   colours <- colour_scale
    # }
    #
    # #getting ready to merge colours into metadata
    # colours <- as.data.frame(colours)
    # tmp <- rownames(colours)
    # colours <- cbind(tmp, data.frame(colours, row.names = NULL))
    #
    # #rename for joining
    # colnames(colours)[colnames(colours) == "tmp"] <- default_colour_var
    #
    # #join with metadata
    # metadata <- plyr::join(x=metadata, y=colours, by=default_colour_var)
    #
    # gg_chart <- gg_chart %<+% metadata + geom_tippoint(color=metadata$colours) +
    #   scale_color_manual(values = colour_scale)
  }

  #modifying tree aesethetics

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits)


  gg_chart<-gg_chart + theme(legend.position = "none") #added this for comp

  return(gg_chart)
}


#' Render a dendrogram
#' @title render_dendro
#' @param ...
#'
#' @return
render_dendrogram <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }


  #Subset the data frame to only contain the cluster_vars for clustering and the tip_var as the rownames(for labelling nodes)
  if (!is.na(tip_var) && !is.na(cluster_vars)) {
    data <- unique(data %>%
                     group_by_(tip_var) %>%
                     select(c(tip_var, cluster_vars)))
    #Could set rownames using tibble package instead of base but it's not worth the extra dependency
    data <- as.data.frame(data)
    rownames(data) <- data[ , tip_var]
    data[ , tip_var] <- NULL
  }

  dend <- data %>%
    scale() %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  # ------ reencodings ------

  # ---- LABELS COLOR CHANGES [MARK TYPE = TEXT] ----

  #add labels
  if (!is.na(labels)) {
    dend <- dend %>% dendextend::set("labels", labels)
  }

  #TODO: should check if both labels_col_var and labels_col_values are set and return an error or warning if so (in check fcns)

  #add variable of color to legend
  if (!is.na(labels_col_var)) {
    # -- set label color according to var
    label_color_values <- as.numeric(data[[labels_color]])
    label_color_values <- label_color_values[order.dendrogram(dend)]

    if (!is.na(labels_col_palette)) {
      #TODO: if not provided with colour_palette then generate with get_colour_palette()!!!
      #to show how palette will be generated in many types linked
      # get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      # colours <- get_palette(length(unique(label_color_values)))
      # names(colours) <- unique(label_color_values)

      #map palette to variable values vector and make color
      label_color_values <- plyr::mapvalues(label_color_values, from = names(labels_col_palette), to = labels_col_palette)
    }

    #change color of labels accordingly.
    dendextend::labels_colors(dend) <- label_color_values
  }

  #Add values of colors to legend
  if (!is.na(labels_col_values)) {

    if (!is.na(labels_col_palette)) {
      #TODO: if not provided with colour_palette then generate with get_colour_palette()!!!
      #to show how palette will be generated in many types linked
      # get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      # colours <- get_palette(length(unique(label_color_values)))
      # names(colours) <- unique(label_color_values)

      #map palette to variable values vector and make color
      labels_col_values <- plyr::mapvalues(labels_col_values, from = names(labels_col_palette), to = labels_col_palette)
    }

    dendextend::labels_colors(dend) <- labels_col_values
  }

  #dend <- dend %>% dendextend::set(...)

  if(!is.na(labels_size)) {
    label_size_values <- as.numeric(data[[labels_size]])
    label_size_values <- label_size_values[order.dendrogram(dend)]
    dend <- dend %>% dendextend::set("labels_cex", labels_size_values)
  }

  #Make sure the labels are characters
  dend <- dend %>% dendextend::set("labels_to_char")

  # ---- LEAF CHANGES [MARK TYPE = LINE] ----

  if (!is.na(leaf_col_var)) {
    # -- set leaf color according to var
    leaf_color_values <- as.numeric(data[[leaf_col_var]])
    leaf_col_values <- leaf_color_values[order.dendrogram(dend)]

    if (!is.na(leaf_col_palette)) {
      #map palette to variable values vector and make color
      leaf_col_values <- plyr::mapvalues(leaf_col_values, from = names(leaf_col_palette), to = leaf_col_palette)
    }

    dend <- dendextend::assign_values_to_leaves_edgePar(dend = dend, value = leaf_col_values, edgePar = "col")

    #change color of labels accordingly.
    dendextend::labels_colors(dend) <- leaf_color_values

  }

  #plot using ggplot
  ggdend <- dendextend::as.ggdend(dend)
  ggdend<-ggplot(ggdend) + ylim(-0.4, max(dendextend::get_branches_heights(dend))) #Use ylim to deal with long labels in ggplot2

  return(ggdend)
}
