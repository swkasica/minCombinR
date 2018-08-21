# Helper functions for genomic chart types
#TODO: currently only allows a gevitDataObj of type tree as input
#TODO: currently only considers reencoded line marks
#data must be a gevitDataObj of type tree
# edge_col_var = a variable found in data that will be used to color the leaves
# edge_col_palette = A NAMED vector of colors (hex) for labels; has to be named to choose which colour corresponds to which val.
#Phylogenetic Tree
render_phylo_tree <- function(data, x_limits=NA, y_limits=NA, flip_coord=FALSE, edge_col_var=NULL, edge_col_palette=NULL) {


  if (class(data) != "gevitDataObj") {
    #TODO: allow for a nwk file as input too?
    stop("phylogenetic tree must be first created using gevitR input functions.")
  }

  tree <- data@data$tree
  meta <- data@data$metadata

  gg_chart <- ggtree::ggtree(tree) + ggtree::geom_treescale()

  if(!is.null(branch_col_var)) {
    if (is.null(branch_col_palette)) {
      get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      colours <- get_palette(length(unique(meta[[branch_col_var]])))
      names(colours) <- unique(meta[[branch_col_var]])
    }

    #getting ready to merge into metadata
    colours <- as.data.frame(colours)
    tmp <- rownames(colours)
    colours <- cbind(tmp, data.frame(colours, row.names = NULL))
    #rename for joining
    colnames(colours)[colnames(colours) == "tmp"] <- branch_col_var
    #join with metadata
    metadata <- plyr::join(x=meta, y=colours, by=branch_col_var)

    gg_chart <- gg_chart %<+% metadata + aes(color=colours) + theme_tree()

    # tree_sample <- treeEBOV@data$tree
    # ggchart <- ggtree::ggtree(tree_sample) + ggtree::geom_treescale()
    # ggchart %<+% metadata + aes(color=I(colours)) + theme_tree()
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

  # if(!is.null(colour_var)) {
  #   gg_chart <- gg_chart %+% ggtree(aes_string(color=colour_var))
  # }

  gg_chart
}

# -- Render a Dendrogram --
# OPTIONAL REORDERING:
# DEFAULT MARK IS TEXT LABELS (SHOULD IT BE LINE?)
# labels = vector of strings for labels
# labels_col_var = a variable found in data that will be used to color the labels
# labels_col_values = A numeric vector of values to color labels.
# labels_col_palette = A NAMED vector of colors (hex) for labels
# leaf_col_var = a variable found in data that will be used to color the leaves
# leaf_col_palette = A NAMED vector of colors (hex) for labels; has to be named to choose which colour corresponds to which val.
# OTHER OPTIONAL VARS:
# cluster_vars = a vector of columns to cluster by.
render_dendro <- function(data, labels=NULL,
                          labels_col_var=NULL, labels_col_values=NULL, labels_col_palette=NULL, labels_size=NULL,
                          leaf_col_var=NULL, leaf_col_palette=NULL,
                          tip_var=NULL, cluster_vars=NULL) {

  #Subset the data frame to only contain the cluster_vars for clustering and the tip_var as the rownames(for labelling nodes)
  if (!is.null(tip_var) && !is.null(cluster_vars)) {
    data <- unique(data %>%
                     group_by_(tip_var) %>%
                     select(c(tip_var, cluster_vars)))
    #Could set rownames using tibble package instead of base but it's not worth the extra dependency
    data <- as.data.frame(data)
    rownames(data) <- data[ , tip_var]
    data[ , tip_var] <- NULL
  }

  dend <- mtcars %>%
    scale() %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  #To get order
  # clust_data <- mtcars %>%
  #   scale() %>%
  #   dist() %>%
  #   hclust()
  # order <- clust_data$order
  #To reorder data frame according to clustering order
  # reordered_data <- arrange(data, clust_data$order)

  # ------ REENCODEMENTS ------

  # ---- LABELS COLOR CHANGES [MARK TYPE = TEXT] ----

  #add labels
  if (!is.null(labels)) {
    dend <- dend %>% dendextend::set("labels", labels)
  }

  #TODO: should check if both labels_col_var and labels_col_values are set and return an error or warning if so (in check fcns)

  #add variable of color to legend
  if (!is.null(labels_col_var)) {
    # -- set label color according to var
    label_color_values <- as.numeric(data[[labels_color]])
    label_color_values <- label_color_values[order.dendrogram(dend)]

    if (!is.null(labels_col_palette)) {
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
  if (!is.null(labels_col_values)) {

    if (!is.null(labels_col_palette)) {
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

  if(!is.null(labels_size)) {
    label_size_values <- as.numeric(data[[labels_size]])
    label_size_values <- label_size_values[order.dendrogram(dend)]
    dend <- dend %>% dendextend::set("labels_cex", labels_size_values)
  }

  #Make sure the labels are characters
  dend <- dend %>% dendextend::set("labels_to_char")

  # ---- LEAF CHANGES [MARK TYPE = LINE] ----

  if (!is.null(leaf_col_var)) {
    # -- set leaf color according to var
    leaf_color_values <- as.numeric(data[[leaf_col_var]])
    leaf_col_values <- leaf_color_values[order.dendrogram(dend)]

    if (!is.null(leaf_col_palette)) {
      #map palette to variable values vector and make color
      leaf_col_values <- plyr::mapvalues(leaf_col_values, from = names(leaf_col_palette), to = leaf_col_palette)
    }

    dend <- dendextend::assign_values_to_leaves_edgePar(dend = dend, value = leaf_col_values, edgePar = "col")

    #change color of labels accordingly.
    dendextend::labels_colors(dend) <- leaf_color_values

  }


  #plot using ggplot
  ggdend <- dendextend::as.ggdend(dend)
  ggplot(ggdend) + ylim(-4, max(dendextend::get_branches_heights(dend))) #Changes the ylim to deal with long labels in ggplot2
}


# --- Render Clonal Tree ---
# data = gevitDataObj of type tree
#     metadata in data should have values for all nodes (including both tip nodes and internal nodes)
# branch_col_var = a variable found in data that will be used to color the edge of the associated nodes.
# branch_col_palette = A NAMED vector of colors (hex) for branch colors for branch_col_var;
#     has to be named to choose which colour corresponds to which val. (mostly have this for many types linked!)
# node_col_var = a variable to color the node points by
# node_col_palette = A NAMED vector of colors for point colors for node_col_var ; will by used by many_types_linked
render_clonal_tree <- function(data, branch_col_var=NULL, branch_col_palette=NULL, node_col_var=NULL, node_col_palette=NULL,
                               x_limits=NA, y_limits=NA, flip_coord=FALSE) { #node_groups,
  # tree <- ape::read.tree(nwk_file)
  # tree <- ggtree::groupClade(object=tree, node=node_groups)

  if (class(data) != "gevitDataObj") {
    #TODO: allow for a nwk file as input too?
    stop("phylogenetic tree must be first created using gevitR input functions.")
  }
  tree <- data@data$tree
  meta <- data@data$metadata

  gg_chart <- ggtree::ggtree(tree, aes(color=node_groups)) +
    ggtree::geom_treescale() +
    geom_point()

  if (!is.null(branch_col_var)) {
    if (is.null(branch_col_palette)) {
      #TODO: consider what happens with more than 9 colors
      get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      colours <- get_palette(length(unique(meta[[branch_col_var]])))
      names(colours) <- unique(meta[[branch_col_var]])
    }

    #getting ready to merge into metadata
    colours <- as.data.frame(colours)
    tmp <- rownames(colours)
    colours <- cbind(tmp, data.frame(colours, row.names = NULL))
    #rename for joining
    colnames(colours)[colnames(colours) == "tmp"] <- branch_col_var
    #join with metadata
    metadata <- plyr::join(x=meta, y=colours, by=branch_col_var)

    #TODO: could technically use I(colours) here because should have metadata for all internal nodes too
    gg_chart <- gg_chart %<+% metadata + aes(color=colours) + theme_tree()
  }

  if (!is.null(node_col_var)) {
    if (is.null(branch_col_palette)) {
      #TODO: consider what happens with more than 9 colors
      get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
      colours <- get_palette(length(unique(meta[[branch_col_var]])))
      names(colours) <- unique(meta[[branch_col_var]])
    }

    #getting ready to merge into metadata
    colours <- as.data.frame(colours)
    tmp <- rownames(colours)
    colours <- cbind(tmp, data.frame(colours, row.names = NULL))
    #rename for joining
    colnames(colours)[colnames(colours) == "tmp"] <- branch_col_var
    #join with metadata
    metadata <- plyr::join(x=meta, y=colours, by=branch_col_var)

    gg_chart %<+% metadata + geom_point(aes(color=I(colours)))
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

#Standard Genomic Map
render_linear_genome_map_from_df <- function(data, comparisons = NULL) {
  genoPlotR::plot_gene_map(data, comparisons)
}

#Radial Genomic Map
#TODO: get data to model this and inform input
render_radial_genomic_map <- function(ideogram, chr_exclude, tracks_inside, tracks_outside) {
  RCircos::RCircos.Set.Core.Components(cyto.info=ideogram, chr.exclude=chr_exclude,
                                       tracks.inside=tracks_inside, tracks_outside)
  RCircos::RCircos.Set.Plot.Area()
  RCircos::RCircos.Chromosome.Ideogram.Plot()
}

#from ggseqlogo package
  #from ggseqlogo package documentation:
  # data =  Character vector of sequences or named list of sequences. All sequences must have same width
render_sequence_logo <- function(data) {
  ggseqlogo::ggseqlogo(data)
}

#Alignment
#dna object??
#TODO The sequence alignment code came from the ggseqlogo package examples! (reference)
#TODO: hasn't been included in gevitR functions yet.
#data is a data frame with columns: letters, nucleotide position (x) and bits (y) OR a GevitDataObj
render_alignment <- function(data, nucleotide_pos=NULL, bits=NULL) {
  #TODO: set a limit on the alignment length and if passed, will plot using colour instead of text?? or using table??
  #ape::image.DNAbin(data@data$dnaBin)

  #TODO: test case with one bit.
  #These should only be smaller in size.
  if (class(data)[[1]] == "gevitDataObj") {
    alignment <- ape::as.alignment(data@data$dnaBin)
    num_samples <- alignment$nb
    seq_length <- nchar(alignment$seq[[1]]) + 1 #For some reason nchar doesn't include one character
    seq <- alignment$seq
    seq_var <- stringr::str_split(seq, pattern="", n=(seq_length *num_samples))
    print(seq_var)

    nucleo_pos_var <- rep(1:seq_length, num_samples)
    sample_var <- rep(1:num_samples, each=seq_length)
    data <- data.frame(seq_var=seq_var, nucleo_pos_var=nucleo_pos_var, sample_var=sample_var)
    print(data)
  }

  alignment_chart <- ggplot(data, aes_string(x=nucleo_pos_var, y=sample_var)) +
    geom_text(aes_string(label=seq_var)) +
    theme_logo()

  return(alignment_chart)

}

#Alignment (assuming table)
#TODO: get data to model this and inform input
# render_table <- function(data) {
#   if (class(data) == "gevitDataObj") {
#     grid::grid.newpage()
#   } else {
#     grid::grid.newpage()
#     gridExtra::grid.table(data)
#   }
# }
