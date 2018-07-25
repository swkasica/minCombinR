# Helper functions for genomic chart types

#Phylogenetic Tree
plot_phylo_tree <- function(nwk_file, x_limits=NA, y_limits=NA, flip_coord=FALSE) {

  tree_from_nwk <- ape::read.tree(nwk_file)
  gg_chart <- ggtree::ggtree(tree_from_nwk) + ggtree::geom_treescale()

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

#Dendrogram
#Can also use ggraph
#cluster_vars is a vector of columns to cluster by.
plot_dendro <- function(data, tip_var=NA, cluster_vars=NA) {

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

  clust_data <- data %>%
    scale() %>%
    dist() %>%
    hclust(method = "ward.D2")
  clust_dendro <- as.dendrogram(clust_data)

  #To get order
  # order <- clust_data$order
  #To reorder data frame according to clustering order
  # reordered_data <- arrange(data, clust_data$order)

  #This package is mainly used to get the data from a dendrogram
  # I could probably just use ggplot here using geom_segment and geom_text
  ggdendro::ggdendrogram(clust_dendro)

  # # par(mar=c(7,3,1,1))  # move bottom margin to have the complete label
  # # plot(dend)
  # ggraph::ggraph(clust_dendro, layout='dendrogram') +
  #   ggraph::geom_edge_elbow()
}

#Clonal Tree
plot_clonal_tree <- function(nwk_file, node_groups, x_limits=NA, y_limits=NA, flip_coord=FALSE) {
  tree <- ape::read.tree(nwk_file)
  tree <- ggtree::groupClade(object=tree, node=node_groups)
  gg_chart <- ggtree::ggtree(tree, aes(color=node_groups)) +
    ggtree::geom_nodepoint(aes(size=5, alpha=0.5))

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
plot_linear_genome_map_from_df <- function(data, comparisons = NULL) {
  genoPlotR::plot_gene_map(data, comparisons)
}

#Radial Genomic Map
#TODO: get data to model this and inform input
plot_radial_genomic_map <- function(ideogram, chr_exclude, tracks_inside, tracks_outside) {
  RCircos::RCircos.Set.Core.Components(cyto.info=ideogram, chr.exclude=chr_exclude,
                              tracks.inside=tracks_inside, tracks_outside)
  RCircos::RCircos.Set.Plot.Area()
  RCircos::RCircos.Chromosome.Ideogram.Plot()
}

#Alignment (assuming table)
#TODO: get data to model this and inform input
plot_table <- function(data) {
  grid::grid.newpage()
  gridExtra::grid.table(data)
}
