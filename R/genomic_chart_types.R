# Helper functions for genomic chart types

#Phylogenetic Tree
plot_phylo_tree <- function(nwk_file) {
  my_tree <- ape::read.tree(nwk_file)
  ggtree::ggtree(my_tree)
  #plot(my_tree) #using ape package only (instead of ggtree)
}

#Dendrogram
plot_dendro <- function(data, cluster_var) {
  if (!missing(cluster_var)) {
    data <- data %>%
      dplyr::select(cluster_var)
  }

  dend <- data %>%
    scale() %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  # par(mar=c(7,3,1,1))  # move bottom margin to have the complete label
  # plot(dend)
  ggraph::ggraph(dend, layout='dendrogram') +
    ggraph::geom_edge_elbow()
}

#Clonal Tree
plot_clonal_tree <- function(nwk_file, node_groups) {
  tree <- ape::read.tree(nwk_file)
  tree <- ggtree::groupClade(object=tree, node=node_groups)
  ggtree::ggtree(tree, aes(color=node_groups)) +
    ggtree::geom_nodepoint(aes(size=5, alpha=0.5))
}

#Standard Genomic Map
plot_linear_genome_map_from_df <- function(all_data, comparisons = NULL) {
  genoPlotR::plot_gene_map(all_data, comparisons)
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
