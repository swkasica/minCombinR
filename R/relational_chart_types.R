#Helper functions for relational chart types

#Node link
plot_node_link <- function(data, directed = FALSE) {
  my_graph=igraph::graph_from_data_frame(d=data, directed=directed)
  ggraph::ggraph(my_graph) + ggraph::geom_edge_link() + ggraph::geom_node_point()
}

#Flow diagram
plot_chord_diagram <- function(data) {
  circlize::chordDiagram(data, transparency=0.5)
}
