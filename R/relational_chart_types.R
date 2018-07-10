#Helper functions for relational chart types

#Node link
plot_node_link <- function(data, directed = FALSE) {
  my_graph=igraph::graph_from_data_frame(d=data, directed=directed)
  ggraph::ggraph(my_graph) + ggraph::geom_edge_link() + ggraph::geom_node_point()
}

#Flow diagram
# TODO: I don't like this package. Find a new one or a way to fix it.
#Use RCircos... and consider Sankey diagram
plot_flow_diagram <- function(data) {
  circlize::chordDiagram(data, transparency=0.5)
}
