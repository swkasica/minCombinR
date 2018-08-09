#Helper functions for relational chart types

#Node link
plot_node_link <- function(data, directed=FALSE) {
  my_graph=igraph::graph_from_data_frame(d=data, directed=directed)
  ggraph::ggraph(my_graph) + ggraph::geom_edge_link() + ggraph::geom_node_point()
}

#Flow diagram
# TODO: I don't like this package. Find a new one or a way to fix it.
# default colour_mark_type is grid
#Use RCircos... and consider Sankey diagram
#'@param colour_scale A vector of colors with named values
#'@param colour_mark_type A character indicating the type of mark to colour, can be one of: "default" (outside grid) or "links"
plot_flow_diagram <- function(data, colour_scale=NA, colour_mark_type=NA) {

  #TODO: This is an exception where the colour_scale has to have the grid or link values
  #Many typed linked options
  if (!is.na(colour_mark_type)) {
    if (colour_mark_type == "default") {
      if (!is.na(colour_scale)[1]) {
        circlize::chordDiagram(data, grid.col = colour_scale, transparency=0.5)
      }
    } else if (colour_mark_type == "links") {
      circlize::chordDiagram(data, col = colour_scale, transparency=0.5)
    }
  }

  else {
    circlize::chordDiagram(data, transparency=0.5)
  }
}
