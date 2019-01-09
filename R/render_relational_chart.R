#' Node-link diagram
#' @title render_node_link
#' @param ...
#'
#' @return
render_node_link <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }

  my_graph <- igraph::graph_from_data_frame(d = data, directed = directed)
  graph_chart <- ggraph::ggraph(my_graph, layout = "kk") + ggraph::geom_edge_link() + ggraph::geom_node_point()

  if (!is.null(edge_col_var)) {
    if (is.null(edge_col_palette)) {
      colours <- get_colour_palette(data, edge_col_var)
    } else {
      colours <- edge_col_palette
    }

    graph_chart <- graph_chart %+%
      ggraph::geom_edge_link(aes_string(color = edge_col_var)) %+%
      ggraph::scale_edge_colour_manual(name = edge_col_var, values = colours)
  }

  if (!is.null(node_col_var)) {
    graph_chart <-
      graph_chart %+% ggraph::geom_node_point(aes(color = node_col_var))
  }

  return(graph_chart)
}


#' Rendering a chord diagram
#' @title render_chord
#' @param ...
#'
#' @return
render_chord <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }

  #TODO: This is an exception where the colour_scale has to have the grid or link values
  #Many typed linked options
  if (!is.na(colour_mark_type)) {
    if (colour_mark_type == "default") {
      if (!is.na(colour_scale)[1]) {
        circlize::chordDiagram(data, grid.col = colour_scale, transparency=0.5)
        tmp <- recordPlot()
        circlize::circos.clear()
        return(tmp)
      }
    } else if (colour_mark_type == "links") {
      circlize::chordDiagram(data, col = colour_scale, transparency=0.5)
      tmp <- recordPlot()
      circlize::circos.clear()
      return(tmp)
    }
  }

  else {
    tmp <- circlize::chordDiagram(data, transparency=0.5)
    gridGraphics::grid.echo()
    return(grid::grid.grab())
  }
}
