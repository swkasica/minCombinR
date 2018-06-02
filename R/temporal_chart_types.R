#Helper functions for temporal chart types

#Streamgraph
plot_streamgraph <- function(data, key = "key", value = "value", date = "date") {
  data <- dplyr::rename(data, key = key, value = value, date = date)
  streamgraph(data, interactive = FALSE)
}

#Annotated Timeline
plot_timeline <- function(data) {
  timelineS::timelineS(data)
}

#Stacked Timeline
#TODO: Obtain data to inform decision on input values
plot_stacked_timeline <- function(data, start, end, names, phase) {
  timelineG(data, start = start, end = end, names = names, phase = phase)
}
