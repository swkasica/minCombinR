#Helper functions for temporal chart types
#TODO: convert to uniform input (data, x, y)

#Streamgraph
plot_streamgraph <- function(data, key = "key", value = "value", date = "date") {
  data <- dplyr::rename(data, key = key, value = value, date = date)
  streamgraph(data, interactive = FALSE)
}

#Streamgraph using ggplot2
#http://austinwehrwein.com/tutorials/streams/

#Annotated Timeline
plot_timeline <- function(data, stacked=FALSE, start, end, names, phase) {
  if(stacked) {
    timelineS::timelineG(data, start = start, end = end, names = names, phase = phase)
  }
  timelineS::timelineS(data)
}

#Stacked Timeline
#TODO: Obtain data to inform decision on input values
# plot_stacked_timeline <- function(data, start, end, names, phase) {
#   timelineS::timelineG(data, start = start, end = end, names = names, phase = phase)
# }
