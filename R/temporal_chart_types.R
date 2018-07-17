#Helper functions for temporal chart types
#TODO: convert to uniform input (data, x, y)

#Streamgraph
#Note: Can make using ggplot's geom_area but I don't like the look.
#TODO: Have a conversation about this chart... streamgraph is not on cran yet, which might affect our package getting onto cran
plot_streamgraph <- function(data, key, value, date) {
  # data <- dplyr::rename(data, key = key, value = value, date = date)
  stream_chart <- streamgraph(data = data, key = key, value = value, date = date, interactive = FALSE)
  stream_chart
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
