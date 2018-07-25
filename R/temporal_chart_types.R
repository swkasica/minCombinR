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
#There are two types of timelines.
# The first is an annotated timeline, where event descriptions are shown on corresponding dates.
#     This version requires data with a column for events (called 'Events' or specified using events input)
#     and a column for the corresponding dates.
# The second is a stacked version (timelineG). This version can be facetted.
#     This version requires a data frame and its specified start, end, names and phase columns.
#The algorithm will make an annotated timeline if the data provided has a start, end and phase column or the start, end and phase paramaters are provided.
#Otherwise, the algorithm will create a stacked version.
#TODO: Are there better names for any of these inputs?
plot_timeline <- function(data, start=NA, end=NA, names=NA, phase=NA, events=NA) {

  if(!is.na(start) && !is.na(end) && !is.na(phase) && !is.na(names)) {
    #Stacked
    timelineS::timelineG(data, y, start = start, end = end, names = names, phase = phase)
  } else {
    #Annotated
    timelineS::timelineS(data, labels = data[[events]])
  }

  #TODO: some sort of autocomplete where the user doesn't have to specify start, end, names and phase if they have those names already
  #If this is the case, then I will probably need to include another var input to determine which timeline type to render

  # else if (is.na(events)) {
    # timelineS::timelines(data)
  #}
  #TODO: some sort of autocomplete where the user doesn't have to specify events.

}

#Stacked Timeline
#TODO: Obtain data to inform decision on input values
# plot_stacked_timeline <- function(data, start, end, names, phase) {
#   timelineS::timelineG(data, start = start, end = end, names = names, phase = phase)
# }
