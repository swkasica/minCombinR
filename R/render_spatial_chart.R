#Helper functions for spatial chart types

# Rendering a choropleth plot
#' @title render_choropleth
#'
#' @param ...
#'
#' @return
render_choropleth <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }

  ggchart <- ggplot() +
    geom_polygon(data = data,
                 aes_string(x = lat_var, y = long_var, group = group, fill = fill),
                 color = "black", size = 0.25) +
    coord_map()

  if (flip_coord) {
    gg_chart <- gg_chart %>% coord_flip()
  }

  return(gg_chart)
}


#Geographic Map
# TODO: data frame with lat and lon
#    Involves: Setting the view in leaflet to view all points and plotting all points (easy to do)
#path is a path to a rds file.
#' Rendering a Geographic Map
#' @title render_geographic_map
#' @param ...
#'
#' @return
render_geographic_map <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #if a character has been passed as the name, get that variable from the environment
  if(!is.data.frame(data)  && (class(data) %in% c("character","factor"))){
    data<-get(data,envir = globalenv())  #get data from the global environment
  }


  #if arguements are passed as lat long instead of latitude and longitude
  if(is.na(lat) & !is.na(y)){
    data<-dplyr::rename(data,lat = y)
  }else if(!is.na(lat)){
    data$lat<-data[[lat]]
  }

  if(is.na(long) & !is.na(x)){
    data<-dplyr::rename(data,long = x)
  }else if(!is.na(long)){
    data$long<-data[[long]]
  }

  map_chart <- leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
    leaflet::addCircleMarkers(~long, ~lat)
  return(map_chart)
}

