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

  aes_val<-aes()

  if(!is.na(color)){
    #If the user enters internal ID
    color<-if(color == "internalID") "minID" else color

    if(!(color %in% colnames(data))){
      print("The color variable you've specificed is not in the data. Skipping..")
    }else{
      aes_val<-aes_val + aes_string(fill = color)
    }

  }

  gg_chart<-ggplot2::ggplot(data=data,aes_val)+
    ggplot2::geom_sf()+
    theme_bw()

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

  #for geographic maps a data frame is expected with lat and long
  #for shape files, use choropleth maps.

  # ---- TO DO : Gracefully re-direct ----
  if(!is.data.frame(data)){
    stop("To generate a geographic map, use tabular data and specify lat and long co-ordinates")
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

  #Draw the map
  #Create a bounding box
  bbox<-c(min(data[,long]),min(data[,lat]),
          max(data[,long]),max(data[,lat]))

  aes_val<-aes_string(x = long,y=lat)

  if(!is.na(color)){
    aes_val<-aes_val + aes_string(color = color)
  }

  #automatically set the alpha based upon the number of points
  #in future, this could be a place to automatically cluster data


  #now render it
  gg_chart<-ggmap::get_stamenmap(bbox,maptype = "toner-lite",zoom=6) %>%
  ggmap::ggmap()+
    geom_point(data=data,aes_val)

  return(gg_chart)
}

