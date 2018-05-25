#helper functions for spatial chart types

#Choropleth
plot_choropleth <- function(data, fill) {
  ggplot() +
    geom_polygon(data = data,
                 aes(x = data$long, y = data$lat, group = data$group, fill = fill),
                 color = "black", size = 0.25) +
    coord_map()
}


#Geographic Map
plot_geographic_map <- function(lat, long) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView( lng = long, lat = lat, zoom = 8 )
}

#For interior map just input the image
