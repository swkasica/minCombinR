#helper functions for spatial chart types

#Choropleth
# TODO: Hopes for input: data as a data frame, fill as a column in the data frame (no shp file required)
#       Involves: Convert the data frame into a shape file (need coordinate reference system, which seems tricky to get
        # but maybe (?) not impossible : http://rspatial.org/spatial/rst/6-crs.html
plot_choropleth <- function(data, fill) {
  ggplot() +
    geom_polygon(data = data,
                 aes(x = data$long, y = data$lat, group = data$group, fill = fill),
                 color = "black", size = 0.25) +
    coord_map()
}


#Geographic Map
# TODO: data frame with lat and lon
#    Involves: Setting the view in leaflet to view all points and plotting all points (easy to do)
plot_geographic_map <- function(lat, long) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView( lng = long, lat = lat, zoom = 8 )
}

#For interior map just input the image
