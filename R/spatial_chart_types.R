#Helper functions for spatial chart types

#Choropleth
plot_choropleth <- function(data, lat_var, long_var, fill, group, flip_coord=FALSE) {

  # #Group is required to make the choropleth without gaps.

  #TODO: Currently requires that the user specify a group but is this necessary?
  # commented section below doesn't work but is there a better way to do this?
  # if (is.na(group)) {
  #   data <- data %>% mutate(group = "group1")
  #   group <- "group"
  # }

  ggchart <- ggplot() +
    geom_polygon(data = data,
                 aes_string(x = lat_var, y = lon_var, group = group, fill = fill),
                 color = "black", size = 0.25) +
    coord_map()

  if (flip_coord) {
    gg_chart <- gg_chart %>% coord_flip()
  }

  gg_chart
}


#Geographic Map
# TODO: data frame with lat and lon
#    Involves: Setting the view in leaflet to view all points and plotting all points (easy to do)
#path is a path to a rds file.
plot_geographic_map <- function(data, lat_var, long_var) {

  dat <- dplyr::rename(data, lat_var = "Lat", long_var = "Long")
  leaflet(dat) %>%
    addTiles() %>%
    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  #%>% addMarkers(~long, ~lat)

  #This is also a possibile implementation:
  # guinea_shp <- rgdal::readOGR("data//ebov_vis-master//Guinea-Admin1//GIN_adm1_1m_ocha.shp", "GIN_adm1_1m_ocha", TRUE)
  # leaflet::leaflet(guinea_shp) %>% leaflet::addTiles() %>% leaflet::addPolygons(fill=FALSE, color="#000")

  #This is another possible implementation:
  # leaflet::leaflet() %>%
  #   leaflet::addTiles() %>%
  #   leaflet::setView( lng = long, lat = lat, zoom = 8 )
}

#For interior map just input the image
