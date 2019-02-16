render_image <- function(...) {
  #cowplot::ggdraw() + cowplot::draw_image(path)
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())
  data_class<-class(data)

  #now do make the pretty picture
  imgRaster <- grid::rasterGrob(data, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = TRUE)

  p<-ggplot()+
    xlim(c(0,imgDetails$width))+
    ylim(c(0,imgDetails$height))+
    annotation_custom(imgRaster, -Inf, Inf, -Inf, Inf) +
    theme_bw()


  if(!is.null(metadata) && !all(is.na(metadata))){
    df_data<-metadata

    # df<-data.frame(elemID =df_data[,1],
    #                x = as.numeric(df_data[,2]),
    #                y = as.numeric(df_data[,3]),
    #                xmax = as.numeric(df_data[,4]),
    #                ymax = as.numeric(df_data[,5]),
    #                element_name = df_data[,6],
    #                type = df_data[,7],
    #                stringsAsFactors = FALSE)

    df_point <- dplyr::filter(df_data,type=="point")
    df_shape <- dplyr::filter(df_data,type=="square")

    aes_rect<-aes_string(xmin="x",ymin="y",xmax="xmax",ymax = "ymax",group="elemID")
    if(!is.na(color)){
      #--- TO DO: Make this a little more elegant ---
      aes_rect<-aes_string(xmin="x",ymin="y",xmax="xmax",ymax = "ymax",group="elemID",fill=color)
    }

    p<- p +
      geom_point(data = df_point,aes(x =x,y=y,color=color),colour="red",size=2)+
      geom_rect(data = df_shape,aes_rect,alpha = 0.2,colour="blue")+
      scale_fill_gradient(low = "pink",high ="red")+
      theme_bw()
  }

  return(p)

}
