render_image <- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #now do make the pretty picture
  imgRaster <- grid::rasterGrob(data, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = TRUE)

  p<-ggplot()+
    xlim(c(0,imgDetails$width))+
    ylim(c(0,imgDetails$height))+
    #annotation_custom(imgRaster, -Inf, Inf, -Inf, Inf) +
    annotation_custom(imgRaster, 0, imgDetails$width, 0, imgDetails$height)+
    theme(panel.background =  element_rect(color="white"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())

  if(!is.null(metadata) && !all(is.na(metadata))){
    df_data<-metadata

    df_point <- dplyr::filter(df_data,type=="point")
    df_shape <- dplyr::filter(df_data,type=="square")

    aes_rect<-aes(xmin=x,ymin=y,xmax=xmax,ymax = ymax,group=elemID)
    aes_point<-aes(x=x,y=y)

    #--- TO DO: Make this a little more elegant ---
    # also handle colours differently for rects and points..
    # a little bare bones right now
    if(!is.na(color)){
      aes_rect<-aes_rect + aes_string(fill=color)
      aes_point<-aes_point + aes_string(color=color)
    }

    p<- p +
      geom_point(data=df_point,aes_point,size=2)+
      geom_rect(data = df_shape,aes_rect,alpha = 0.2,colour="blue")+
      scale_fill_gradient(low = "pink",high ="red")+
      theme_bw()+
      theme(panel.background =  element_rect(color="white"),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  }

  return(p)

}
