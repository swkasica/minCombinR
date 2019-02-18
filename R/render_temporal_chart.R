#Helper functions for temporal chart types
#TODO: convert to uniform input (data, x, y)

#Streamgraph
#Note: Can make using ggplot's geom_area but I don't like the look.
#TODO: Have a conversation about this chart... streamgraph is not on cran yet, which might affect our package getting onto cran
render_streamgraph_old <- function(data, key, value, date) {
  # data <- dplyr::rename(data, key = key, value = value, date = date)
  stream_chart <- streamgraph::streamgraph(data = data, key = key, value = value, date = date, interactive = FALSE)
  stream_chart
}

#' Render Timeline
#'
#' @param ...
#'
#' @return
render_timeline<-function(...){

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #right now, expect that this comes form metadata
  date_tmp<-data

  start<-if(is.na(start) & !is.na(x)) x else start

  aes_val<-aes_string(x = start, y = y)
  #check if start and (if applicable) end dates are actually dates
  #--- TO DO : Very loose, makes too many assumptions. Attend to more closely ---
  if(!class(date_tmp[,start]) %in% c("Date")){
    warning("Start date is not a date class. Will try to automatically convert it - unintended side effects may occur.")
    date_tmp[,start]<-as.Date(date_tmp[,start])
  }

  if(!is.na(end) && class(date_tmp[,end]) %in% c("Date")){
    aes_val<-aes_string(x = start, y = y , xend = end)
    warning("Start date is not a date class. Will try to automatically convert it - unintended side effects may occur.")
    date_tmp[,end]<-as.Date(date_tmp[,end])
  }

  p<-NULL

  if(is.na(end)){
    p<- ggplot(data=date_tmp)+
      geom_point(aes_val)+
      theme_bw()+
      theme(axis.text.y = element_blank())
  }else{
    #assumes there is only one point defined per item (start or end)
    df_point<-date_tmp %>% dplyr::filter(is.na(!!sym(end)) | is.na(!!sym(end)))

    #assumes everything has a starting point and that some or all items have and endpoint
    df_range<-dplyr::filter(date_tmp,!is.na(!!sym(end)))

    p<- ggplot()+
      geom_point(data = df_point,aes_string(x = start,y=y))+
      geom_segment(data = df_range,aes_string(x = start,xend = end,y=y,yend=y))+
      theme_bw()+
      theme(axis.text.y = element_blank())
  }
  return(p)
}


