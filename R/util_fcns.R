#might go elsewhere, but essentially, calls a shinyapp like a function to allow a user to annotate their image
#and create the image file
#An important detail : all the spatial mappings are depend on OUR chioce of how to render the image
#So it's based on assuming 1000 pixel width. The transformations are meaningless if the image is resized.
#NOTE : CURRENTLY ONLY TESTED ON IMAGE DATA, BUT EXPECTED TO BECOME MORE GENERIC IN THE FUTURE
#' Title
#'
#' @param img
#' @param imgDetails
#'
#' @return
#' @export
#'
#' @examples
annotate_app<-function(img,imgDetails){
  require(shiny)
  require(ggplot2)
  require(DT)
  require(grid)

  annotDat<-c() #global variable needed for return value on session end

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h1("Welcome to annotator"),
      shiny::p("Here's what you can do. Click on the button to start annotating items on your image. Complex polygons are not currently supported, so please pick the centriod (i.e middle of a room) or some other landmark to annotate data"),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6,
               shiny::plotOutput("testPlot",dblclick = "plot_click",brush = "plot_brush",height="1000px")),
        shiny::column(6,
               shiny::textInput(inputId = "elementID",label="Element Name",
                         placeholder = "Add name here, then click on plot"),
               DT::dataTableOutput("elementTable",width="80%"))
      )
    ),
    server = function(input,output,session){
      #when the user closes the browser the app stops running
      #and passes a dataframe of the annotations the annote_image function
      session$onSessionEnded(function(){stopApp(annotDat)})

      #reactivedata
      values <- reactiveValues(df_data = NULL,
                               pointObj = 0,
                               shapeObj = 0)

      imgBase<- reactive({
        imgRaster <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = TRUE)
        ggplot()+
          xlim(c(0,imgDetails$width))+
          ylim(c(0,imgDetails$height))+
          annotation_custom(imgRaster, -Inf, Inf, -Inf, Inf) +
          theme_bw()
      })

      output$testPlot<-renderPlot({
        #this image raster code allows it to be automatically resized according to display window that the plot is rendered into
        p<-imgBase()
        if(!is.null(values$df_data)){
          df<-data.frame(elemID =values$df_data[,1],
                         x = as.numeric(values$df_data[,2]),
                         y = as.numeric(values$df_data[,3]),
                         xmax = as.numeric(values$df_data[,4]),
                         ymax = as.numeric(values$df_data[,5]),
                         element_name = values$df_data[,6],
                         type = values$df_data[,7],
                         stringsAsFactors = FALSE)

          df_point <- dplyr::filter(df,type=="point")
          df_shape <- dplyr::filter(df,type=="square")

          p<- p +
            geom_point(data = df_point,aes(x =x,y=y),colour="red",size=2)+
            geom_rect(data = df_shape,aes(xmin=x,ymin=y,xmax=xmax,ymax = ymax,group=elemID),alpha = 0.2,colour="blue")+
            theme_bw()
        }

        p
      })

      #Table output of save elements
      output$elementTable<-renderDataTable({
        if(is.null(values$df_data))
          return(NULL)

        df<-data.frame(elemID =values$df_data[,1],
                       x = as.numeric(values$df_data[,2]),
                       y = as.numeric(values$df_data[,3]),
                       xmax = as.numeric(values$df_data[,4]),
                       ymax = as.numeric(values$df_data[,5]),
                       element_name = values$df_data[,6],
                       type = values$df_data[,7],
                       stringsAsFactors = FALSE)

        df
      },editable = T)


      #Add plot shapes
      observeEvent(input$plot_click,{
        type="point"
        elemID<-paste0(type,values$pointObj)
        values$pointObj<-values$pointObj+1

        if(!input$elementID==""){
          values$df_data<-rbind(values$df_data,c(elemID,input$plot_click$x,input$plot_click$y,NA,NA,input$elementID,type))
          updateTextInput(session,"elementID",value="")
        }else{
          values$df_data<-rbind(values$df_data,c(elemID,input$plot_click$x,input$plot_click$y,NA,NA,"ADD ELEMENT ID",type))
        }

        annotDat<<-values$df_data

      })

      #Add square shapes
      observeEvent(input$plot_brush,{
        type="square"
        elemID<-paste0(type,values$shapeObj)
        values$shapeObj<-values$shapeObj+1
        if(!input$elementID==""){
          values$df_data<-rbind(values$df_data,c(elemID,input$plot_brush$xmin,input$plot_brush$ymin,input$plot_brush$xmax,input$plot_brush$ymax,input$elementID,type))
          updateTextInput(session,"elementID",value="")
        }else{
          values$df_data<-rbind(values$df_data,c(elemID,input$plot_brush$xmin,input$plot_brush$ymin,input$plot_brush$xmax,input$plot_brush$ymax,"ADD ELEMENT ID",type))
        }

        annotDat<<-values$df_data

        session$resetBrush("plot_brush")
      })

      #observe and keep edits user makes to cell
      observeEvent(input$elementTable_cell_edit,{
        changeSite<-input$elementTable_cell_edit
        values$df_data[changeSite$row,changeSite$col+1]<-changeSite$value

        annotDat<<-values$df_data
      })

    }
  )

}
