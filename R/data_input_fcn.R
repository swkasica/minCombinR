#Generic input function for inputing data, helper functions are in the back
input_data<-function(file  = NA, dataType = NA, desc = NA, ...){
  #Only supports specific data types
  if(!(dataType %in% c("tree","table","dna","spatial","image")))
    stop("Data type is not supported. Please choose one of : tree, table, dna, spatial, image. Use ?input_data to learn more about the different input types.")

  switch(dataType,
         "table" = input_table(file,desc,...),
         "tree" = input_phyloTree(file,desc,...) ,
         "dna" = input_dna(file,desc,...),
         "spatial" = input_spatial(file,desc,...),
         "image" = input_image(file,desc,...))
}


#Helper function: inputs tables
input_table<-function(file=NA,stringsAsFactors = FALSE,desc=NA,...){
  #autodetect file type
  print("I will help you load in a table")
}

#Helper function : input dna
input_dna<-function(file=NA,desc = NA,...){
  print("I will help you load in some DNA")
}

#Helpfer function : input spatial data
input_spatial<-function(file=NA,desc = NA,...){
  print("I will help you load in some spatial data")
}

#Helpfer function: input tree
#' A helper function that reads in tree file data. Expecting either Newick or Nexus format
#'
#' @param file
#' @param desc
#' @param ...
#'
#' @return a phylo tree object
#' @import stringr
#'
#' @examples
input_phyloTree<-function(file = NA,desc = NA,...){
  #Make sure that tree has the right format to load
  if(!stringr::str_detect(file,"tree$|nwk$|tre$|nexus$")){
    stop("Phylogenetic tree file cannot be loaded. Please ensure that your tree has a .tree, .tre, .nwk, or .nexus format.")
  }

  #Try to load the tree, if for whatever reason it can't be loaded, throw error
  tree<-tryCatch(treeio::read.tree(file=file,...),
                 error = function(e) stop("Could not load tree."))

  #return the tree to the user
  return(tree)
}

#Helpder function : input_image
#' Input Image
#' A helper function for input_data
#' @param file
#' @param desc
#' @param ...
#'
#' @return
#' @import magick
#' @examples
input_image<-function(file = NA,desc = NA,...){
  img<-magick::image_read(path=file)

  #all images get resized so that they are mangeable to work with
  #arbitrarily set this 1000 pixels for max height width. Note that
  #rescaling will preserve the aspect ratio
  #smaller size also lets it load faster
  img<-magick::image_resize(img, "1000x1000")

  #get details for later
  imgDetails<-capture.output(img)

  #cleaning up the image details
  imgDetails<-data.frame(headerInfo= factor(c("img",unlist(strsplit(trimws(imgDetails[1]),"\\s+")))),
                         values = c(file,unlist(strsplit(trimws(imgDetails[2]),"\\s+"))[-1]))

  imgDetails<-tidyr::spread(imgDetails,headerInfo,values)
  imgDetails$width<-as.numeric(as.character(imgDetails$width))
  imgDetails$height<-as.numeric(as.character(imgDetails$height))

  #just return the image
  warning("To use this image, please be sure to have separate file that links the image to data in pixel space. If you would like to CREATE an annotation file, run the 'annotate_image' command.")
  return(list(img=img,imgDetails = imgDetails))

}

#helper method to annotate FEATURES within an image
annotate_image<-function(img=NULL, imgDetails = NULL, outfile = NULL){
  # If user does not provide a file name, make one up
  if(is.null(outfile)){
    outfile="annotated_image_file.csv"
  }

  annote_dat<-runApp(annotate_image_app(img,imgDetails))

  #cleaning up the annotatation data
  annote_dat<-data.frame(elemID =annote_dat[,1],
                 x = as.numeric(annote_dat[,2]),
                 y = as.numeric(annote_dat[,3]),
                 xmax = as.numeric(annote_dat[,4]),
                 ymax = as.numeric(annote_dat[,5]),
                 element_name = annote_dat[,6],
                 type = annote_dat[,7],
                 stringsAsFactors = FALSE)


  return(annote_dat)
}

#might go elsewhere, but essentially, calls a shinyapp like a function to allow a user to annotate their image
#and create the image file
#An important detail : all the spatial mappings are depend on OUR chioce of how to render the image
#So it's based on assuming 1000 pixel width. The transformations are meaningless if the image is resized.

annotate_image_app<-function(img,imgDetails){
  require(shiny)
  require(ggplot2)
  require(DT)
  require(grid)

  annotDat<-c() #global variable needed for return value on session end

  shiny::shinyApp(
    ui = fluidPage(
      h1("Welcome to annotator"),
      p("Here's what you can do. Click on the button to start annotating items on your image. Complex polygons are not currently supported, so please pick the centriod (i.e middle of a room) or some other landmark to annotate data"),
      br(),
      fluidRow(
        column(6,
               plotOutput("testPlot",dblclick = "plot_click",brush = "plot_brush",height="1000px")),
        column(6,
               textInput(inputId = "elementID",label="Element Name",
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

