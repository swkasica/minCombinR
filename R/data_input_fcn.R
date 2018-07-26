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
  return(img)

}

#helper method to annotate FEATURES within an image
annotate_image<-function(img=NULL, outfile = NULL){
  # If user does not provide a file name, make one up
  if(is.null(outfile)){
    outfile="annotated_image_file.csv"
  }
}

#might go elsewhere, but essentially, calls a shinyapp like a function to allow a user to annotate their image
#and create the image file
annotate_image_app<-function(img,imgDetails){
  require(shiny)
  require(cowplot)
  require(ggplot2)
  require(DT)

  shiny::shinyApp(
    ui = fluidPage(
      h1("Welcome to annotator"),
      p("Here's what you can do. Click on the button to start annotating items on your image. Complex polygons are not currently supported, so please pick the centriod (i.e middle of a room) or some other landmark to annotate data"),
      br(),
      fluidRow(
        column(6,
               plotOutput("testPlot",click = "plot_click",height="1000px")),
        column(6,
               textInput(inputId = "elementID",label="Element Name",
                         placeholder = "Add name here, then click on plot"),
               DT::dataTableOutput("elementTable",width="80%"))
      )
    ),
    server = function(input,output,session){
      session$onSessionEnded(stopApp) #kill the app when the browser is closed

      #reactivedata
      values <- reactiveValues(df_data = NULL)

      output$testPlot<-renderPlot({
        #this image raster code allows it to be automatically resized according to display window that the plot is rendered into
        imgRaster <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = TRUE)
        ggplot()+
          xlim(c(0,imgDetails$width))+
          ylim(c(0,imgDetails$height))+
          annotation_custom(imgRaster, -Inf, Inf, -Inf, Inf) +
          theme_bw()
      })

      output$elementTable<-renderDataTable({
        if(is.null(values$df_data))
          return(NULL)

        values$df_data
      },editable = T)

      observeEvent(input$plot_click,{
        if(!input$elementID==""){
          values$df_data<-rbind(values$df_data,c(input$plot_click$x,input$plot_click$y,input$elementID))
          updateTextInput(session,"elementID",value="")
        }else{
          values$df_data<-rbind(values$df_data,c(input$plot_click$x,input$plot_click$y,"ADD ELEMENT ID"))
        }

        observeEvent(input$elementTable_cell_edit,{
          changeSite<-input$elementTable_cell_edit
          values$df_data[changeSite$row,changeSite$col+1]<-changeSite$value
        })
      })

    }
  )
}

