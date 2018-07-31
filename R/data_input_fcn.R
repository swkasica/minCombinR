#Generic input function for inputing data, helper functions are in the back
input_data<-function(file  = NA, dataType = NA, desc = NA, ...){
  #Only supports specific data types
  if(!(dataType %in% c("tree","table","dna","spatial","image")))
    stop("Data type is not supported. Please choose one of : tree, table, dna, spatial, image. Use ?input_data to learn more about the different input types.")

  print(as.list(match.call()))

  switch(dataType,
         "table" = input_table(file,desc,...),
         "tree" = input_phyloTree(file,desc,...) ,
         "dna" = input_dna(file,desc,...),
         "spatial" = input_spatial(file,desc,proj4String,...),
         "image" = input_image(file,desc,...))
}


#Helper function: inputs tables
#TO DO: set up object return
input_table<-function(file=NA,stringsAsFactors = FALSE,desc=NA,asObj=TRUE,...){
  #autodetect file type
  #basic, assumes CSV
  dat<-read.csv(file=file,stringsAsFactors = stringsAsFactors,header=TRUE)

  return(dat)
}

#Helper function : input dna
input_dna<-function(file=NA,desc = NA,...){
  print("I will help you load in some DNA")
}

#Helpfer function : input spatial data
input_spatial<-function(file=NA,desc = NA,...){

  if(!stringr::str_detect(file,"shp$")){
    stop("Currently, the input file only loads shapefiles. Is your map an image file? Please choose set type to 'image' in order to load it properly")
  }

  nc <- sf::st_read(file, quiet = TRUE)

  #convert everythign to same layer in event for multiple maps
  #doesn't alway work well
  nc<- sf::st_transform(nc, "+init=epsg:3857")

  objDat<-new("gevitDataObj",
              type = "spatial",
              source = file,
              data = list(geometry=nc)
  )

  return(objDat)

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
input_phyloTree<-function(file = NA, desc = NA,sepLabel = NA,metadata=NULL,...){
  #Make sure that tree has the right format to load
  if(!stringr::str_detect(file,"tree$|nwk$|tre$|newick$|nexus$")){
    stop("Phylogenetic tree file cannot be loaded. Please ensure that your tree has a .tree, .tre, .nwk, or .nexus format.")
  }

  #Try to load the tree, if for whatever reason it can't be loaded, throw error
  tree<-tryCatch(treeio::read.tree(file=file,...),
                 error = function(e) stop("Could not load tree."))


  if(!is.na(sepLabel)){
    #if it is a special character add escape
    if(grepl('[[:punct:]]', sepLabel))
      sepLabel<-paste0("\\",sepLabel)

    #Think more about how you want to handle the error messages
    tipDat<-tryCatch(do.call(rbind,strsplit(tree$tip.label,sepLabel)),
                     error = function(e) return(NULL))
  }else{
    tipDat<-NULL
  }

  #if the user added metadata load too
  if(!is.null(metadata)){
    metadata<-input_table(file = metadata,asObj=FALSE)

    #quick scan for column that matches node labels
    tipLab<-tree$tip.label
    containsLabs<-apply(metadata,2,function(x){sum(tipLab %in% x)})
    if(max(containsLabs) == 0){
      warning("Current metadata file does not contain column matching tree labels")
      linkVar <- NULL
    }else{
      #Best guess of which columns contain the label information
      linkVar<-names(which(containsLabs == max(containsLabs)))
    }
  }
  #return the tree and parsed tip data (if that's what the user wants)
  #Is there more I should get out of a tree?

  objDat<-new("gevitDataObj",
              type = "tree",
              source = file,
              data = list(tree=tree)
  )

  if(!is.null(tipDat))
    objDat@data$tipData<-tipDat

  if(!is.null(metadata))
    objDat@data$metadata<-metadata

  if(!is.null(linkVar))
    objDat@data$linkVar<-linkVar

  return(objDat)

  #return(list(tree=tree,nodeDat = tipDat))
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

  objDat<-new("gevitDataObj",
            type = "image",
            source = file,
            data = list(img=img,imgDetails = imgDetails)
           )

  return(objDat)
}

#helper method to annotate FEATURES within an image
annotate_image<-function(img = NULL,imgDetails=NULL,outfile = NULL){
  # If user does not provide a file name, make one up
  if(is.null(outfile)){
    outfile="annotated_image_file.csv"
  }

  if(class(img) == "gevitDataObj"){
    annote_dat<-runApp(annotate_app(img@data$img,img@data$imgDetails))
    #
  }else{
    annote_dat<-runApp(annotate_image_app(img,imgDetails))
  }
  #cleaning up the annotatation data
  annote_dat<-data.frame(elemID =annote_dat[,1],
                 x = as.numeric(annote_dat[,2]),
                 y = as.numeric(annote_dat[,3]),
                 xmax = as.numeric(annote_dat[,4]),
                 ymax = as.numeric(annote_dat[,5]),
                 element_name = annote_dat[,6],
                 type = annote_dat[,7],
                 stringsAsFactors = FALSE)

  if(class(img) == "gevitDataObj"){
    img@data$annotate<-annote_dat
    return(img)
  }else{
    return(annote_dat)
  }

}
