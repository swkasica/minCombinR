#************************************************************************************
#
# Function that lets user know the chart types and their required and possible specs
#
#************************************************************************************

#' Return a list of all chart types that are currently supported in gevitR
#' @title base_chart_types
#' @return character list of all chart types
#' @export
#'
#' @examples
#' '\dontrun{
#'  base_chart_types()
#' }
base_chart_types<-function(){
  return(gevitr_env$all_chart_types)
}


#' Return a list of all specifications for a chart type.
#' @title chartSpecs
#' @param chartType
#'
#' @import dplyr
#' @return character list of chart specifications
#' @export
#'
#' @examples
#' '\dontrun{
#' base_chart_specs(chartType = "phylogenetic_tree")
#' }
base_chart_specs<-function(chart_type = NULL){
  if(is.null(chart_type))
    stop("Please specify a chart type to see it's specifications. If you don't know the possible chart types, run the commoan base_chart_types() to get a list")

  if(!(chart_type %in% gevitr_env$all_chart_types))
    stop("That chart type is not supported. If you don't know the possible supported chart types, run the commoan base_chart_types() to get a list")

  charVar <- chart_type #weird little fix
  specs<-dplyr::filter(chart_specifications,chart_type == charVar) #from sysdata.R
  #create some text of the user to specify the possibilities
  required_param<-dplyr::filter(specs,param_required=="yes")$param %>% paste(.,collapse=", ")
  optional_param<-dplyr::filter(specs,param_required=="no")$param %>% paste(.,collapse=", ")

  spec_txt<-sprintf("Chart type: %s", chart_type)
  spec_txt<-c(spec_txt,sprintf("You must specify the follow parameters: %s",required_param))
  spec_txt<-c(spec_txt,sprintf("You have the option of specifying variables for the following parameters: %s",optional_param))

  return(spec_txt)
}

#************************************************************************************
#
# Primary user functions
#
#************************************************************************************

#' Specify a base chart type
#'
#' @title specify_base
#' @param chartType
#' @param data
#' @param ...
#'
#' @return a specification for a base chart type that will be read by gevitR::plot
#' @export
specify_base<-function(chart_type=NULL, data=NULL,...){
  if(is.null(chart_type))
    stop("Please specify a chart type. Use base_chart_types() to see a list of chart types")

  if(!(chart_type %in% gevitr_env$all_chart_types))
    stop("That chart type is not supported. If you don't know the possible supported chart types, run the commoan base_chart_types() to get a list")

  if(is.null(data))
    stop("Please specify data for chart")

  if(!exists(data))
    stop("Please specify data that you've already loaded into this R session")

  #Check that the user has specified all the required variables for the base chart type
  arg_vals<-as.list(match.call(expand.dots = TRUE))

  #check that user has provided the necessary arguments for base chart type
  args_passed<-names(arg_vals)

  charVar<-chart_type #odd little fix
  required_specs<-dplyr::filter(chart_specifications,chart_type == charVar) %>%
    dplyr::filter(param_required=="yes")

  if(!all(required_specs$param %in% args_passed)){
    #the user has not specified all of the required parameters for a chart type
    missing_param<-which((required_specs$param) %in% args_passed == FALSE)
    stop(sprintf("The following parameters MUST be specified: %s",paste(required_specs$param[missing_param],collapse = ", ")))
  }

  #The user has specified all the necessary paramters for a specific chart type (yay!)
  #Retunt the specification for later plotting
  call_info<-match.call()
  class(call_info)<-c(class(call_info),"gevitSpec")
  return(call_info)
}

#PLOTTING FUNCTION FOR GEVITR

#' Plot function for gevitR. To use this function, the user only actually has to type plot
#'
#' @title plot.gevitR
#' @param specs
#'
#' @return a plot that is displayed to the screen
#' @export

plot.gevitSpec<-function(specs = NULL){
  #Verify that a gevitR specification has been passed
  if(is.null(specs))
    stop("Please create chart specifications. See ?specify_base for details.")

  if(!("gevitSpec" %in% class(specs)))
    stop("Please create chart specifications. See ?specify_base for details.")


  #First, generate all the basic charts that will be plotted
  spec_list<-as.list(specs)[-1]#don't need the function call moving forwards

  #Second, if the user has specificed multiple charts, to be combined make the combinations
  spec_plot <- do.call(plot_simple, args = spec_list)

  plot(spec_plot)

}




