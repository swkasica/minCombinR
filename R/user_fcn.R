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

  #arg_vals$call<-match.call()
  class(arg_vals)<-c(class(arg_vals),"gevitSpec","baseSpecs")

  #The user has specified all the necessary paramters for a specific chart type (yay!)
  #Retunt the specification for later plotting
  return(arg_vals)
}

specify_combination<-function(combo_type=NA,
                              base_charts = NA,
                              facet_by=NA,
                              link_var=NA,
                              link_mark_type="default",
                              alignment = NA,
                              common_var=NA,
                              order=NA){

  if(is.na(combo_type) | length(combo_type)>1)
    stop("Please specify a combination that is either: small_multiple, composite, many_types_linked, many_types_general. Please see www.gevit.net for examples of each type of combination")

  #check that you have more than two charts and that they are gevitR base specs
  if(!(class(base_charts) == "character"))
    stop("Please pass the charts names as a character vector. For example, as c('bar_chart','scatter_chart'")

  if(length(base_charts)<2 & combo_type != "small_multiple"){
    stop("Only one chart provided, cannot form a combination")
  }else if(length(base_charts)>=2 & combo_type == "small_multiple"){
    stop("For small multiples, just specify one chart and the facet_by variable")
  }

  #get all elements passed to function, including defaults that were not passed
  combo_specs<- as.list(environment())

  #get all the specifications that are not NA
  #keep everything that is not NA
  combo_specs<-base::Filter(Negate(is.na), combo_specs)

  combo_spec_passed<-names(combo_specs)

  #making sure all the necessary parameters are passed for specfic types of combinations
  if(combo_type == "small_multiple"){
    if(!(all("facet_by" %in% combo_spec_passed)))
    stop("Not all parameters specified for small multiples")
  }else if(combo_type == "many_types_linked"){
    if(!(all(c("link_var","line_mark_type") %in% combo_spec_passed)))
      stop("Not all parameters specified for many types linked")
  }else if(combo_type == "composite"){
    if(!(all(c("alignment","common_var","ordered") %in% combo_spec_passed)))
      stop("Not all parameters specified for composite")
  }

  class(combo_specs)<-c(class(combo_specs),"gevitSpec","comboSpecs")
  return(combo_specs)

}

#PLOTTING FUNCTION FOR GEVITR

#' Plot function for gevitR. To use this function, the user only actually has to type plot
#'
#' @title plot.gevitR
#' @param specs
#'
#' @return a plot that is displayed to the screen
#' @export

plot.gevitSpec<-function(specs = NULL,do_not_display=FALSE){
  #Verify that a gevitR specification has been passed
  if(is.null(specs))
    stop("Please create chart specifications. See ?specify_base for details.")

  if(!("gevitSpec" %in% class(specs)))
    stop("Please create chart specifications. See ?specify_base for details.")


  if("baseSpecs" %in% class(specs)){ #single plot
    #First, generate all the basic charts that will be plotted
    spec_list<-as.list(specs)[-1]#don't need the function call moving forwards
    #Second, if the user has specificed multiple charts, to be combined make the combinations
    spec_plot <- do.call(plot_simple, args = spec_list)

  }else if("comboSpecs" %in% class(specs)){ #multiple plots
    #instead of just making a simple specification, the user wants a combination

    # 1. Many Types General Combinations
    #"Many types genera"l means you just want to put a bunch of plots together
    #and they are not spatially or visually linked in any way
    if (specs$combo_type == "many_types_general") {
      base_specs<-c()
      for(spec_name in specs$base_charts){
        tmp<-get(spec_name,envir = globalenv())
        base_specs[[spec_name]]<-plot(tmp,do_not_display = TRUE)
      }
      spec_plot<-do.call(plot_many_types_general, args = base_specs)
    }

    #2. SMALL MULTIPLE COMBINATION
    # ggplot supports this using the facet command, but here
    # the design decision was to do this all via cowplots to
    # keep the interface consistent with different packages

    if (specs$combo_type == "small_multiple") {
      base_specs<-specs$base_charts #only one chart
      base_specs<-get(base_specs,envir = globalenv())
      base_specs<-append(base_specs,specs)

      spec_plot<-do.call(plot_small_multiples,args = base_specs,envir = parent.frame())
    }

  }

  # For combinations, may not want to display plot
  # but return the grob object instead
  if(do_not_display){
    return(spec_plot)
  }

  #Display the plot to the screen
  plot(spec_plot)

}


