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

  # ------- Small Multiples  -------
  if(combo_type == "small_multiple"){
    if(!(all("facet_by" %in% combo_spec_passed)))
    stop("Not all parameters specified for small multiples")

  # ------- Many Types Linked  -------
  }else if(combo_type == "many_types_linked"){
    if(!(all(c("link_var","line_mark_type") %in% combo_spec_passed)))
      stop("Not all parameters specified for many types linked")

  # ------- Composite  ------
  }else if(combo_type == "composite"){
    #if(!(all(c("alignment","common_var","ordered") %in% combo_spec_passed)))
    #  stop("Not all parameters specified for composite")
    #check that all charts passed are combinable, if not, only return those that are
    chart_info<-data.frame(data=NULL,
                           chart_type=NULL,
                           x=NULL,
                           y=NULL,
                           stringsAsFactors = FALSE)

    for(spec in base_charts){
      chart_specs<-get(spec,envir=globalenv())
      spec_info<-data.frame(chart_name = spec,
                            data = chart_specs$data,
                            chart_type = gsub("\\s+","_",tolower(chart_specs$chart_type)),
                            x = ifelse(!is.null(chart_specs$x),chart_specs$x,paste(spec,"gevitr_checkID",sep="_")), #id is a special place holder that says "look up the id from the chart type"
                            y = ifelse(!is.null(chart_specs$y),chart_specs$y,NA),
                            stringsAsFactors = FALSE)

      chart_info<-rbind(chart_info,spec_info)
    }

    #1. check if any charts are not alignable
    not_align<-dplyr::filter(chart_info,chart_type %in% gevitr_env$not_spatially_alignable)

    if(nrow(not_align)>=1){
      base_charts<-setdiff(base_charts,not_align$chart_name)

      print(sprintf("The following chart types cannot form a composite combination: %s. Composite combination will be formed with the following charts only: %s",paste(not_align$chart_name,collapse=","),paste(base_charts,collapse=", ")))

      chart_info<-dplyr::filter(chart_info,chart_name %in% base_charts)

      if(length(base_charts) == 0){
        stop("None of these charts are compatible in a composite")
      }
      combo_specs$base_charts<-base_charts
    }

    #2. Compatability check
    #comp_matrix lives inside sysdata of the package
    sub_mat<-comp_matrix[chart_info$chart_type,chart_info$chart_type]

    lead_charts<-dplyr::filter(chart_info,chart_type %in% gevitr_env$master_chart_types)

    if(nrow(lead_charts)>1){
      #for each master chart, return a set of suggested specification
      #to user the user, then exit function with an error
      suggested_specs<-c()

      warning("There are multiple *charts types* listed that are not compatible in a composite combination. Please see the suggested comptabilities below. Please re-run this command with one of these suggestions:")

      for(i in 1:nrow(lead_charts)){
        #Find compatible charts with the master
        lead_chart<-lead_charts[i,'chart_type']
        tmp<-sub_mat[m_chart,] #return the vector just for that master chart
        compat<-names(tmp)[tmp==1]

        #get the names of other compatible charts
        tmp_info<-dplyr::filter(chart_info, type %in% compat)
        compt_oth<-paste((setdiff(tmp_info$chart_name,lead_charts[i,'chart_name'])),collapse=", ")

        #return a friendly message
        print(sprintf("%s is compatible with: %s",lead_charts[i,'chart_name'],compt_oth,collapse=", "))
      }
      stop()
    }

    #check that all chart types of the chart are compatible with each other, it at least come order
    idx_not_compat<-which(rowSums(sub_mat) == 1)

    if(length(idx_not_compat)>0){
      not_compat<-dplyr::filter(chart_info,type %in% colnames(sub_mat)[idx_not_compat])
      base_charts<-setdiff(base_charts,not_compat$chart_name)

      if(length(base_specs)==0){
        stop("None of these charts are compatible for a composite combination")
      }else{
        combo_specs$base_charts<-base_charts
        warning(sprintf("The following charts are not compatible with any other chart types and have been removed from the specification: %s", paste(not_compat$chart_name,collapse = ", ")))
      }
    }

    #finally, quickly confirm that the charts have some common variable to spatially align on
    chart_info<-dplyr::filter(chart_info,chart_name %in% base_charts)

    compat_charts<-return_compatible_chart_link(chart_info)
    combo_specs$base_charts<-unique(unlist(compat_charts[,c(1,2)])) #final set of compatible chart types
    combo_specs$common_var<-unique(compat_charts$link)
    combo_specs$chart_info<-chart_info #useful for ordering and arranging plots


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

  # ------- Single Chart  ------
  if("baseSpecs" %in% class(specs)){ #single plot
    #First, generate all the basic charts that will be plotted
    spec_list<-as.list(specs)[-1]#don't need the function call moving forwards
    #Second, if the user has specificed multiple charts, to be combined make the combinations
    spec_plot <- do.call(plot_simple, args = spec_list)

  }else if("comboSpecs" %in% class(specs)){ #multiple plots
    #instead of just making a simple specification, the user wants a combination

    # ------- Many Types General  ------
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

    # ------- Small Multiples  ------
    # ggplot supports this using the facet command, but here
    # the design decision was to do this all via cowplots to
    # keep the interface consistent with different packages

    if (specs$combo_type == "small_multiple") {
      base_specs<-specs$base_charts #only one chart
      base_specs<-get(base_specs,envir = globalenv())

      #append the specifications for the chart to the specifications for the combination
      base_specs<-append(base_specs,specs)

      spec_plot<-do.call(plot_small_multiples,args = base_specs,envir = parent.frame())
    }

    # ------- Composite  ------
    if (specs$combo_type == "composite") {
      spec_plot<-do.call(plot_composite,args = specs, envir = parent.frame())
    }

  }

  # For combinations, may not want to display plot
  # but return the grob object instead
  if(do_not_display){
    return(spec_plot)
  }

  #Display the plot to the screen
  #plot(spec_plot)
  return(spec_plot)

}


