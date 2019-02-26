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


#' Method that checks a user has provided a valid parameter to the specification
#'
#' @title check_param
#' @param ...
#'
#' @return

check_param<-function(...){
  params<-list(...)
  incorrect<-setdiff(names(params),gevitr_env$param_defaults)

  if(length(incorrect)>0){
    print("The following parameters are not valid and will be ignored : %s",paste(incorrect,sep=", "))
    print("To see the full list of valid parameters type get_params()")

    for(item in incorrect){
      params[[item]]<-NULL
      params<-Filter(Negate(is.null), params)
    }
    return(list(invalid = TRUE, revised_args = params))
  }

  return(list(invalid = FALSE, revised_args = NULL))
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

  # ---- a special check for image data ---
  if(chart_type == "image"){
    tmpDat<-get(data,envir=globalenv())
    if(class(tmpDat) !="gevitDataObj"){
      stop("For image data, only items created by this package are supported. See online resources to learn more")
    }
    rm(tmpDat)
    gc()

  }

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

  #finally, check that the user has passed valid parameters overall
  checkValid<-check_param(arg_vals)

  if(checkValid$invalid){
    arg_vals<-checkValid$revised_args
  }

  #for special variables, store the information as x and y
  tmp_names<-names(arg_vals)
  if("start" %in% tmp_names){arg_vals$x<-arg_vals$start}
  if("longitude" %in% tmp_names){arg_vals$x<-arg_vals$longitude}
  if("latitude" %in% tmp_names){arg_vals$x<-arg_vals$latitude}

  #arg_vals$call<-match.call()
  class(arg_vals)<-c(class(arg_vals),"gevitSpec","baseSpecs")

  #The user has specified all the necessary paramters for a specific chart type (yay!)
  #Retunt the specification for later plotting
  return(arg_vals)
}

#' Specify combination
#'
#' @param combo_type
#' @param base_charts
#' @param facet_by
#' @param link_by
#' @param alignment
#'
#' @return
#' @export
specify_combination<-function(combo_type=NA,
                              base_charts = NA,
                              facet_by=NA,
                              link_by=NA,
                             # link_mark_type=NA,
                              alignment = NA){


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
  #combo_specs<-base::Filter(Negate(is.na), combo_specs)

  #combo_spec_passed<-names(combo_specs)


  #making sure all the necessary parameters are passed for specfic types of combinations

  # ------- Small Multiples  -------
  if(combo_type == "small_multiple"){
    #if(!(all("facet_by" %in% combo_spec_passed)))
    if(is.na(combo_specs$facet_by))
    stop("Not all parameters specified for small multiples")

  # ------- Many Types Linked  -------
  }else if(combo_type == "color_linked"){
    if(is.na(combo_specs$link_by)){
      stop("Stop! You must specifcy a linking variable for many types linked")
    }
    #if(!(all(c("link_var") %in% combo_spec_passed))){
    #  stop("Stop! You must specifcy a linking variable for many types linked")
    #}
    chart_info<-data.frame(data=NULL,
                           chart_type=NULL,
                           chart_var=NULL,
                           stringsAsFactors = FALSE)

    for(spec in base_charts){
      chart_specs<-get(spec,envir=globalenv())
      if(!is.null(chart_specs$color)){
        warning(sprintf("Overriding the color variable that you've specificed for %s",spec))
      }

      spec_info<-data.frame(chart_name = spec,
                            data = chart_specs$data,
                            chart_type = gsub("\\s+","_",tolower(chart_specs$chart_type)),
                            x = ifelse(!is.null(chart_specs$x),chart_specs$x,paste(spec,"gevitr_checkID",sep="_")), #id is a special place holder that says "look up the id from the chart type"
                            y = ifelse(!is.null(chart_specs$y),chart_specs$y,NA),
                            stringsAsFactors = FALSE)

      chart_info<-rbind(chart_info,spec_info)
    }

    #return all charts that are linkable because there is a common variable
    #compat_charts<-return_compatible_chart_link(chart_info,combo_type = "composite")

    #for each of the charts, confirm that the linking variable exists
    #and that it has the same "levels" as the other chart types
    lvl_check<-c()
    for(chart in chart_info$chart_name){
      chart_name<-chart
      chart<-get(chart,envir=globalenv())

      dat<-get(chart$data,envir=globalenv())
      if(is.data.frame(dat)){
        if(link_by %in% colnames(dat)){
          var_lvls<-unique(dat[,link_by])
          lvl_check<-rbind(lvl_check,cbind(rep(chart_name,length(var_lvls)),var_lvls))
        }
      }else if("gevitDataObj" %in% class(dat)){
        if(dat@type == "table"){
          if(link_by %in% colnames(dat@data[[1]])){
            tmp_dat<-dat@data[[1]]
            var_lvls<-unique(tmp_dat[,link_by])
            lvl_check<-rbind(cbind(rep(chart_name,length(var_lvls)),var_lvls),lvl_check)
          }
        }else{
          if(!is.null(dat@data$metadata)){
            #there is associated metadata
            if(link_by %in% colnames(dat@data$metadata)){
              tmp_dat<-dat@data$metadata
              var_lvls<-unique(tmp_dat[,link_by])
              lvl_check<-rbind(cbind(rep(chart_name,length(var_lvls)),var_lvls),lvl_check)
            }
          }else{
            #there is no associated meta
            #check if there is a perfect discovered a perfect link
            comp_charts<-setdiff(chart_info$chart_name,chart_name)
            #for(comp_chart in comp_charts){
            #  comp_info<-dplyr::filter(comp_charts, name == comp_chart)
            #  comp_dat<-get(comp_info$data)
            #}

            #compat_tmp<-dplyr::filter(compat_charts, chart_one == chart_name | chart_two == chart_name)

            if(length(comp_charts)>0){
              #metasrc<-setdiff(unname(unlist(compat_tmp[,c(1,2)])),chart_name)
              metasrc<-comp_charts
              metadat<-NULL
              for(item in metasrc){
                meta_spec<-get(item,envir=globalenv())
                meta_tmp<-get(meta_spec$data,envir=globalenv())
                if(meta_tmp@type == "table"){
                  dat_raw<-get_raw_data(dat)
                  comp_raw<-get_raw_data(meta_tmp)

                  data_link<-check_link(dat_raw,comp_raw)

                  #only tables can be metadata
                  if(length(data_link)>0 & (link_by %in% colnames(meta_tmp@data[[1]]))){
                    dat_tmp<-meta_tmp@data[[1]]
                    #now report those linkages
                    var_lvls<-unique(dat_tmp[,link_by])
                    lvl_check<-rbind(lvl_check,cbind(rep(chart_name,length(var_lvls)),var_lvls))

                    #add this to our data object
                    dat@data$metadata<-dat_tmp
                    assign(chart$data,dat,envir=globalenv())
                    break() #once you find the first compatability, stop
                  }
                }
              }
            }
          }
        }
      }else{
        warning(sprintf("%s has a dataype that is currently not supported by color_linked combinations. It will be dropped from the specification."))
      }
    }
    if(nrow(lvl_check)==0){
      stop("Linking variables between these chart types do not appear to be compatible. Cannot carry out this combination")
    }

    keep_charts<-unique(lvl_check[,1])

    if(length(keep_charts)==1){
      stop("Linking variables between these chart types do not appear to be compatible. Cannot carry out this combination")
    }

    #Now check that these linking variables have the same levels
    #cause data can have the same column name but not the same entities
    lvl_check<-data.frame(lvl_check)
    colnames(lvl_check)<-c("chart","lvl")

    # TO DO: Right now, this work if there are exact matches - not so much of inexact matches
    keep_lvls<-lvl_check %>%
      group_by(lvl) %>%
      count() %>%
      mutate(match_items = n == length(keep_charts)) %>%
      filter(match_items)

    if(nrow(keep_lvls)==0){
      #TO DO in future: find a combo that works, for now, just warn the user
      stop("Linking variables between these chart types do not appear to be compatible. Cannot carry out this combination")
    }

    keep_chart<-lvl_check %>% dplyr::filter(lvl %in% keep_lvls$lvl) %>% dplyr::select(chart)

    if(length(setdiff(combo_specs$base_charts,keep_chart$chart)) !=0){
      throw_out<-setdiff(combo_specs$base_charts,keep_chart$chart)
      warning("The following charts where not compatible with others for a color linkage and have been dropped from the specification:%s",paste(throw_out,sep=", "))
      combo_specs$base_charts<-keep_chart$chart
    }

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

      if(length(base_charts)< 2){
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
      #suggested_specs<-c()

      warning("There are multiple *charts types* listed that are not compatible in a composite combination. Please see the suggested comptabilities below. Please re-run this command with one of these suggestions:")

      for(i in 1:nrow(lead_charts)){
        #Find compatible charts with the master
        lead_chart<-lead_charts[i,'chart_type']
        tmp<-sub_mat[m_chart,] #return the vector just for that master chart
        compat<-names(tmp)[tmp==1]

        #get the names of other compatible charts
        tmp_info<-dplyr::filter(chart_info, chart_type %in% compat)
        compt_oth<-paste((setdiff(tmp_info$chart_name,lead_charts[i,'chart_name'])),collapse=", ")

        #return a friendly message
        print(sprintf("%s is compatible with: %s",lead_charts[i,'chart_name'],compt_oth,collapse=", "))
      }
      stop()
    }

    #check that all chart types of the chart are compatible with each other, it at least come order
    idx_not_compat<-which(rowSums(sub_mat) == 1)

    if(length(idx_not_compat)>0){
      not_compat<-dplyr::filter(chart_info,chart_type %in% colnames(sub_mat)[idx_not_compat])
      base_charts<-setdiff(base_charts,not_compat$chart_name)

      if(length(base_charts)<2){
        stop("None of these charts are compatible for a composite combination")
      }else{
        combo_specs$base_charts<-base_charts
        warning(sprintf("The following charts are not compatible with any other chart types and have been removed from the specification: %s", paste(not_compat$chart_name,collapse = ", ")))
      }
    }

    #finally, quickly confirm that the charts have some common variable to spatially align on
    chart_info<-dplyr::filter(chart_info,chart_name %in% base_charts)

    compat_charts<-return_compatible_chart_link(chart_info,combo_type = "composite")
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

    # ------- Many Types Linked  ------
    if (specs$combo_type == "color_linked"){
      spec_plot<-do.call(plot_many_linked, args = specs,envir=parent.frame())
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


