#' For genomic data, return the positions where all nucleotides are NOT the same
#' @title get_diff_post
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
get_diff_pos<-function(dat){
  if(!"gevitDataObj" %in% class(dat)){
    stop("This method only works for gevitR objects")
  }

  if(dat@type != "dna"){
    stop("This method only works for DNA data")
  }

  data<-dat@data[[1]]
  tmp<-data.frame(t(bind_rows(as.character(data))),stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column(var = "idvar")

  diff<-apply(tmp,2,function(x){length(setdiff(unique(x),"n"))})
  tmp<-tmp[,which(diff>1)]

  return(gsub("^X","",colnames(tmp)))

}

#' Render Seqlogo
#'
#' @title render_seqlogo
#' @param ...
#'
#' @return
render_seqlogo <- function(...) {

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #special parameters for this method
  diff_only<-if(exists('diff_only')) diff_only else TRUE
  if(!exists('seq_type')){seq_type<-"dna"}

  if(!exists('show_pos')){
    show_pos<-NULL
  }

  #custom options
  tmp<-data.frame(t(bind_rows(as.character(data))),stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column(var = "idvar")

  if(diff_only){
    diff<-apply(tmp,2,function(x){length(setdiff(unique(x),"n"))})
    tmp<-tmp[,which(diff>1)]
  }

  if(!is.null(show_pos)){
    idx_show<-which(gsub("X","",colnames(tmp)) %in% show_pos)
    if(length(idx_show)>0){
      tmp<-tmp[,idx_show]
    }
  }


  if(ncol(tmp)>20){
    warning("These are long sequences! The sequence logo plot generally doesn't look. I'll display it, but consider using an alignment instead.")
  }

  #create a frequency matrix for the sequence logo plot
  nuc_freq<-apply(tmp,2,function(x){
    x<-toupper((x))
    nuc_count<-c(sum(x == "A"),
        sum(x == "C"),
        sum(x == "T"),
        sum(x == "G"),
        sum(x == "N"))
    nuc_count<-nuc_count/length(x)
    return(nuc_count)
  })
  colnames(nuc_freq)<-gsub("X","",colnames(nuc_freq))
  rownames(nuc_freq)<-c("A","C","T","G","N")

  custom_color<- ggseqlogo::make_col_scheme(chars=c('A', 'T', 'C', 'G','N'),
                        cols=c("#1F77B4","#FF7F0E","#D62728","#2CA02C","#d3d3d3"))

  #if there are only N's apparantely
  #ggseqlogo isn't too happy.
  #so warn user and die gracfull
  if(sum(nuc_freq[5,]) == ncol(nuc_freq)){
    warning("All positions for all sequecnes are N. There is nothing to show.")
    return(NULL)
  }else{
    p<-ggseqlogo::ggseqlogo(nuc_freq,col_scheme = custom_color, seq_type = "dna")
    return(p)
  }
}



#' Rendering a genomic alignment
#'
#' @title render_alignment
#' @param ...
#'
#' @return
render_alignment<-function(...){

  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #special parameters for this method
  diff_only<-if(exists('diff_only')) FALSE else TRUE
  if(!exists('seq_type')){seq_type<-"dna"}

  if(!exists('show_pos')){
    show_pos<-NULL
  }

  #convert dna bin to tidy data frame
  tmp<-data.frame(t(bind_rows(as.character(data))),stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column(var = "idvar")

  n_seq<-nrow(tmp)

  #Showing only a sepcific subset of positions
  if(diff_only){
    diff<-apply(tmp,2,function(x){length(setdiff(unique(x),"n"))})
    tmp<-tmp[,which(diff>1)]
  }

  if(!is.null(show_pos)){
    idx_show<-which(gsub("X","",colnames(tmp)) %in% show_pos)
    if(length(idx_show)>0){
      tmp<-tmp[,idx_show]
    }
  }


  tmp<-tidyr::gather(tmp,key = "pos",value = "seqval",-idvar) %>%
    mutate(pos = as.numeric(gsub("X","",pos))) %>%
    mutate(seqval = toupper(seqval))

  if(seq_type == "dna"){
    tmp$seqval<-factor(tmp$seqval,levels=c("A","C","T","G","N"))
  }

  #preparing the aes_string
  aes_val<-aes(x = pos,y=idvar)

  #check if combo_axis_var as been passed, as this will
  #change how the visualization is rendered
  if(!is.null(spec_list$combo_axis_var)){
    #id_var<-spec_list$combo_axis_var$var
    pos_mod<-data.frame(idvar = spec_list$combo_axis_var$var_lab,
                        idpos = spec_list$combo_axis_var$var)
    tmp<-dplyr::left_join(tmp,pos_mod)
    y_limits<-spec_list$combo_axis_var$y_limits
    aes_val<-aes(x = pos,y=idpos)
  }

  #create msa plot in ggplot
  if(diff_only){
    p<-tmp %>%
      dplyr::mutate(pos = factor(pos,levels = sort(unique(tmp$pos)))) %>%
      ggplot(data =., aes_val)

  }else{
    p<-ggplot(tmp,aes_val)
  }

  gg_chart<-p+
    geom_tile(aes(fill = seqval),color="black")+
    scale_fill_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#ffffff"),name="nucleotide")+
    theme_bw()+
    theme(axis.text.y = element_blank())

  if(!is.null(spec_list$combo_axis_var)){
    gg_chart<-gg_chart+scale_y_continuous(limits=y_limits)
  }

  #getting rid of x an y labels for massive character vectors
  if(!is.na(x) && class(data[,x]) %in% c("character","factor")){
    if(length(unique(data[,x])) > 50){
      rm_x_labels<-TRUE
    }
  }

  if(!is.na(y) && class(data[,y]) %in% c("character","factor")){
    if(length(unique(data[,y])) > 50){
      rm_y_labels<-TRUE
    }
  }

  gg_chart<-common_stats_aesethetics(gg_chart,
                                     title=title,
                                     flip_coord = flip_coord,
                                     y_limits = y_limits,
                                     x_limits=x_limits,
                                     x_labels = x_labels,
                                     y_labels = y_labels,
                                     rm_x_labels = TRUE,
                                     rm_y_labels = rm_y_labels,
                                     shrink_plot_margin = shrink_plot_margin)


  return(gg_chart)
}
