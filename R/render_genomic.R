render_seqlogo <- function(...) {
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #custom options

}

render_alignment<-function(...){
  spec_list<-list(...)

  #put the specification variables in a location environment
  #so they can be accessed without using a list
  list2env(spec_list,env=environment())

  #special parameters for this method
  diff_only<-if(is.null(diff_only)) FALSE else TRUE
  data_type<-if(is.null(data_type)) "dna" else data_type

  #convert dna bin to tidy data frame
  tmp<-data.frame(t(bind_rows(as.character(data))),stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column(var = "idvar")

  n_seq<-nrow(tmp)

  if(diff_only){
    diff<-apply(tmp,2,function(x){length(setdiff(unique(x),"n"))})
    tmp<-tmp[,which(diff>1)]
  }

  tmp<-tidyr::gather(tmp,key = "pos",value = "seqval",-idvar) %>%
    mutate(pos = as.numeric(gsub("X","",pos))) %>%
    mutate(seqval = toupper(seqval))

  if(data_type == "dna"){
    tmp$seqval<-factor(tmp$seqval,levels=c("A","C","T","G","N"))
  }

  #create msa plot in ggplot
  if(diff_only){
    p<-tmp %>%
      dplyr::mutate(pos = factor(pos,levels = sort(unique(tmp$pos)))) %>%
      ggplot(data =., aes(x=pos,y=idvar))

  }else{
    p<-ggplot(tmp,aes(x = pos,y = idvar))
  }

  p<-p+
    geom_tile(aes(fill = seqval),color="black")+
    scale_fill_manual(values=c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#ffffff"),name="nucleotide")+
    theme_bw()+
    theme(axis.text.y = element_blank())

  return(p)
}
