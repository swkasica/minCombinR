# Environment that holds various global variables and settings for gevitR,
# such as the differen chart types and the require aesthetics.
# It is not exported and should not be directly manipulated by other packages.

gevitr_env<-new.env(parent = emptyenv())


# a summary of all supported chart types
.all_chart_types<-c(unique(chart_specifications$chart_type),"timeline","alignment") #obtain specif table from sysdata.R
gevitr_env$all_chart_types <- .all_chart_types

#keep track of chart types that are no spatially alignable
.not_spatially_alignable <- c("pie", "venn", "node_link", "image")
gevitr_env$not_spatially_alignable <-.not_spatially_alignable

#keep track of charts that have precedence in composite combinations
.lead_chart_types <- c(
  "timeline", "histogram", "pdf", "chord",
  "stream", "geographic_map", "choropleth", "interior_map",
  "dendrogram", "phylogenetic_tree","clonal tree", "density_plot"
)

gevitr_env$lead_chart_types<-.lead_chart_types


# --- These are external and exposed to the user ---
.marks<-c("point","line","area","text")
.pos_channels<-c("x","y","lat","long","date","start_date","end_date")
.ret_channels<-c("color","size","shape","alpha")

.mark_specific_channels<-sapply(.marks,function(x,chan){
  sapply(chan,function(y,x){
    paste(x,y,sep = "_")
  },x = x)
},chan = .ret_channels) %>%
  unlist() %>%
  unname() %>%
  as.vector()

.param_defaults<-list()

for(item in c("metadata",.pos_channels,.ret_channels,.mark_specific_channels)){
  .param_defaults[[item]]<-NA
}


#package defaults
.style_external_defaults<-list(
  #style defaults
  title = NA,
  layout = NA,
  facet_by =NA,
  link_by = NA, #instead of common_var,
  combo_type = "simple",
  align_dir = "horizontal",
  group=NA
)

#all the param_defaults are exposed to the user
.plot_param_defaults<-append(.param_defaults,.style_external_defaults)


# --- These are internal, not exposed to the user  ---
.plot_internal_defaults<-list(
  #Style defaults
  flip_coord=FALSE,
  rm_y_labels=FALSE, rm_x_labels=FALSE,
  x_limits=NA, y_limits=NA,
  x_labels=NA, y_labels = NA,
  shrink_plot_margin = FALSE,
  #combination defaults
  order=NA,
  #Many types linked
  color_palette = NA,
  colour_scale=NA,
  default_colour_var=NA #remove this later
)

# #storing some parameter defaults so that they don't have to be constantly repeated
# #this is everything in excess of chart type and data, which must always be specificed
# .plot_param_defaults<-list(
#     #general plotting
#     x=NA, y=NA, z=NA, stack_by=NA, fill=NA, group=NA, title=NA,
#     path=NA, category=NA, comparisons=NA,
#     scale_y_cont=NA,
#     #For non-tablular data,
#     metadata = NA,
#     #For bar
#     proportional = FALSE, reference_vector=NA, reference_var=NA,
#     #For bar and phylo
#     layout="default",
#     #For stream
#     key=NA, value=NA, date=NA,
#     #For timeline
#     start=NA, end=NA, names=NA, events=NA,
#     #For table
#     rownames=NA,
#     #For geographic map
#     lat_var=NA, long_var=NA,
#     #For dendro reencodings,
#     labels=NA,
#     labels_col_var=NA, labels_col_values=NA, labels_col_palette=NA, labels_size=NA,
#     leaf_col_var=NA, leaf_col_palette=NA,
#     tip_var=NULL, cluster_vars=NULL,
#     #For node link
#     directed = FALSE,
#     #For node link reencodings
#     edge_col_var = NA, edge_col_palette = NA,
#     node_col_var = NA, node_col_palette = NA,
#     #FOR COMPOSITE (only implemented for a few chart types)
#     flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
#     #TODO: change this so it is split into reencode_var, mark_type, colour_scale and reencode_channel
#     #FOR DEFAULT REENCODINGS of mark type = 'colour'
#     default_colour_var=NA, colour_scale=NA,
#     #FOR ADDED MARKS:
#     add_mark=NA,
#     #FOR MANY TYPES LINKED
#     colour_mark_type=NA,
#     colour = NA, #generic colour placeholder for many types linked
#     #FOR SMALL MULTIPLES and composite
#     x_limits=NA, y_limits=NA,
#     x_labels=NA, y_labels = NA,
#     #Composite generak
#     alignment=NA,common_var=NA,order=NA,
#     #For composite with a tree
#     tree_dat=NA,
#     combo_type="simple",
#     shrink_plot_margin = FALSE
# )

gevitr_env$plot_param_defaults<-.plot_param_defaults
gevitr_env$plot_internal_defaults<-.plot_internal_defaults
