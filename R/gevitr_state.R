# Environment that holds various global variables and settings for gevitR,
# such as the differen chart types and the require aesthetics.
# It is not exported and should not be directly manipulated by other packages.

gevitr_env<-new.env(parent = emptyenv())


# a summary of all supported chart types
.all_chart_types<-c(unique(chart_specifications$chart_type),"clonal tree") #obtain specif table from sysdata.R
gevitr_env$all_chart_types <- .all_chart_types

#keep track of chart types that are no spatially alignable
.not_spatially_alignable <- c("pie", "venn", "node_link", "image")
gevitr_env$not_spatially_alignable <-.not_spatially_alignable

#keep track of charts that have precedence in composite combinations
.master_chart_types <- c(
  "timeline", "histogram", "pdf", "chord",
  "stream", "geographic_map", "choropleth", "interior_map",
  "dendrogram", "phylogenetic_tree","clonal tree", "density_plot"
)

gevitr_env$master_chart_types<-.master_chart_types


#storing some parameter defaults so that they don't have to be constantly repeated
#this is everything in excess of chart type and data, which must always be specificed
.plot_param_defaults<-list(
    #general plotting
    x=NA, y=NA, z=NA, stack_by=NA, fill=NA, group=NA, title=NA,
    path=NA, category=NA, comparisons=NA,
    scale_y_cont=NA,
    #For non-tablular data,
    metadata = NA,
    #For bar
    proportional = FALSE, reference_vector=NA, reference_var=NA,
    #For bar and phylo
    layout="default",
    #For stream
    key=NA, value=NA, date=NA,
    #For timeline
    start=NA, end=NA, names=NA, events=NA,
    #For table
    rownames=NA,
    #For geographic map
    lat_var=NA, long_var=NA,
    #For dendro reencodings,
    labels=NA,
    labels_col_var=NA, labels_col_values=NA, labels_col_palette=NA, labels_size=NA,
    leaf_col_var=NA, leaf_col_palette=NA,
    tip_var=NULL, cluster_vars=NULL,
    #For node link
    directed = FALSE,
    #For node link reencodings
    edge_col_var = NA, edge_col_palette = NA,
    node_col_var = NA, node_col_palette = NA,
    #FOR COMPOSITE (only implemented for a few chart types)
    flip_coord=FALSE, rm_y_labels=FALSE, rm_x_labels=FALSE,
    #TODO: change this so it is split into reencode_var, mark_type, colour_scale and reencode_channel
    #FOR DEFAULT REENCODINGS of mark type = 'colour'
    default_colour_var=NA, colour_scale=NA,
    #FOR ADDED MARKS:
    add_mark=NA,
    #FOR MANY TYPES LINKED
    colour_mark_type=NA,
    #FOR SMALL MULTIPLES and composite
    x_limits=NA, y_limits=NA,
    x_labels=NA, y_labels = NA,
    #Composite generak
    alignment=NA,common_var=NA,order=NA,
    #For composite with a tree
    tree_dat=NA,
    combo_type="simple"
)

gevitr_env$plot_param_defaults<-.plot_param_defaults
