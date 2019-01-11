#
load("R/sysdata.rda")
chart_specifications<-readxl::read_excel("/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/chart_requirements.xlsx")

tmp<-data.matrix(composite_matrix)
tmp<-tmp[,2:ncol(tmp)]

tmp[upper.tri(tmp)] = t(tmp)[upper.tri(tmp)]
rownames(tmp)<-colnames(tmp)

comp_matrix<-tmp

save(chart_specifications,composite_matrix,comp_matrix,file="R/sysdata.rda")
