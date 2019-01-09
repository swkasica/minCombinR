#
load("R/sysdata.rda")
chart_specifications<-readxl::read_excel("/Users/acrisan/Dropbox/Papers/gevitR/scratch/ana/chart_requirements.xlsx")
save(chart_specifications,composite_matrix,file="R/sysdata.rda")
