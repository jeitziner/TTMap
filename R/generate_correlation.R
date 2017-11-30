generate_correlation <-
function(ttmap_part1_hda,select){
dd <- as.dist((1 - cor(ttmap_part1_hda$Dc.Dmat[select,]))/2)
dd <- as.matrix(dd)
if(sum(is.na(dd))!=0){
for(i in 1:dim(dd)[1]){
for(j in 1:dim(dd)[2]){
if(is.na(dd[i,j])==TRUE) dd[i,j]<-0
}}
}
return(dd)
}
