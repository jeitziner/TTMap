generate_euclidean <-
function(ttmap_part1_hda,select){
dd <- dist(t(ttmap_part1_hda$Dc.Dmat[select,]),
method="euclidean")
dd <- as.matrix(dd)
if(sum(is.na(dd))!=0){
for(i in 1:dim(dd)[1]){
for(j in 1:dim(dd)[2]){
if(is.na(dd[i,j])==TRUE) dd[i,j]<-0
}}
}
return(dd)
}
