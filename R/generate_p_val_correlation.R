generate_p_val_correlation <-
function(ttmap_part2,select){
dd <- as.dist((1 - cor(ttmap_part2$Dc.Dmat[select,]))/2)
dd <- as.matrix(dd)
for(i in 1:dim(dd)[1]){
for(j in 1:dim(dd)[2]){
dd[i,j]<-cor.test(ttmap_part2$Dc.Dmat[select,i],ttmap_part2$Dc.Dmat[select,j],alternative="greater")$p.value}}
return(dd)	
}