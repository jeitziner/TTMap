ttmap_part2 <- function(x,k=dim(x$Normal.mat)[2],dataname,normalname,Org.directory=getwd())
{
u <- meshRows.hda(x$tag.pcl, x$flat.Nmat$mat)
tag.pcl <- u[[1]]
tag.pcl <- as.data.frame(rbind(1,tag.pcl)); rownames(tag.pcl)[[1]] <- "EWEIGHT"

s<- meshRows.hda(x$Normal.mat,x$flat.Nmat$mat)
t <- meshRows.hda(s[[1]], x$Disease.mat)
Normal.mat <- s[[1]];
Disease.mat <- t[[2]];
flat.Nmat <- s[[2]];
rm(s,t,u)
B <- x$B
U1<-x$U1
U2<-x$U2
rm(x);
if(k == dim(Normal.mat)[2]){
Normal.model <- flat.Nmat
}
else{
Normal.model <- pca.hda(mat = flat.Nmat,j = k); 
print("K!=dim..")}
Nc.Dmat<- lapply(1:length(unique(B)),function(i){
normal.hda2(Dmat = Disease.mat[,U2[[i]]],Nmodel = Normal.model[,U1[[i]]],new.cnames=paste(U2[[i]],"Norm",sep=".")); 
})
###
 #Dc.Dmat <- do.call(cbind,Dc.Dmat)
Nc.Dmat <- do.call(cbind,Nc.Dmat)
rownames(Nc.Dmat)<-rownames(Normal.mat)
Dc.Dmat<-
deviation.hda2(Dmat = Disease.mat,Nc.Dmat,
new.cnames=paste(unlist(U2),"Dis",sep="."));

rownames(Dc.Dmat)<-rownames(Normal.mat)
####  store all these: 
setwd(Org.directory)
Dc.Dpcl <- mat2pcl(mat = Dc.Dmat,tag = tag.pcl);
write.pcl(Dc.Dpcl,paste(dataname,"Tdis",sep = "."));               # <-- download & retain
Nc.Dpcl <- mat2pcl(mat = Nc.Dmat,tag = tag.pcl);
write.pcl(Nc.Dpcl,paste(dataname,"Tnorm",sep = "."));              # <-- download & remove
Normal.model <- mat2pcl(mat = Normal.model,tag = tag.pcl);
write.pcl(Normal.model,paste(normalname,"NormalModel",sep = "."));  # <-- download & remove
sumabs <- function(vec){y<- abs(vec); z<-sum(y);return(z)}
m <- apply(Dc.Dpcl[,-(1:3)],2,sumabs)
junk <- list(Dc.Dmat = Dc.Dmat,m=m);
return(junk)
};