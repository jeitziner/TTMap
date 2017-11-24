ttmap_part4 <-
function(ttmap_part3,ttmap_part2,ttmap_part1,c,n=2,a=0,filename="TEST2",annot=ttmap_part1$tag.pcl,col = "NAME",path=getwd(),Relaxed = 1) {
setwd(path)
dir.create(paste(getwd(),"all",sep="/"))
dir.create(paste(getwd(),"mid1",sep="/"))
dir.create(paste(getwd(),"mid2",sep="/"))
dir.create(paste(getwd(),"high",sep="/"))
dir.create(paste(getwd(),"low",sep="/"))
if(Relaxed!=1) {
ttmap_part4_inter <- ttmap_part4_inter2
}
junk_low <- list(x=0)
for(i in 1: length(ttmap_part3$low_map)){
A <-ttmap_part4_inter(ttmap_part3$low_map[[i]],ttmap_part2,alpha=a)
A<- as.matrix(A)	
A <-cbind(as.character(annot[rownames(A),col]),A)
write.table(A,file=paste(paste("low",filename,sep="/"),
paste(c[ttmap_part3$low_map[[i]][1],n],"txt",sep="."),sep="_"),
quote=FALSE,sep="\t",row.names=TRUE,col.names=NA)
}
for(i in 1: length(ttmap_part3$mid1_map)){
A <-ttmap_part4_inter(ttmap_part3$mid1_map[[i]],ttmap_part2,alpha=a)
A<- as.matrix(A)
A <- as.data.frame(A)
A <-cbind(annot[rownames(A),col],A)
write.table(A,file=paste(paste("mid1",filename,sep="/"),
paste(c[ttmap_part3$mid1_map[[i]][1],n],"txt",sep="."),sep="_"),
quote=FALSE,sep="\t",row.names=TRUE,col.names=NA)
}
for(i in 1: length(ttmap_part3$mid2_map)){
A <-ttmap_part4_inter(ttmap_part3$mid2_map[[i]],ttmap_part2,alpha=a)
A<- as.matrix(A)
A <- as.data.frame(A)
A <-cbind(annot[rownames(A),col],A)
write.table(A,file=paste(paste("mid2",filename,sep="/"),
paste(c[ttmap_part3$mid2_map[[i]][1],n],"txt",sep="."),sep="_"),quote=FALSE,sep="\t",row.names=TRUE,
col.names=NA)
}	
for(i in 1: length(ttmap_part3$high_map)){
A <-ttmap_part4_inter(ttmap_part3$high_map[[i]],ttmap_part2,alpha=a)
A<- as.matrix(A)
A <- as.data.frame(A)
A <-cbind(annot[rownames(A),col],A)
write.table(A,file=paste(paste("high",filename,sep="/"),
paste(c[ttmap_part3$high_map[[i]][1],n],"txt",sep="."),sep="_"),quote=FALSE,
sep="\t",row.names=TRUE,col.names=NA)
}

for(i in 1: length(ttmap_part3$all)){
A <-ttmap_part4_inter(ttmap_part3$all[[i]],ttmap_part2,alpha=a)
A<- as.matrix(A)
A <- as.data.frame(A)
if(dim(A)[2]==1){ colnames(A)<-ttmap_part3$all[[i]]}
A <-cbind(annot[rownames(A),col],A)
write.table(A,file=paste(paste("all",filename,sep="/"),
paste(c[ttmap_part3$all[[i]][1],n],"txt",sep="."),sep="_"),
quote=FALSE,sep="\t",row.names=TRUE,col.names=NA)
}
}
