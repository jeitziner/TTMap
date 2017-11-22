ttmap_part1 <-
function(normal.pcl ,tumor.pcl, normalname, dataname, org.directory = "" ,A=1,e=0,meth=0,P=1.1,B=0)
{
	METHO <- ifelse(A==0,0,1)
	if(length(B)==1){
		B<-c(rep(0,(dim(normal.pcl)[2]+dim(tumor.pcl)[2]-6)))
	}
	
	 names(B)<- c(colnames(normal.pcl)[-c(1,2,3)],colnames(tumor.pcl)[-c(1,2,3)])
	
   ####  Select genes so both pcl files have the same genes. Normalize columns 
   record <- list();
   record$ntumors.original <- ncol(tumor.pcl) - 3;
   record$ngenes.tumor.original <- nrow(tumor.pcl) - 1;
   record$nnormal.original <- ncol(normal.pcl) - 3;
   record$ngenes.normal.original <- nrow(normal.pcl) - 1;
  norm.tum.list <- meshRows.norm.hda(df1 = normal.pcl, df2 = tumor.pcl);
   Normal.pcl <- norm.tum.list[[1]];
   Disease.pcl <- norm.tum.list[[2]];
   
###just in case there is not the same lines in tumour and normal
if(nrow(tumor.pcl)!=nrow(normal.pcl)){
write.pcl(Normal.pcl,paste(org.directory,normalname,".normMesh",sep = ""));
	write.pcl(Disease.pcl,paste(org.directory,dataname,".normMesh",sep = ""));
	}
   record$ngenes.common <- nrow(Normal.pcl) - 1;
   rm(norm.tum.list);
   tag.pcl <- Disease.pcl[(1:3)];

U <- lapply(1: length(unique(B)),function(i){
	intersect(names(B[B[]==(i-1)]),colnames(Normal.pcl))
})
U2 <- lapply(1: length(unique(B)),function(i){
	intersect(names(B[B[]==(i-1)]),colnames(Disease.pcl))
})
mylist<-list()
  mylist <- lapply(1:length(unique(B)),function(i){
    
   	Normal.mat <- as.matrix(Normal.pcl[,-c(1:3)][-1,U[[i]]]);
   	rownames(Normal.mat)<-rownames(Normal.pcl[-1,])
   		
   	#E <- apply(Normal.mat,1,mean)
   	E<- unlist(lapply(1:dim(Normal.mat)[1],function(i){mean(Normal.mat[i,][Normal.mat[i,]>0])}))
   	E[is.na(E)==TRUE]<-0
	V<- unlist(lapply(1:dim(Normal.mat)[1],function(i){var(Normal.mat[i,][Normal.mat[i,]>0])}))
		V[is.na(V)==TRUE]<-0
	ymin <- min(V)
	ymax <- max(V)
if(all(is.na(V))==TRUE){
	print("Unique vector, no variance")
	if(is.na(max(Normal.mat))==TRUE || max(Normal.mat)== (-Inf) ){
		print(head(Normal.mat))
		e <- 10000000
	}
	else{
		
	e <- max(Normal.mat)+1}
} 
else{
pdf(paste(dataname,"mean_vs_variance.pdf",sep="_"),paper="a4",height=8,width=11)
   plot(E,V,main=paste("Mean/variance plot",paste("batch",i,sep=""),sep=""), xlab="Mean",ylab="Variance")
abline(h = quantile(V,0.90), col = "red")
legend("topleft", paste("Red line=",quantile(V,0.90)), bty="n")
   dev.off()
   if(e==0){
   	print(dim(Normal.mat)[2])
   	print(max(V))
   	e <- sqrt(quantile(V,0.90))/sqrt(dim(Normal.mat)[2])
   }
   }
   flat.Nmat <- FLAT.hda2(Normal.mat, el = e,metho = meth,p=P,method_mean_or_median = METHO);
   head(flat.Nmat$mat)
    	E<- unlist(lapply(1:dim(flat.Nmat$mat)[1],function(i){mean(flat.Nmat$mat[i,][flat.Nmat$mat[i,]>0])}))
   	E[is.na(E)==TRUE]<-0
	V<- unlist(lapply(1:dim(flat.Nmat$mat)[1],function(i){var(flat.Nmat$mat[i,][flat.Nmat$mat[i,]>0])}))
		V[is.na(V)==TRUE]<-0
pdf(paste(dataname,"mean_vs_variance_after_correction.pdf",sep="_"),paper="a4",height=8,width=11)
   plot(E,V,main=paste("Mean/variance plot",paste("batch",i,sep=""),sep=""), xlab="Mean",ylab="Variance",ylim=c(0,ymax))
abline(h = quantile(V,0.90), col = "red")
legend("topleft", paste("Red line=",quantile(V,0.90)), bty="n")
   dev.off()
 
   u <- cbind(flat.Nmat$na_numbers,Normal.pcl[rownames(flat.Nmat$na_numbers),"NAME"])
  
write.table(u,file=paste(paste(paste(org.directory,dataname,sep="/"),paste("batch",i-1,sep=""),sep=""),"na_numbers_per_row.txt",sep="_"),quote=FALSE,sep="\t",row.names=TRUE,col.names=TRUE)
    u <- as.matrix(flat.Nmat$m)
   write.table(u,file=paste(paste(paste(org.directory,dataname,sep="/"),paste("batch",i-1,sep=""),sep=""),"na_numbers_per_col.txt",sep="_"),quote=FALSE,sep="\t",row.names=TRUE,col.names=TRUE)
    return(flat.Nmat$mat)
    })
   	all.tbl <- table(unlist(lapply(mylist,rownames)))
sel <- names(all.tbl[all.tbl == length(mylist)])
flat <- do.call(cbind,lapply(mylist,function(x) x[sel,]))
print(flat[1,1])
 colnames(flat)<- colnames(Normal.pcl[,unlist(U)])
flat.Nmat <- list()
flat.Nmat$mat <- flat
if(length(unique(B))!=0){
	denom <- colnames(flat.Nmat$mat)
	flat.Nmat$mat <- t(apply(flat,1,as.numeric))
	colnames(flat.Nmat$mat) <- denom
}


junk <- list(e= e, tag.pcl = tag.pcl,Normal.mat = Normal.pcl[-1,-c(1:3)],Disease.mat = Disease.pcl[-1,-c(1:3)]
,flat.Nmat = flat.Nmat,record = record,B=B,U1=U,U2=U2);
return(junk)
}
