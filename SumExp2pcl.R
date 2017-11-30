SumExp2pcl <- function(nki,col_ctrl,col_test,
NAME,CLID,GWEIGHT = rep(1,dim(nki)[1]),EWEIGHT=0){
nki <- as(nki,"RangedSummarizedExperiment")
SummarizedExperiment::assays(nki) <- 
S4Vectors::endoapply(SummarizedExperiment::assays(nki), asinh)
f <- (SummarizedExperiment::assay(nki))
# f <- f[,colnames(nki)]
f <- lapply(1:dim(f)[1],function(x){ w<- unlist(f[x,]); 
w[is.na(w)==TRUE]<- median(w,na.rm=TRUE);return(w)})
f <- matrix(unlist(f),ncol=length(colnames(nki)))
rownames(f)<-rownames(nki)
colnames(f)<-colnames(nki)
de <- make_matrices(f,col_ctrl,col_test,
NAME,CLID,GWEIGHT = rep(1,dim(nki)[1]),EWEIGHT=0)
}
