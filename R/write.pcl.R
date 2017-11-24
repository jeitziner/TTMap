write.pcl <-
function(df,dataname,fileaddress="") {
dir.address <- paste(fileaddress,dataname,".pcl",sep="");
X <- write.table(df,file=dir.address,
append=FALSE,quote=FALSE,sep="\t",eol="\n",na="",
row.names=FALSE,col.names=TRUE);
return(X)}