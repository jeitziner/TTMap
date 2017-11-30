meshRows_norm_hda <-
function(df1,df2) {
junk <- meshRows_hda(df1,df2);
Df1 <- junk[[1]];Df2 <- junk[[2]];
df.mat1 <- as.matrix(Df1[-(1:3)][-1,]);
Df1 <- mat2pcl(mat = df.mat1,tag = Df1[(1:3)]);
df.mat2 <- as.matrix(Df2[-(1:3)][-1,]);
Df2 <- mat2pcl(mat = df.mat2,tag = Df2[(1:3)]);
return(list(Df1,Df2))}
