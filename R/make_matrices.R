make_matrices <- function(mat,col_ctrl,col_test,
NAME,CLID,GWEIGHT = rep(1,dim(mat)[1]),EWEIGHT=0 ){
junk <- list()
junk$CTRL <- as.data.frame(mat[,col_ctrl])
junk$CTRL <- cbind(CLID,NAME,GWEIGHT,junk$CTRL)
junk$CTRL <- as.data.frame(junk$CTRL)
if(length(EWEIGHT)==0){
EWEIGHT<- rep(1,dim(mat[,col_ctrl])[2]+3)}
junk$CTRL <- rbind(EWEIGHT, junk$CTRL)
rownames(junk$CTRL)[1] <- "EWEIGHT"
junk$CTRL <- as.data.frame(junk$CTRL)
junk$CTRL[,1] <- as.character(junk$CTRL[,1])
junk$CTRL[1,1] <- "EWEIGHT"
colnames(junk$CTRL)[1:2] <- c("CLID","NAME")
junk$TEST <- as.data.frame(mat[,col_test])
junk$TEST <- cbind(CLID,NAME,GWEIGHT,junk$TEST)
junk$TEST <- as.data.frame(junk$TEST)
if(length(EWEIGHT)==0){
	EWEIGHT<- rep(1,dim(mat[,col_test])[2]+3)}
junk$TEST <- rbind(EWEIGHT, junk$TEST)
rownames(junk$TEST)[1] <- "EWEIGHT"
junk$TEST <- as.data.frame(junk$TEST)
junk$TEST[,1] <- as.character(junk$TEST[,1])
junk$TEST[1,1] <- "EWEIGHT"
colnames(junk$TEST)[1:2] <- c("CLID","NAME")
return(junk)
}