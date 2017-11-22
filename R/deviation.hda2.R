deviation.hda2 <-
function(Dmat,Nc.Dmat,new.cnames="Dis"){
 
	
		alpha <- function(Da,Nc) {
			Da2 <- Da-Nc
			
		return(Da2)
			}
			
		# Da2 <- ifelse(N[,1]<=Da,0,Da-N[,1])
		# Da2 <- Da2+ ifelse(Da<=N[,2],0,Da-N[,2])
				# return(Da2)}
	# if(dim(as.matrix(Nmodel))[2]==1){
	# N <- cbind(Nmodel,Nmodel)
# }

# else{
# min <- apply(Nmodel,1,min)
# max <- apply(Nmodel,1,max)
# N <- cbind(min,max)
# N <- N[rownames(Dmat),]}
	mat <- alpha(as.matrix(Dmat[rownames(Nc.Dmat),]),as.matrix(Nc.Dmat));
	mat <- ifelse(as.matrix(Dmat[rownames(Nc.Dmat),])==0,0,mat)
colnames(mat) <- new.cnames
	return(mat)
}
