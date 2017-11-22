calcul_e<-
function(dd5,pvalcutoff=0.95,tt1,alpha = 1,S=colnames(tt1$Normal.mat)){
	
	  w <- apply(tt1$Normal.mat[,colnames(tt1$Normal.mat)%in%S],1,mean)
  w2 <- apply(tt1$Normal.mat[,colnames(tt1$Normal.mat)%in%S],1,sd)
 #w <- apply(tt1$flat.Nmat$mat,1,mean)
# w2 <- apply(tt1$flat.Nmat$mat,1,sd)
# print(mean(w2))
 #w<- apply(tt1$flat.Nmat$mat,1,mean)
 #w2 <- apply(tt1$flat.Nmat$mat,1,sd)
m_f <- apply(tt1$flat.Nmat$mat[rownames(tt1$Normal.mat),colnames(tt1$flat.Nmat$mat)%in%S],1,min)
m2_f <- apply(tt1$flat.Nmat$mat[rownames(tt1$Normal.mat),colnames(tt1$flat.Nmat$mat)%in%S],1,max)

##Calculate p3
# p2 <- rep(0,length(m_f))
# p2p <- rep(0,length(m_f))
print(length(m_f))
p2<-lapply(1:length(m_f), function(i){
pnorm(m_f[i],w[i],w2[i],lower.tail=TRUE)
})

p2 <- unlist(p2)


p2p<-lapply(1:length(m_f), function(i){pnorm(m_f[i]-alpha,w[i],w2[i],lower.tail=TRUE)})
p2p <- unlist(p2p)


# # p3 <- rep(0,length(m_f))
# p4 <- rep(0,length(m_f))
#p3p <- rep(0,length(m_f))
#p4p <- rep(0,length(m_f))

#for(i in 1:length(m_f)){
p3<-lapply(1:length(m_f), function(i){ pnorm(m2_f[i],w[i],w2[i],lower.tail=TRUE)})
p4<-lapply(1:length(m_f), function(i){ pnorm(m_f[i],w[i],w2[i],lower.tail=TRUE)})
p3 <- unlist(p3)
p4 <- unlist(p4)
p3<- p3-p4


# p1 <- rep(0,length(m_f))
# p1p <- rep(0,length(m_f))
# for(i in 1:length(m_f)){
p1<-lapply(1:length(m_f), function(i){ pnorm(m2_f[i],w[i],w2[i],lower.tail=FALSE)})
p1p<-lapply(1:length(m_f), function(i){pnorm(m2_f[i]+alpha,w[i],w2[i],lower.tail=FALSE)})

p1 <- unlist(p1)
p1p <- unlist(p1p)
# head(p3)
# a <- 2*((p3*p1)+(p2*p1)+(p3*p2))
# a2 <- 2*(((p3)*p1p)+(p2p*p1p)+((p3)*p2p)) 
#-p1p*p2p
a3 <- 2*(((p3+p1)*p2p) +((p3+p2)*p1p)-(p1p*p2p))
#a3 <- 2*(p3*(p2p+p1p))

sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	ele <- as.vector(dd5[upper.tri(dd5)])[i]
	#s <- ppnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 #s<-ppois(ele*length(m_f),sum(a3))
	#print(paste(s,ele,sep=":"))
	ifelse(s < pvalcutoff, ele,0)

	})

	#print(sl)
	s <- max(unlist(sl))
	if(s<=0){
		sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	ele <- as.vector(dd5[upper.tri(dd5)])[i]
	#s <- pnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 #s<-ppois(ele*length(m_f),sum(a3))
	#print(paste(s,ele,sep=":"))
	ifelse(s < 1-.Machine$double.neg.eps, ele,0)

	})

	#print(sl)
	s <- max(unlist(sl))
		
	}
	return(s)
}

	# if(s >0){
		# sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	# ele <- as.vector(dd5[upper.tri(dd5)])[i]
	# sp <- pnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 # #s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 # #s<-ppois(ele*length(m_f),sum(a3))
	# print(paste(sp,ele,sep=":"))
	# ifelse(sp <=0.001, ele,0)

	# })
# sl <- max(unlist(sl))

	# s<-ifelse(sl<s,sl,s)
	# print(paste("sp",s))

		
		
	# }
	
	# sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	# ele <- as.vector(dd5[upper.tri(dd5)])[i]
	# s2 <- pnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 # #s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 # #s<-ppois(ele*length(m_f),sum(a3))
	# print(paste(s2,ele,sep=":"))
	# ifelse(s2 >=0.95, ele,0)

	# })
# sl <- unlist(sl)
# sl[sl==0]<-NA
	# #print(sl)
	# s2 <- min(sl,na.rm=TRUE)
	# s2<-ifelse(is.na(s2),0,s2)
	# print(paste("s2",s2))
	 
	# if(s2>s){sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	# ele <- as.vector(dd5[upper.tri(dd5)])[i]
	# s3 <- pnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 # #s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 # #s<-ppois(ele*length(m_f),sum(a3))
	# print(paste(s3,ele,sep=":"))
	# ifelse(s3 >= 0.999, ele,0)

	# })
	# sl <- unlist(sl)
# sl[sl==0]<-NA
	# #print(sl)
	# s3 <- min(sl,na.rm=TRUE)
	# s3 <- ifelse(is.na(s3),0,s3)
# print(paste("s3",s3))
	
	# if(s3>s2) {
		
		
		# sl <- lapply(1:length(as.vector(dd5[upper.tri(dd5)])),function(i){ 
	
	# ele <- as.vector(dd5[upper.tri(dd5)])[i]
	# s4 <- pnorm(ele*length(m_f), mean = sum(a3,na.rm=TRUE), sd = sqrt(sum(a3*(1-a3),na.rm=TRUE)), lower.tail = TRUE)
	 # #s<-ppois(ele*length(m_f),sum(a3), lower.tail = TRUE, log.p = FALSE)
	
	 # #s<-ppois(ele*length(m_f),sum(a3))
	# print(paste(s4,ele,sep=":"))
	# ifelse(s4 <= 1-.Machine$double.neg.eps, ele,0)

	# })
	
	# sl <- unlist(sl)
# sl[sl==0]<-NA
	# #print(sl)
	# s4 <- max(sl,na.rm=TRUE)
	# s4 <- ifelse(is.na(s4),0,s4)
	# s4 <- ifelse(s4==Inf,0,s4)
	# s2 <- ifelse(s4>s3,s4,s3)
	# }
	


		
		
		
		
	# }
 
 # if(s==s2) {print("Yeiii")}
 # else{if(s==0){s <-s2}else{if(s2<max(dd5)){s <- s2}}}
# return(s)

# }
