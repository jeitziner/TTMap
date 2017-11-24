\name{calcul_e}
\alias{calcul_e}
\alias{calcul_e_single}
\title{
Calculation of the value of epsilon}
\description{
Calculation of the value of epsilon}
\usage{
calcul_e(dd5, pvalcutoff = 0.95, tt1, alpha = 1, S = 
colnames(tt1$Normal.mat))
calcul_e_single(dd5, pvalcutoff = 0.95, tt1, alpha = 1, S = 
colnames(tt1$Normal.mat))
}
\arguments{
\item{dd5}{
distance matrix as created by generate_mismatch_distance}
\item{pvalcutoff}{
cutoff of 0.05 percent (default) or less}
\item{tt1}{
output of ttmap_part1}
\item{alpha}{
a cutoff value for the FC between the group of control and the 
disease group}
\item{S}{
subset of columns to be considered}
}

\value{
\item{al}{number representing the cutoff to choose for the 
relatedness with dd5}
}

\author{
Rachel Jeitziner}

\seealso{
ttmap_part1,ttmap_part2,ttmap_part3,ttmap_part4,generate_mismatch_distance}
\examples{
## See full example ttmap_part4
##--
##--
Aa = 6
B1 = 3
B2=3
C0=100
D0 = 10000
a0 = 4
b0=0.1
a1=6
b1= 0.1
a2=2
b2=0.5
ALPHA=1
E=1
Pw=1.1
Bw=0
RA <- matrix(rep(0,Aa*D0),nrow=D0)
RB1<- matrix(rep(0,B1*D0),nrow=D0)
RB2 <- matrix(rep(0,B2*D0),nrow=D0)
RA<- lapply(1:(D0-C0),function(i) rnorm(Aa, 
mean = a0, sd = sqrt(b0)))
RA<-do.call(rbind,RA)
RB1<- lapply(1:(D0-C0),function(i) rnorm(B1, 
mean = a0, sd = sqrt(b0)))
RB1<-do.call(rbind,RB1)
RB2<- lapply(1:(D0-C0),function(i) rnorm(B2, 
mean = a0, sd = sqrt(b0)))
RB2<-do.call(rbind,RB2)
RA_c<- lapply(1:C0,function(i) rnorm(Aa, 
mean = a0, sd = sqrt(b0)))
RA_c<-do.call(rbind,RA_c)
RB1_c<- lapply(1:C0,function(i) rnorm(B1, 
mean = a1, sd = sqrt(b1)))
RB1_c<-do.call(rbind,RB1_c)
RB2_c<- lapply(1:C0,function(i) rnorm(B2, 
mean = a2, sd = sqrt(b2)))
RB2_c<-do.call(rbind,RB2_c)
norm1 <- rbind(RA,RA_c)
dis <- cbind(rbind(RB1,RB1_c),rbind(RB2,RB2_c))
colnames(norm1)<- paste("N",c(1:Aa),sep="")
rownames(norm1)<-c(paste("norm",c(1:(D0-C0)),sep=""),
paste("diff",c(1:C0),sep=""))
colnames(dis) <- c(paste("B1",c(1:B1),sep=""),
paste("B2",c(1:B2),sep=""))
rownames(dis)<-c(paste("norm",
c(1:(D0-C0)),sep=""),paste("diff",c(1:C0),sep=""))	
junk <- TTMap::make_matrices(cbind(norm1,dis),
col_ctrl = colnames(norm1),col_test = colnames(dis),
NAME=rownames(norm1),CLID=rownames(norm1))
TTMAP_part1prime <-TTMap::ttmap_part1(normal.pcl = junk$CTRL,
tumor.pcl = junk$TEST, normalname = "Hi", dataname = "Hello",
 org.directory = getwd(),e=E,P=Pw,B=Bw);
Kprime <- Aa;
TTMAP_part2 <- TTMap::ttmap_part2(x = TTMAP_part1prime,
k = Kprime,dataname = "Hello", normalname = "Hi");
annot <- c(paste(colnames(junk$TEST[,-c(1:3)]),"Dis",sep="."),
paste(colnames(junk$CTRL[,-c(1:3)]),"Dis",sep="."))
 annot <- cbind(annot,annot)
 rownames(annot)<-annot[,1]
dd5_sgn_only <-
TTMap:::generate_mismatch_distance(TTMAP_part2,
select=rownames(TTMAP_part2$Dc.Dmat),alpha = ALPHA)
 e <- TTMap:::calcul_e(dd5_sgn_only,0.95,TTMAP_part1prime,1)
}