\name{ttmap_part4}
\alias{ttmap_part4}
\alias{ttmap_part4_inter2}
\alias{ttmap_part4_inter}
\title{
Gives a list of associated genes per cluster
}
\description{
ttmap_part4 function
}
\usage{
ttmap_part4(ttmap_part3, ttmap_part2, ttmap_part1, c, n = 2, a = 0, 
filename = "TEST2", annot = ttmap_part1$tag.pcl, 
col = "NAME", path = getwd(), Relaxed = 1)
ttmap_part4_inter2(q, ttmap_part2, alpha = 0)
ttmap_part4_inter(q, ttmap_part2, alpha = 0)
}
\arguments{
\item{ttmap_part3}{
output of ttmap_part3}
\item{ttmap_part2}{
output of ttmap_part2}
\item{ttmap_part1}{
output of ttmap_part1}
\item{c}{annotation file of the samples
}
\item{n}{
column to give the name to the cluster}
\item{a}{cutoff to be considered different than noise
}
\item{filename}{
Name of the files}
\item{annot}{
annotation file}
\item{col}{
which column should be considered to annotate the features}
\item{path}{
where to put the output files}
\item{Relaxed}{
If Relaxed then one allows sample to be as the control and for all 
the others in one cluster to be going in the same direction (more 
than alpha) otherwise all the features must be deviating to be 
considered a significant feature}
\item{q}{
The sample in one cluster}
\item{alpha}{
cutoff to be considered different than noise inherited by a
}
}
\details{
Is giving per cluster the features that vary in the same direction
}
\value{
generates a file per cluster of significant features with an 
annotation
}
\author{
Rachel Jeitziner}
\examples{
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
rownames(dis)<-c(paste("norm",c(1:(D0-C0)),sep=""),
paste("diff",c(1:C0),sep=""))
junk <- TTMap::make_matrices(cbind(norm1,dis),
col_ctrl = colnames(norm1),
col_test = colnames(dis),NAME=rownames(norm1),CLID=rownames(norm1))
TTMAP_part1prime <-TTMap::ttmap_part1(normal.pcl = junk$CTRL,
tumor.pcl = junk$TEST, 
normalname = "Hi", dataname = "Hello", 
org.directory = getwd(),e=E,P=Pw,B=Bw);
Kprime <- Aa;
TTMAP_part2 <- TTMap::ttmap_part2(x = TTMAP_part1prime,
k = Kprime,dataname = "Hello", normalname = "Hi");
annot <- c(paste(colnames(junk$TEST[,-c(1:3)]),"Dis",sep="."),
paste(colnames(junk$CTRL[,-c(1:3)]),"Dis",sep="."))
annot <- cbind(annot,annot)
rownames(annot)<-annot[,1]
dd5_sgn_only <-TTMap:::generate_mismatch_distance(TTMAP_part2,
select=rownames(TTMAP_part2$Dc.Dmat),alpha = ALPHA)
de1 <- TTMap::ttmap_part3(TTMAP_part2,TTMAP_part2$m,
select=rownames(TTMAP_part2$Dc.Dmat),annot,
e= TTMap::calcul_e(dd5_sgn_only,0.95,TTMAP_part1prime,1), 
filename="TEST",n=1,dd=dd5_sgn_only)
TTMap:::ttmap_part4(de1, TTMAP_part2, TTMAP_part1prime, annot,
n = 2, a = 1, filename = "TEST2",
annot = TTMAP_part1prime$tag.pcl, col = "NAME", 
path = getwd(), Relaxed = 1)
}