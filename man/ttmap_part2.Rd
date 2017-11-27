\name{ttmap_part2}
\alias{ttmap_part2}
\alias{normal.hda2}
\alias{deviation.hda2}
\title{
Calculation of deviation components
}
\description{
ttmap-part2 function calculates the deviation components using 
normal.hda2 which calculates the normal component of the test 
sample and deviation.hda2 which calculates the deviation component.
}
\usage{
ttmap_part2(x, k = dim(x$Normal.mat)[2], dataname, 
normalname,Org.directory=getwd())
normal.hda2(Dmat, Nmodel, new.cnames = "Norm")
deviation.hda2(Dmat, Nc.Dmat, new.cnames = "Dis")
}
\arguments{
\item{x}{
output object given back by ttmap_part1, list}
\item{k}{
A factor if not all the lines in the control group should be kept}
\item{dataname}{
the name of the project}
\item{normalname}{
A name for the corrected control files}
\item{Org.directory}{
where the outputs should be saved}
\item{Dmat}{
Test samples in a matrix}
\item{Nmodel}{
Normal samples in a matrix}
\item{Nc.Dmat}{
Output of normal.hda2}
\item{new.cnames}{
New names to give to the matrix created}
}
\details{
The function performs the second part of Hyperrectangle deviation 
assessement (HDA)
}
\value{
Outputs
\item{Tdis.pcl}{The matrix of the deviation components for each 
test sample}
\item{Tnorm.pcl}{The matrix of the normal components for each test 
sample}
\item{NormalModel.pcl}{The normal model used}
Values
\item{Dc.Dmat}{the deviation component matrix composed of the 
deviation components of all the samples in the test group}
\item{m}{the values of the filter function per sample in the test 
group}
}
\author{
Rachel Jeitziner}
\seealso{
ttmap_part1,ttmap_part3,ttmap_part4}
\examples{
##a full example can be found in ttmap_part4
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
}