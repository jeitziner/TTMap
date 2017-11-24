\name{ttmap_part1}
\alias{ttmap_part1}
\alias{meshRows.norm.hda}
\alias{meshRows.hda}
\title{
Calculates a corrected control group, discovers outliers in it.
}
\description{
ttmap_part1 function finds outliers in the control group and 
removes them
}
\usage{
ttmap_part1(normal.pcl, tumor.pcl, normalname, dataname, 
org.directory = "", A = 1, e = 0, meth = 0, P = 1.1, B = 0)
meshRows.norm.hda(df1, df2)
meshRows.hda(df1, df2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{normal.pcl}{
the control matrix with annotation as obtained by $CTRL from 
make_matrices }
  \item{tumor.pcl}{
the disease/test data matrix with annotation as obtained by $TEST 
from make_matrices} 
  \item{normalname}{
A name for the corrected control files}
  \item{dataname}{
the name of the project}
  \item{org.directory}{
where the outputs should be saved}
  \item{A}{
integer if A=0 then the difference to the median is calculated 
otherwise the difference to the mean.
}
  \item{e}{
integer giving how far to the median an outlier is at least}
  \item{meth}{
value or method that defines how to replace outliers, default is 
set to replace by the median
}
  \item{P}{
if more than P percent of features are outliers the feature is 
removed, by default all are kept}
  \item{B}{
Batch vector a vector for normal and test samples with a same 
number corresponding to a same batch}
  \item{df1}{
matrix 1}
 \item{df2}{
matrix 2}
}
\details{
ttmap_part1 calculates a corrected control group, discovers 
outliers in it.
meshRows.norm.hda will select only common features of df1 and df2 
and removes the tags
meshRows.hda will select only common features of df1 and df2 when 
df1 and df2 are matrices without tags
}
\value{
Several files are created
\item{paste(org.directory,normalname,".normMesh",sep = "")}{The 
normal matrix with only common features with the test matrix. This 
file is only created if the two have different rows}
\item{paste(org.directory,dataname,".normMesh",sep = "")}{The 
test matrix with only common features with the normal matrix. This 
file is only created if the two have different rows.}
\item{mean_vs_variance.pdf}{A pdf showing a plot of the mean (X 
axis) against the variances (Y axis) of each feature}
\item{mean_vs_variance_after_correction.pdf}{A pdf showing a plot 
of the mean (X axis) against the variances (Y axis) of each feature
after correction of the control group}
\item{na_numbers_per_row.txt}{number of outliers per row}
\item{na_numbers_per_col.txt}{number of outliers per column}
\item{mattt.txt}{corrected normal matrix}
And values of ttmap_part1
\item{e}{Selected criteria for what is an outlier}
\item{tag.pcl}{Annotation of features, ID of features and weight}
\item{Normal.mat}{The control matrix without annotation and only 
with the common rows with Disease.mat}
\item{Disease.mat}{The test/disease matrix without annotation and 
only with the common rows with Disease.mat}
\item{flat.Nmat}{A list $mat being the corrected control matrix $m 
a record of the different numbers of removed genes per sample}
\item{record}{numbers recording the number of columns in 
Disease.mat and Normal.mat}
\item{B}{The batch vector B introduced in the begining}
\item{U1}{The different batches in Normal.mat}
\item{U2}{The different batches in Disease.mat}
}

\author{
Rachel Jeitziner}

\seealso{
ttmap_part2,ttmap_part3,ttmap_part4}
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
rownames(dis)<-c(paste("norm",
c(1:(D0-C0)),sep=""),paste("diff",c(1:C0),sep=""))	
junk <- TTMap::make_matrices(cbind(norm1,dis),
col_ctrl = colnames(norm1),col_test = colnames(dis),
NAME=rownames(norm1),CLID=rownames(norm1))
TTMAP_part1prime <-TTMap::ttmap_part1(normal.pcl = junk$CTRL,
tumor.pcl = junk$TEST, normalname = "Hi", dataname = "Hello",
 org.directory = getwd(),e=E,P=Pw,B=Bw);}