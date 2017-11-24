\name{make_matrices}
\alias{make_matrices}
\title{
Prepares the matrices for ttmap_part1
}
\description{
make_matrices generates the control and the test matrice in the 
right format
}
\usage{
make_matrices(mat,col_ctrl,col_test,NAME,CLID,GWEIGHT = 
rep(1,dim(mat)[1]),EWEIGHT=0 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mat}{the matrix of gene expressions}
\item{col_ctrl}{the columns in the matrix "mat" of the control 
samples}
\item{col_test}{the columns in the matrix "mat" of the test samples}
\item{NAME}{Name of genes,or annotation, e.g. WNT4}
\item{CLID}{Identities of genes,e.g. ENSMUSG00000000001}
\item{GWEIGHT}{the weight for each gene}
\item{EWEIGHT}{the weight for each experiment}
}
\details{
make_matrices generates the test matrix and the control matrix in the format accepted by ttmap_part1
}
\value{
\item{junk}{A list containing $CTRL and $TEST the matrices to impute in ttmap_part1}
}

\author{
Rachel Jeitziner}

\seealso{
ttmap_part1,ttmap_part2,ttmap_part3,ttmap_part4}
\examples{
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

	junk <- TTMap::make_matrices(cbind(norm1,dis),col_ctrl = colnames(norm1),col_test = colnames(dis),NAME=rownames(norm1),CLID=rownames(norm1))
	
}