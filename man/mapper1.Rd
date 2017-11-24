\name{mapper1}
\alias{mapper1}
\alias{mat_d}
\alias{mat_d_col}
\alias{mat_1_0}
\title{
Matrix that will help build the Mapper}
\description{
Calculates the information to create the Mapper. The main function 
is Mapper1 which uses mat_1_0 mat_d and mat_d_col.}
\usage{
mapper1(mat, e = 0.1)
mat_1_0(mat, e)
mat_d(mat)
mat_d_col(mat)
}
\arguments{
\item{mat}{Matrix}
\item{e}{cutoff}
}
\details{
These functions are necessary to calculate the Mapper using single 
linkage algorithme and a two-tier cover.
}
\value{
\item{m}{Matrix} 
}
\author{
Rachel Jeitziner}
\seealso{
ttmap_part3}
\examples{
#example 
d <- matrix(c(0,0.1,0.2,0.1,0,0.15,0.2,0.15,0),nrow=3)
colnames(d)<-c("A","B","C")
rownames(d)<-c("A","B","C")
#Replaces everything lower than e by 0 and higher by 1
d2 <-TTMap:::mat_1_0(d,0.1)
d2 <- TTMap:::mat_d(d2)
d2 <- TTMap:::mat_d_col(d2)
#d2 is the same as d3 given by
d3 <- TTMap:::mapper1(d,0.1)
}
