\name{write_pcl}
\alias{write_pcl}
\alias{read_pcl}
\alias{mat2pcl}
\title{
Reading, writing and annotation files}
\description{
Reading (read_pcl), writing (write_pcl) 
files and annotating matrices (mat2pcl)}
\usage{
mat2pcl(mat, tag)
write_pcl(df, dataname, fileaddress = "")
read_pcl(filename, na.type = "", Nrows = -1, Comment.char = "", ...)
}\arguments{
\item{df}{
PCL object to be saved
}
\item{dataname}{
Name of the file
}
\item{fileaddress}{
Where to save the file
}
\item{filename}{
File name to be loaded on R
}
\item{na.type}{
feels the parameter na.strings of read.table}
\item{Nrows}{
Number of rows to be ignored (nrows of read.table)}
\item{Comment.char}{
comment.char of read.table}
\item{\dots}{
other read.table arguments}
\item{mat}{
matrix to be changed in annotated}
\item{tag}{
annotation}
}
\details{
The file (called filename) MUST contain 3 columns before the 
actual values, which are called CLID, NAME and GWEIGHT, described
bellow. The first row must be the header of the columns (starting 
with CLID,NAME and GWEIGHT) and the second row must be EWEIGHT. 
Representing how much weight each column has: if some columns are n 
replicates they can have each a weight of 1/n. 
}
\value{
Data frame composed of
\item{CLID}{Column called CLID which is the ID of the features, 
which will then be the rownames of the dataframe}
\item{NAME}{A possibly longer name, 
more meaningfull than CLID, text format}
\item{GWEIGHT}{A weight for each gene or feature. 
If some genes are less important than others or only a pathway 
should be selected than the file (called filename) 
should have this information}
\item{Matrix}{The matrix with numbers of the different observations}
}
\author{
Rachel Jeitziner}
\seealso{
ttmap_part1_ctrl_adj}
\examples{
d <- matrix(c(1,2,3,4,5,6),nrow=2)
d <- as.data.frame(d)
d$CLID <- c("Gene1","Gene2")
d$NAME <- c("Gene of interest one","Gene of interest two")
d$GWEIGHT <- c(1,1)
rownames(d) <- d$CLID
d <- d[,c(4,5,6,1,2,3)]
d <- rbind(c(NA,NA,NA,1,1,1),d)
rownames(d)[1]<-"EWEIGHT"
d[1,1]<-"EWEIGHT"
tag <- d[,c(1:3)]
mat <- d[-1,c(4:6)]
to_be_saved <- TTMap:::mat2pcl(mat,tag)
TTMap:::write_pcl(to_be_saved,"/the_file",getwd())
d <- TTMap:::read_pcl("the_file",na.type = "",Nrows= -1)
}