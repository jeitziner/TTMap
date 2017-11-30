\name{ttmap_part1_ctrl_adj}
\alias{ttmap_part1_ctrl_adj}
\alias{meshRows_norm_hda}
\alias{meshRows_hda}
\title{
Calculates a corrected control group, discovers outliers in it.
}
\description{
ttmap_part1_ctrl_adj function 
finds outliers in the control group and 
removes them
}
\usage{
ttmap_part1_ctrl_adj(normal.pcl, tumor.pcl, normalname, dataname, 
org.directory = "", A = 1, e = 0, meth = 0, P = 1.1, B = 0)
meshRows_norm_hda(df1, df2)
meshRows_hda(df1, df2)
}
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
ttmap_part1_ctrl_adj calculates a 
corrected control group, discovers 
outliers in it.
meshRows_norm_hda will select only common features of df1 and df2 
and removes the tags
meshRows_hda will select only common features of df1 and df2 when 
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
And values of ttmap_part1_ctrl_adj
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
ttmap_part1_hda,ttmap_part2_gtlmap,ttmap_sgn_genes}
\examples{
##--
data(nki)
ALPHA <- 1
junk <- TTMap:::SumExp2pcl(nki,1:10,50:60,
rownames(nki),rownames(nki))
TTMAP_part1prime <-TTMap::ttmap_part1_ctrl_adj(
normal.pcl = junk$CTRL,
tumor.pcl = junk$TEST, 
normalname = "Hi", dataname = "Hello", 
org.directory = getwd(),e=0,P=1.1,B=0);
}