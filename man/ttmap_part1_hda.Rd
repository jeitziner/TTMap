\name{ttmap_part1_hda}
\alias{ttmap_part1_hda}
\title{
Calculation of deviation components
}
\description{
ttmap-part1-hda function calculates the hyperrectangle deviation
assessment (HDA) that calculates the deviation components using 
normal_hda2 which calculates the normal component of the test 
sample and deviation_hda2 which calculates the deviation component.
}
\usage{
ttmap_part1_hda(x, k = dim(x$Normal.mat)[2], dataname, 
normalname,Org.directory=getwd())
}
\arguments{
\item{x}{
output object given back by ttmap_part1_ctrl_adj, list}
\item{k}{
A factor if not all the lines in the control group should be kept}
\item{dataname}{
the name of the project}
\item{normalname}{
A name for the corrected control files}
\item{Org.directory}{
where the outputs should be saved}
}
\details{
The function performs the Hyperrectangle deviation 
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
ttmap_part1_ctrl_adj,ttmap_part2_gtlmap,ttmap_sgn_genes}
\examples{
##a full example can be found in ttmap_sgn_genes
##--
library(breastCancerNKI)
data(nki)
ALPHA <- 1
the_experiment <- TTMap::make_matrices(nki
,seq_len(10),seq_len(10)+40,
rownames(nki),rownames(nki))
TTMAP_part1prime <-TTMap::ttmap_part1_ctrl_adj(
normal.pcl = the_experiment$CTRL,
tumor.pcl = the_experiment$TEST, 
normalname = "The_healthy_controls", 
dataname = "Effect_of_cancer", 
org.directory = getwd(),e=0,P=1.1,B=0);
Kprime <- 10;
TTMAP_part1_hda <- TTMap::ttmap_part1_hda(x = TTMAP_part1prime,
k = Kprime,dataname = "Effect_of_cancer",
normalname = "The_healthy_controls");
}