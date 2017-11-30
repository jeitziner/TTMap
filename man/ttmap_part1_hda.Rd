\name{ttmap_part1_hda}
\alias{ttmap_part1_hda}
\alias{normal_hda2}
\alias{deviation_hda2}
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
normal_hda2(Dmat, Nmodel, new.cnames = "Norm")
deviation_hda2(Dmat, Nc.Dmat, new.cnames = "Dis")
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
\item{Dmat}{
Test samples in a matrix}
\item{Nmodel}{
Normal samples in a matrix}
\item{Nc.Dmat}{
Output of normal_hda2}
\item{new.cnames}{
New names to give to the matrix created}
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
data(nki)
ALPHA <- 1
junk <- TTMap:::SumExp2pcl(nki,1:10,50:60,
rownames(nki),rownames(nki))
TTMAP_part1prime <-TTMap::ttmap_part1_ctrl_adj(
normal.pcl = junk$CTRL,
tumor.pcl = junk$TEST, 
normalname = "Hi", dataname = "Hello", 
org.directory = getwd(),e=0,P=1.1,B=0);
Kprime <- 10;
TTMAP_part1_hda <- TTMap::ttmap_part1_hda(x = TTMAP_part1prime,
k = Kprime,dataname = "Hello", normalname = "Hi");
}