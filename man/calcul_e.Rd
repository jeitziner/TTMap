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
output of ttmap_part1_ctrl_adj}
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
ttmap_part1_ctrl_adj,ttmap_part1_hda,
ttmap_part2_gtlmap,ttmap_sgn_genes,generate_mismatch_distance}
\examples{
## See full example ttmap_sgn_genes
##--
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
org.directory = tempdir(),e=0,P=1.1,B=0);
Kprime <- 10;
TTMAP_part1_hda <- TTMap::ttmap_part1_hda(x = TTMAP_part1prime,
k = Kprime,dataname = "Effect_of_cancer",
normalname = "The_healthy_controls");
annot <- c(paste(colnames(
the_experiment$TEST[,-seq_len(3)]),"Dis",sep="."),
paste(colnames(the_experiment$CTRL[,-seq_len(3)]),"Dis",sep="."))
annot <- cbind(annot,annot)
rownames(annot)<-annot[,1]
dd5_sgn_only <-TTMap::generate_mismatch_distance(TTMAP_part1_hda,
select=rownames(TTMAP_part1_hda$Dc.Dmat),alpha = ALPHA)
e<-TTMap::calcul_e(dd5_sgn_only,0.95,TTMAP_part1prime,1)
}