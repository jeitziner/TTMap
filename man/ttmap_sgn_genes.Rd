\name{ttmap_sgn_genes}
\alias{ttmap_sgn_genes}
\alias{ttmap_sgn_genes_inter2}
\alias{ttmap_sgn_genes_inter}
\title{
Gives a list of associated genes per cluster
}
\description{
ttmap_sgn_genes function
}
\usage{
ttmap_sgn_genes(ttmap_part2_gtlmap, ttmap_part1_hda,
ttmap_part1_ctrl_adj, c, n = 2, a = 0, 
filename = "TEST2", annot = ttmap_part1_ctrl_adj$tag.pcl, 
col = "NAME", path = getwd(), Relaxed = 1)
ttmap_sgn_genes_inter2(q, ttmap_part1_hda, alpha = 0)
ttmap_sgn_genes_inter(q, ttmap_part1_hda, alpha = 0)
}
\arguments{
\item{ttmap_part2_gtlmap}{
output of ttmap_part2_gtlmap}
\item{ttmap_part1_hda}{
output of ttmap_part1_hda}
\item{ttmap_part1_ctrl_adj}{
output of ttmap_part1_ctrl_adj}
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
library(breastCancerNKI)
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
annot <- c(paste(colnames(junk$TEST[,-c(1:3)]),"Dis",sep="."),
paste(colnames(junk$CTRL[,-c(1:3)]),"Dis",sep="."))
annot <- cbind(annot,annot)
rownames(annot)<-annot[,1]
dd5_sgn_only <-TTMap:::generate_mismatch_distance(TTMAP_part1_hda,
select=rownames(TTMAP_part1_hda$Dc.Dmat),alpha = ALPHA)
TTMAP_part2 <-
TTMap::ttmap_part2_gtlmap(TTMAP_part1_hda,TTMAP_part1_hda$m,
select=rownames(TTMAP_part1_hda$Dc.Dmat),annot,
e= TTMap::calcul_e(dd5_sgn_only,0.95,TTMAP_part1prime,1), 
filename="TEST",n=1,dd=dd5_sgn_only)
TTMap:::ttmap_sgn_genes(TTMAP_part2, TTMAP_part1_hda, 
TTMAP_part1prime, annot,
n = 2, a = 1, filename = "TEST2",
annot = TTMAP_part1prime$tag.pcl, col = "NAME", 
path = getwd(), Relaxed = 1)
}