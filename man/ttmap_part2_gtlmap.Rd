\name{ttmap_part2_gtlmap}
\alias{ttmap_part2_gtlmap}
\alias{create_links}
\alias{create_colors}
\alias{create_places}
\alias{cutoff_low}
\alias{annot_right}
\title{
Visualisation of the clustering
}
\description{
Enables a quick view on the groups in the dataset (globally) and 
how locally they differ.
}
\usage{
ttmap_part2_gtlmap(ttmap_part1_hda, m1, 
select = row.names(ttmap_part1_hda$Dc.Dmat), 
ddd, e, filename = "TEST", n = 3, ad = 0, bd = 0, piq = 1, 
dd = generate_mismatch_distance(ttmap_part1_hda = ttmap_part1_hda, 
select = select), mean_value_m1 = "N", ni = 2)
create_links(q, q1, f, f1, m, d, l)
create_colors(q, muc, size)
create_places(size)
cutoff_low(low_map, para = 1, text = "low")
annot_right(p_high, p, l, d_high, d_mid2, d_mid1, d_low, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ttmap_part1_hda}{
list output of ttmap_part1_hda}
\item{m1}{
either a user imputed vector whose names are the names of the 
samples with addition of .Dis. or by default it is the amount of 
deviation}
\item{select}{
Should all the features (default) or only a sublist be considered 
to calculate the distance}
\item{ddd}{
Annotation matrix with rownames the different sample names with 
addition of .Dis. There can be as many columns as wanted, but only 
the column n will be selected to annotated the clusters}
\item{e}{
integer parameter defining under which value two samples are 
considered to be close}
\item{filename}{
Name for the description file annotating the clusters}
\item{n}{ The column to be considered to annotate the clusters
}
\item{ad}{
if ad!=0 then the clusters on the output picture will not be 
annotated}
\item{bd}{
if different than 0 (default), the output will be without outliers 
of the test data set (clusters composed of only "piq" element)}
\item{piq}{
parameter used to determine what small clusters are, see bd}
\item{dd}{
the distance matrix to be used}
\item{mean_value_m1}{
if == "N" the average of the values in m1 divided by the number of 
the samples are put into the legend (by default represents the 
average of the samples in a cluster of the mean-deviation of the 
features) otherwise it will show the average value of the values in 
m1 (is useful for instance if m1 represents the age of the 
samples) }
\item{ni}{
The column to consider to annotate the samples (is put into 
parenthesis) for the description file}
\item{q}{
clusters,i.e. a list of lists in each list i there is the names of 
the sample in cluster i }
\item{q1}{
the clusters in another layer}
\item{f}{
the place of the clusters in q}
\item{f1}{
the place of the clusters in q1 }
\item{m}{
the space in direction x between the different layers}
\item{d}{
the space in direction y between the layers}
\item{l}{
the space in direction z between the layers}
\item{size}{
the size of the different clusters}
\item{low_map}{
a submatrix of the distance matrix}
\item{para}{
parameter used to determine what small clusters are, see piq,bd }
\item{text}{
a text describing in which layer the clusters are}
\item{p_high}{
place where to put the annotation}
\item{p}{
place where to put the annotation}
\item{d_high}{
place where to put the annotation}
\item{d_mid2}{
place where to put the annotation}
\item{d_mid1}{
place where to put the annotation}
\item{d_low}{
place where to put the annotation}
\item{muc}{is equal to m1}
}
\value{
\item{all}{the clusters in the overall group}
\item{low}{the clusters in the lower quartile group}
\item{mid1}{the clusters in the first middle quartile group}
\item{mid2}{the clusters in the second middle quartile group}
\item{high}{the clusters in the higher quartile group}
}
\details{
Is the Two-tiers Mapper function. The output is an interactive 
image of the clusters in the different layers.}
\author{
Rachel Jeitziner}
\seealso{
ttmap_part1_ctrl_adj,ttmap_part1_hda,ttmap_sgn_genes}
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
}