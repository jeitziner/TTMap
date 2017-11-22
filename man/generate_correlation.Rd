\name{generate_correlation}
\alias{generate_correlation}
\alias{generate_euclidean}
\alias{generate_mismatch_distance}
\alias{generate_p_val_correlation}
\alias{generate_single_cell_complete_mismatch}
\alias{generate_single_cell_mismatch_with_parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates different distance matrices
}
\description{
Single cell complete mismatch distance, single cell complete mismatch distance with a parameter of cutoff, mismatch distance, correlation distance, p-value of correlation test distance and euclidean distance.
}
\usage{
	generate_single_cell_complete_mismatch(ttmap_part2, select, alpha = 1)
generate_single_cell_mismatch_with_parameter(ttmap_part2, select, alpha = 1)
generate_correlation(ttmap_part2, select)
generate_euclidean(ttmap_part2, select)
generate_mismatch_distance(ttmap_part2, select, alpha = 1)
generate_p_val_correlation(ttmap_part2, select)

}
\arguments{
  \item{ttmap_part2}{
an object given back by ttmap_part2}
  \item{select}{
A sublist of rownames of ttmap_part2$Dc.Dmat
}
 \item{alpha}{
 	A real number corresponding to a cutoff
 }
}
\details{
	If one is interested only in clustering samples according to a list of genes belonging to a certain pathway, then this list is provided to the parameter select. 
Alpha is a cutoff for deviations that should be considered as noise, for gene expression data such as normalised RNA-seq or microarrays for instance a cutoff of 1, corresponding to a two fold change is being chosen.
}
\value{
Distance matrix
}

\author{
Rachel Jeitziner}

\seealso{
ttmap_part3}
\examples{
ttmap_part2 <- list()
ttmap_part2$Dc.Dmat <- matrix(c(-1,2,0,-4,5,6),nrow=2)
rownames(ttmap_part2$Dc.Dmat) <- c("Gene1","Gene2")
colnames(ttmap_part2$Dc.Dmat) <- c("A","B","C")
dd <- TTMap:::generate_mismatch_distance(ttmap_part2,select = rownames(ttmap_part2$Dc.Dmat))
dd <- TTMap:::generate_euclidean(ttmap_part2,select = rownames(ttmap_part2$Dc.Dmat))
}