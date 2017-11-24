\name{FLAT.hda2}
\alias{FLAT.hda2}
\alias{impute_median}
\alias{f1prime}
\alias{f1prime2}
\alias{f1prime3}
\title{
flat.hda2
}
\description{
function
}
\usage{
FLAT.hda2(mat, el = 1, metho =0, p = 1.1, method_mean_or_median = 1)
impute_median(ma, med)
f1prime(xprime, mprime, eprime = 1)
f1prime2(xprime, mprime, eprime = 1)
f1prime3(xprime, mprime, eprime = 1)
}
\arguments{ 
\item{xprime}{
the column of the control matrix tested against the others }
\item{mprime}{
the other columns of the control matrix}
\item{eprime}{
cutoff parameter for outlier defined by el}
\item{ma}{
control matrix with NAs where there are outliers}
\item{med}{
vector of values per row to replace the NAs or a method as defined 
by metho}
\item{mat}{
control matrix}
\item{el}{
cutoff parameter for outlier}
\item{metho}{
Either 0 or a function that will describe how to replace the NAs in 
the corrected control matrix}
\item{p}{
If more than p percent of the expression for one gene per row are 
outliers this gene is removed}
\item{method_mean_or_median}{
How to define an outlier, determined by either being far to the mean 
(anything else than 0) or to the median (0, default)}
}
\details{
This function corrects for outliers in the control group. The 
assumption made is that the control group is as homogeneous as 
possible and this function finds out where it is not. 
}
\value{
\item{mat}{matrix}
\item{m}{number of genes removed per sample}
}
\author{
Rachel Jeitziner}
\seealso{
ttmap_part1}
\examples{
##see ttmap_part4}