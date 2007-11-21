\name{strify}
\alias{strify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stratifies Object of Class 'xcount' or 'mefa'}
\description{
Stratifies an object of class 'xcount' or 'mefa' according to levels of a factor, or sample/species attribute, respectively.
}
\usage{
strify(xc, strata, which = c("samples", "species"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{an object of class 'xcount' or 'mefa' to be stratified.}
  \item{strata}{a vector with length equal to the number of samples/species (according to \code{which} argument).
    For a 'mefa' object \code{strata} is a column within the samples/species attribute table (according to
    \code{which} argument).}
  \item{which}{switch between rows (\code{"samples"}) and columns (\code{"species"}) to be stratified.}
}
\value{
A result is an object of class 'xcount'.
  \item{segment}{segment of the original object.}
  \item{data}{matrix containing count data of sample/species crosstabulation with dimensions according to
    stratification. Total count value is the same for the new object, but counts within strata are summed
    according to stratification.}
  \item{nsamples}{number of rows (samples) in \code{data}.}
  \item{nspecies}{number of columns (species) in \code{data}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{as.xcount}}, \code{\link{check.attrib}}, \code{\link{mefa}}, \code{\link{sscount}}, 
  \code{\link{xcount}}, \code{\link{xorder}}
}
\examples{
\dontrun{
data(landsnail, vhabitat, vsample, vtable)

v1 <- as.xcount(vtable, FALSE, segment="mixed")
spec <- xorder(v1, which="species", landsnail, 2)
sampl <- xorder(v1, which="samples", vsample)
vmf <- mefa(v1, sampl, spec)

v2 <- strify(vmf, strata = "site.descr", which = "samples")
v2

check.attrib(v2, which="samples", vhabitat, 2)
habi <- xorder(v2, which="samples", vhabitat, 2)
vmf2 <- mefa(v2, habi, spec)

v3 <- strify(vmf2, strata = "familia", which = "species")
v3
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}