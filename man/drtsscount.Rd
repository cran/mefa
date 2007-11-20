\name{drtsscount}
\alias{drtsscount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes Object of Class 'sscount' from Table with Duplicate Rows}
\description{
The function makes an object ofclass 'sscount' from a table (data frame), when some samples are presented in more than one rows (identifiers are not unique).
}
\usage{
drtsscount(table, sample, segment = "unspecified")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{a data frame with count data.}
  \item{sample}{a vector with same langth as number of rows in \code{table}. Used as factor for coding samples.}
  \item{segment}{arbitrary name of the segment (eg. adult, juvenile, male, female), included in the table, 
    by default it is \code{"unspecified"}.}
}
\value{
A result is an object of class 'sscount'.
  \item{data}{data frame with columns \code{sample}, \code{species}, \code{segment}, \code{count}.}
  \item{zc}{flag for rows with 0 total count, \code{zc="zero.count"}.}
  \item{nsamples}{number of samples in \code{table}.}
  \item{nspecies}{number of species in \code{table}.}
  \item{segment.levels}{value of the \code{segment} argument.}
}
\note{
In \code{table}, columns must refer to species, unlike in \code{\link{ttsscount}}, where species can be rows either by using the \code{species.columns = False} argument. If this is the case, transpose the data frame as in the example below. Samples(rows) with 0 total count are flagged as 'zero.count' in the resulting 'sscount' object.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}, \code{\link{ttsscount}}
}
\examples{
\dontrun{
data(vsample, vtable)

v1 <- drtsscount(t(vtable), vsample$site.descr)
v1

v2 <- drtsscount(t(vtable), vsample$site.descr, segment = "adult")
v2
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}