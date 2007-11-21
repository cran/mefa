\name{xcount}
\alias{xcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes an Object of Class 'xcount'}
\description{
The function makes an object of class 'xcount' from an object of class 'sscount'.
}
\usage{
xcount(ssc, segment = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ssc}{an object of class 'sscount'.}
  \item{segment}{identifier of the \code{segment} within the 'sscount' object
    to make the crosstabulation, the default \code{0} refers to \code{"all"} segments.}
}
\value{
A result is an object of class 'xcount'.
  \item{segment}{segment of the 'sscount' object of which the crosstabulation was made, or \code{"all"}.}
  \item{data}{matrix containing count data of sample/species crosstabulation.
    Crosstabulation results the same dimensions for data matrices of each segments.}
  \item{nsamples}{number of rows (samples) in \code{data}.}
  \item{nspecies}{number of columns (species) in \code{data}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{as.xcount}}, \code{\link{check.attrib}}, \code{\link{mefa}},
  \code{\link{sscount}}, \code{\link{xorder}}
}
\examples{
### Example 1: simple atrificial data

ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

xc1 <- xcount(sscount(ss, z="zero"))
xc1

xc2 <- xcount(sscount(ss, zc="zero.count"))
xc2

xc3 <- xcount(sscount(ss, zc="zero.count"), 0)
xc3

xc4 <- xcount(sscount(ss, zc="zero.count"), 1)
xc4

### Example 2: field data of the dolina

\dontrun{
data(dol.count, dol.sample, landsnail)

dxc <- xcount(sscount(fill.count(dol.count), zc="zero.count"))
dxc
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

