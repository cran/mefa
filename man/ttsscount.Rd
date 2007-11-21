\name{ttsscount}
\alias{ttsscount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes Object of Class 'sscount' from Table}
\description{
The function makes an object of class 'sscount' from a table (data frame).
}
\usage{
ttsscount(table, species.columns = TRUE, segment = "unspecified")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{a data frame.}
  \item{species.columns}{logical, if \code{TRUE} (default), species are columns, if \code{FALSE}, species are rows.}
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
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{drtsscount}}, \code{\link{sscount}}
}
\examples{
### Example 1: simple atrificial data

xc <- as.data.frame(rbind(
c(4, 0, 0),
c(2, 4, 0),
c(0, 0, 5),
c(0, 0, 0)
))
rownames(xc) <- c("sample1", "sample2", "sample3", "sample4")
colnames(xc) <- c("species1", "species2", "species3")

ttsscount(xc)

### Example 2: field data of Villany Hills

\dontrun{
data(vtable)

v1 <- ttsscount(vtable, FALSE)
v1

v2 <- ttsscount(vtable, species.columns = FALSE, segment = "adult")
v2

v3 <- ttsscount(vtable)
v3
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}