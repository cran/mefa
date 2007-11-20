\name{sscount}
\alias{sscount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes Object of Class 'sscount'}
\description{
The function makes an object of class 'sscount' from a data frame with columns for samples, species, count and optionally segment.
}
\usage{
sscount(sstable, zc = "NULL")
}
\arguments{
  \item{sstable}{dataframe with three or four columns. Columns must be in the order of sample, species, segment (optional) and count.}
  \item{zc}{identifier for samples with zero count, by default it is \code{"NULL"}. When non existing identifier is given, the value is set to default with warning.}
}
\value{
A result is an object of class 'sscount'.
  \item{data}{data frame with columns \code{sample}, \code{species}, \code{segment}, \code{count}.
    First three columns are factors, \code{count} is numeric.}
  \item{zc}{flag for rows with 0 total count, \code{zc="zero.count"}.}
  \item{nsamples}{number of samples (levels) in data column \code{sample}.}
  \item{nspecies}{number of species (levels) in data column \code{species}, 
    "pseudo-species" created by \code{zc} is excluded.}
  \item{segment.levels}{list of levels in data column \code{segment}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{drtsscount}}, \code{\link{fill.count}}, \code{\link{inflate}}, \code{\link{sscount}},
  \code{\link{ttsscount}}
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

sscount(ss)
sscount(ss, zc="zero.count")
sscount(ss[,c(1,2,4)], zc="zero.count")

### Example 2: field data of the dolina

\dontrun{
data(dol.count, dol.sample, landsnail)

sscount(fill.count(dol.count), zc="zero.count")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}