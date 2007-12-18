\name{exclmf}
\alias{exclmf}
\title{Excludes Species or Samples in Object of Class 'xcount' or 'mefa'}
\description{
Excludes species or samples in object of class 'xcount' or 'mefa'.
}
\usage{
exclmf(xc, which = c("samples", "species"), excl, empty = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{an object of class 'xcount' or 'mefa' to be stratified.}
  \item{which}{switch between rows (\code{"samples"}) and columns (\code{"species"}) to be excluded.}
  \item{excl}{list of rows/columns (samples/species) to be excluded (according to \code{which} argument). 
    If \code{excl=NULL}, nothing will be excluded (only useful when \code{empty=FALSE}).}
  \item{empty}{logical, if \code{TRUE} (default), rows with zero sum will be leaved intaxt, 
    if \code{FALSE}, rows with zero count will be excluded.}
}
\details{
If species are excluded, some samples might have zero total count. In such cases, 
it is advisable to use \code{empty=FALSE}.

Empty rows can be removed by \code{excl=NULL} and \code{empty=FALSE}.
}
\value{
A result is an object of class 'xcount' or 'mefa'.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\note{
If the resultant crosstable contains less than two rows/columns, a warning is given and there will be no output.
}
\seealso{\code{\link{mefa}}, \code{\link{xcount}}.}
\examples{
ss <- data.frame(
cbind(
c("sample1","sample1","sample2","sample2","sample3","sample4"),
c("species1","species1","species1","species2","species3","zero.count"),
c("male","female","male","female","male","male")
),
c(1, 2, 10, 3, 4, 1)
)
colnames(ss) <- c("sample.id", "species.id", "gender", "catch")

spectab <- as.data.frame(rbind(
	c("species3", "family1", "1"),
	c("species2", "family2", "5"),
	c("species1", "family1", "2"),
	c("species5", "family2", "1"),
	c("species4", "family1", "10")
))
colnames(spectab) <- c("species", "taxonomy", "size")

sampletab <- as.data.frame(rbind(
	c("sample3", "bad"),
	c("sample1", "good"),
	c("sample2", "good"),
	c("sample4", "bad")))
colnames(sampletab) <- c("sample", "quality")

xct <- xcount(sscount(ss, zc="zero.count"))
xo1 <- xorder(xct, "samples", sampletab, 1)
xo2 <- xorder(xct, "species", spectab, 1)
mf1 <- mefa (xct, xo1, xo2)

exclmf(xct, "samples", NULL, FALSE)
exclmf(xct, "samples", 1, FALSE)
exclmf(xct, "samples", 1, TRUE)
exclmf(xct, "samples", c(1:2), TRUE)

exclmf(xct, "species", 1, TRUE)

exclmf(mf1, "samples", 1, TRUE)
}
\keyword{manip}
