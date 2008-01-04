\name{mefa}
\alias{mefa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes Object of Class 'mefa'}
\description{
Makes and object of class 'mefa' from az 'xcont' and sample and species attribute 'xorder' objects.
}
\usage{
mefa(xc, xorder.samples, xorder.species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{object of class 'xcount'.}
  \item{xorder.samples}{object of class 'xorder' for sample attributes.}
  \item{xorder.species}{object of class 'xorder' for species attributes.}
}
\value{
A result is an object of class 'mefa'.
  \item{segment}{segment of the 'xcount' object.}
  \item{xcount}{count data from the 'xcount' object}
  \item{sample.attr}{data frame containing data of 'xorder' object containing sample attributes.}
  \item{species.attr}{data frame containing data of 'xorder' object containing species attributes.}
  \item{nsamples}{number of samples.}
  \item{nspecies}{number of species.}
  \item{totalcount}{total number of counts in \code{xcount}.}
  \item{nsample.attr}{number of sample attribute columns in \code{sample.attr}.}
  \item{nspecies.attr}{number of species attribute columns in \code{species.attr}.}
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{check.attrib}}, \code{\link{sscount}}, \code{\link{xcount}}, \code{\link{xorder}}
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
mf1

mf2 <- mefa(xcount(sscount(ss, zc="zero.count"), 2), xo1, xo2)
mf2

### Example 2: field data of Villany Hills

\dontrun{
data(landsnail, vsample, vtable)

vt <- as.xcount(vtable, FALSE)
spec <- xorder(vt, which="species", landsnail, 2)
sampl <- xorder(vt, which="samples", vsample, 1)

vmf <- mefa(vt, sampl, spec)
vmf

### Example 3: field data of the dolina

data(dol.count, dol.sample, landsnail)

dmf <- mefa(
dxc <- xcount(sscount(fill.count(dol.count), zc="zero.count")),
xorder(dxc, which="samples", dol.sample, 1),
xorder(dxc, which="species", landsnail, 2)
)

dmf
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{classes}
