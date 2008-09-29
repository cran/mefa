\encoding{UTF-8}
\name{qvector}
\alias{qvector}
\title{Ordinal Transformation of a Vector}
\description{
Converts a numeric vector into an ordinal one based on quantiles.
}
\usage{
qvector(x, probs = seq(0, 1, 0.25))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.}
  \item{probs}{numeric vector of probabilities with values in [0, 1] used by \code{\link{quantile}}.}
}
\value{
A numeric vector of length the same as the input vector, containing the respective (upper) quantile values.
}
\references{
\enc{Sólymos}{Solymos} P. 2008. mefa: an R package for handling and reporting count data. \emph{Community Ecology} 9, 125--127.

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
  \code{\link{quantile}}
}
\examples{
x <- rnorm(20)
x
y <- qvector(x)
y
rbind(x, y)
plot(x, y, type="h")
}
\keyword{ manip }
