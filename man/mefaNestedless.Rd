\encoding{UTF-8}
\name{mefaNestedless}
\alias{mefaNestedless}
\title{Make Non-Nested 'mefa' Object}
\description{
The functions makes a non-nested 'mefa' object.
}
\usage{
mefaNestedless(x)
}
\arguments{
  \item{x}{an objects of class 'mefa' with nested segments.}
}
\value{
Returns an object of class 'mefa', with non-nested segments. This function is internally used by the \code{\link{melt.mefa}} function.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \doi{10.18637/jss.v029.i08}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{\code{\link{mefa}}, \code{\link{is.mefa}}, \code{\link{melt.mefa}}}
\examples{
data(dol.count)
x <- mefa(stcs(dol.count), nested = TRUE)
x
y <- mefaNestedless(x)
y
}
\keyword{ manip }
