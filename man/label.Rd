\encoding{UTF-8}
\name{label}
\alias{label}
\alias{label<-}
\title{Assign Label to an Object and Return It}
\description{
This function assigns a label to any kind of object. The label later can be retrieved and reused.
}
\usage{
label(x)
label(x) <- value
}
\arguments{
  \item{value}{any expression.}
  \item{x}{any object.}
}
\details{
The label can be any expression assigned to any object. It is stored as a character attribute. Inheritance is not implemented, so by any transformation of the object into another one, the label is lost.
}
\value{
Print out the label value.
}
\references{
\enc{S\'olymos}{Solymos} P. (2008) mefa: an R package for handling and reporting count data. \emph{Community Ecology} \bold{9}, 125--127.

\enc{S\'olymos}{Solymos} P. (2009) Processing ecological data in R with the mefa package. \emph{Journal of Statistical Software} \bold{29(8)}, 1--28. \doi{10.18637/jss.v029.i08}

\url{http://mefa.r-forge.r-project.org/}
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
x <- 1:10
x
## Assign a character
label(x) <- "x is a vector"
x
## Get back the label
label(x)
## Note the effect of deparsing
label(x) <- 1:10
label(x)
}
\keyword{ manip }
