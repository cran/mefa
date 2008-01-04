\name{inflate}
\alias{inflate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inflates a Data Frame}
\description{
This function inflates a data frame according to a column containing integers, by repeating each rows as many times as the integer value. This is an internal function used by the function \code{\link{sscount}}.
}
\usage{
inflate(factors, count)
}
\arguments{
  \item{factors}{data frame used as factors to be repeated as many times as the value of \code{count}.
    Number of rows must equal length of \code{count}.}
  \item{count}{a numeric (integer) vector with the same length as number of rows in \code{factors}.}
}
\value{
A data frame with columns as in \code{factors} argument, and rows according to sum of \code{count}.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}, \url{http://www.univet.hu/users/psolymos/personal/}}
\seealso{
  \code{\link{sscount}}
}
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

inflate(ss[,1:3], ss[,4])
inflate(ss[,1:3], c(1:6))
inflate(ss[,1:3], rep(2, 6))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
