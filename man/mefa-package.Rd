\name{mefa-package}
\alias{mefa-package}
%\alias{mefa}
\docType{package}
\title{
Faunistic Count Data Handling and Reporting
}
\description{
R package for faunistic count data handling and reporting. The name 'mefa' stands for the term 'metafaunistics' indicating that handling of basic data is only the first, but the most critical and sometimes most time consuming part of data analysis. It contains functions to create and manage objects combining basic faunistic (sample/species/count or crosstabulated) count data and sample/species attribute tables. Segments within the count data and samples with zero count can be indicated and used in subsequent operations. Reports can be generated in plain text or LaTeX format.
}
\details{
\tabular{ll}{
Package: \tab mefa\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-11-14\cr
License: \tab GPL-2\cr
}
}
\author{
Peter Solymos <Solymos.Peter@aotk.szie.hu>
}
\references{
\url{http://www.univet.hu/users/psolymos/personal/}
}
\keyword{ package }
\examples{
### Demo showning possible usage of 'mefa' objects
\dontrun{
library(mefa)
demo(dolina)
}
}
