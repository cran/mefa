\name{mefadocs}
\alias{mefadocs}
\title{Display Package Documentation}
\description{
  Display package documentation using \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
mefadocs(doc = c("ChangeLog","SampleReport"))
}
\arguments{
  \item{doc}{The name of the document (partial match, case sensitive). }
}
\author{Peter Solymos adapted Jari Oksanen's \code{vegandocs} function in the 'vegan' package.}

\examples{
\dontrun{
mefadocs("Change")
mefadocs("Sample")
}
}
\keyword{ utilities }
\keyword{ documentation }
