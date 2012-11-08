\name{fortify.MCA}
\alias{fortify.MCA}
\title{Prepares the data for \code{ggplot.model}.}
\usage{
  fortify.MCA(model, data, ...)
}
\arguments{
  \item{model}{an model of class MCA}

  \item{data}{not used.}

  \item{...}{other parameters to be passed.}
}
\value{
  a dataframe suitable for \code{autoplot.MCA}.
}
\description{
  Prepares the data for \code{ggplot.model}.
}
\examples{
library(FactoMineR)
data(tea)
tea.mca <- MCA(tea[,1:18], graph=FALSE)
autoplot(tea.mca)
}
\seealso{
  \code{\link{autoplot.MCA}}
}
\keyword{ggplot2,}
\keyword{graphics}
\keyword{MCA,}

