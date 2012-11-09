\name{autoplot.MCA}
\alias{autoplot.MCA}
\title{\code{ggplot} output for MCA objects.}
\usage{
  autoplot.MCA(object, axes = c(1, 2), mod = TRUE,
    ind = FALSE, filtre = 0, axis.plot = TRUE, alpha = 1,
    point.type = "petit", ellipses = NA, coloriage = NA,
    taille = FALSE, dl.method = "smartgrid")
}
\arguments{
  \item{object}{a MCA-class object.}

  \item{axes}{A 2-length vector. Selects which dimensions
  should be displayed.}

  \item{mod}{whether the variables modalities should be
  represented.}

  \item{ind}{whether the individuals should be
  represented.}

  \item{filtre}{indicates the value of the contribution
  above which modalities should be represented. If it takes
  the value "moyenne", then the mean of the contributions
  is used.}

  \item{axis.plot}{whether the axes should be plotted.}

  \item{alpha}{the alpha parameter for the individual
  points.}

  \item{point.type}{controls the size of individual points.
  Can be "petit" (small) or "gros" (big).}

  \item{ellipses}{a variable name. Ellipses are drawn for
  each modality of this variable.}

  \item{coloriage}{a variable name. Individual points are
  colored acoordingly to the modalities of this variable.
  In that case, the variables' modalities are drawn in
  black.}

  \item{taille}{whether the individual (resp. modalities)
  points' size should be proportional to their weight
  (resp. contribution).}

  \item{dl.method}{the method to be used for direct
  labeling. See
  \url{http://directlabels.r-forge.r-project.org/docs/index.html}.}
}
\value{
  a \code{ggplot2} object, which is also printed.
}
\description{
  \code{ggplot} output for MCA objects.
}
\details{
  This method takes an object of class MCA, and produces a
  \code{ggplot2} graphical representation of a factorial
  plan, with several options.
}
\examples{
library(FactoMineR)
data(tea)
tea.mca <- MCA(tea[,1:18], graph=FALSE)
autoplot(tea.mca)
}
\seealso{
  \code{\link{fortify.MCA}}
}
\keyword{ggplot2,}
\keyword{graphics}
\keyword{MCA,}

