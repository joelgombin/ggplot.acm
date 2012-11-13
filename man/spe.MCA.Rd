\name{spe.MCA}
\alias{spe.MCA}
\title{Specific MCA}
\usage{
  spe.MCA(data, excl = NULL, ncp = 5, quali.sup = 0,
    excl.sup = list(0))
}
\arguments{
  \item{data}{a data frame with n rows (individuals) and p
  columns (categorical variables)}

  \item{excl}{a vector indicating the indexes of the "junk"
  categories (see the function \code{\link{getindexcat}})}

  \item{ncp}{number of dimensions kept in the results (by
  default 5)}

  \item{quali.sup}{index of the supplementary variables}

  \item{excl.sup}{list of indexes of "junk" categories of
  supplementary variables}
}
\value{
  an object of class 'MCA' or 'spMCA', in a similar way as
  FactoMineR MCA function
}
\description{
  Specific MCA
}
\author{
  Nicolas Robette & Joël Gombin <joel.gombin@gmail.com>
}

