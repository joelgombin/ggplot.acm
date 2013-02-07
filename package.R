#' @title \code{ggplot2} plots made easy for Multiple correspondence analysis (MCA)
#' 
#' @description \code{ggplot.acm} provides an easy way of creating a \code{ggplot2}-style plot for a multiple correspondence analysis run with the \code{FactoMineR} package (this is important, as other packages/functions may product objects with a different internal structure). Multiple options are provided, which should adress the most common uses in social sciences. The \code{autoplot.MCA} function extends the \code{autoplot} generic function, and returns a \code{ggplot2} object, which can then be further modified. 
#' 
#' @import ggplot2, directlabels, ellipse, rgrs, boot
#' @docType packag
#' @name ggplot.acm
#' @aliases package-ggplot.acm ggplot.acm ggplot.acm-package
NULL