\name{autoplot.MCA}
\alias{autoplot.MCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
autoplot.MCA(acm, axes = c(1, 2), mod = TRUE, ind = FALSE, filtre = 0, axis.plot = TRUE, alpha = 1, point.type = "petit", ellipses = NA, coloriage = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acm}{
%%     ~~Describe \code{acm} here~~
}
  \item{axes}{
%%     ~~Describe \code{axes} here~~
}
  \item{mod}{
%%     ~~Describe \code{mod} here~~
}
  \item{ind}{
%%     ~~Describe \code{ind} here~~
}
  \item{filtre}{
%%     ~~Describe \code{filtre} here~~
}
  \item{axis.plot}{
%%     ~~Describe \code{axis.plot} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{point.type}{
%%     ~~Describe \code{point.type} here~~
}
  \item{ellipses}{
%%     ~~Describe \code{ellipses} here~~
}
  \item{coloriage}{
%%     ~~Describe \code{coloriage} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (acm, axes = c(1, 2), mod = TRUE, ind = FALSE, filtre = 0, 
    axis.plot = TRUE, alpha = 1, point.type = "petit", ellipses = NA, 
    coloriage = NA) 
{
    .e <- environment()
    require(ggplot2)
    require(directlabels)
    pt <- "."
    if (point.type \%in\% "gros") 
        pt <- 16
    data <- as.data.frame(cbind(acm$var$coord, acm$var$cos2))
    names(data) <- c(paste(dimnames(acm$var$coord)[[2]], ".coord", 
        sep = ""), paste(dimnames(acm$var$cos2)[[2]], ".cos2", 
        sep = ""))
    modalites <- apply(acm$call$X, 2, FUN = function(x) levels(as.factor(x)))
    k <- 0
    for (i in 1:length(modalites)) {
        modalites[[i]] <- row.names(data)[(k + 1):(k + length(modalites[[i]]))]
        k <- k + length(modalites[[i]])
    }
    k <- 0
    variables <- unlist(lapply(modalites, length))
    for (i in 1:length(variables)) {
        data[(k + 1):(k + variables[i]), "var"] <- names(variables[i])
        data[(k + 1):(k + variables[i]), "label"] <- modalites[[i]]
        k <- k + variables[i]
    }
    individus <- as.data.frame(acm$ind$coord)
    names(individus) <- gsub(" ", "", names(individus))
    p <- ggplot(data[(data[, paste("Dim ", axes[1], ".cos2", 
        sep = "")] > filtre) | (data[, paste("Dim ", axes[2], 
        ".cos2", sep = "")] > filtre), ], aes(x = get(eval(names(data)[axes[1]])), 
        y = get(eval(names(data)[axes[2]]))), environment = .e) + 
        xlab(paste("Dimension ", axes[1], " - ", round(acm$eig[axes[1], 
            "percentage of variance"], 2), " \%", sep = "")) + 
        ylab(paste("Dimension", axes[2], " - ", round(acm$eig[axes[2], 
            "percentage of variance"], 2), " \%", sep = ""))
    if (axis.plot) 
        p <- p + geom_vline(xintercept = 0, colour = "dark gray") + 
            geom_hline(yintercept = 0, colour = "dark gray")
    if (ind & is.na(coloriage)) 
        p <- p + geom_point(data = individus, aes(x = get(names(individus)[axes[1]]), 
            y = get(names(individus)[axes[2]])), shape = pt, 
            alpha = min(1, alpha * 1000/dim(individus)[1]))
    if (ind & !(is.na(coloriage))) {
        data2 <- as.data.frame(cbind(individus, as.data.frame(acm$call$X[, 
            coloriage])))
        names(data2) <- c(names(individus), coloriage)
        p <- p + geom_point(data = data2, aes(x = get(eval(names(data2)[axes[1]])), 
            y = get(eval(names(data2)[axes[2]])), colour = get(coloriage)), 
            shape = pt, alpha = min(1, alpha * 1000/dim(individus)[1])) + 
            scale_colour_discrete(name = coloriage) + guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    if (ind & !(is.na(ellipses))) {
        require(ellipse)
        require(rgrs)
        require(boot)
        data.ellipses <- data.frame(individus, acm$call$X[, ellipses])
        names(data.ellipses)[(dim(individus)[2] + 1):(dim(individus)[2] + 
            length(ellipses))] <- ellipses
        df_ell <- data.frame()
        for (g in levels(data.ellipses[, ellipses])) {
            temp <- data.ellipses[data.ellipses[, ellipses] == 
                g, ]
            df_ell <- rbind(df_ell, cbind(as.data.frame(ellipse(corr(as.matrix(data.frame(temp[, 
                names(data.ellipses)[axes[1]]], temp[, names(data.ellipses)[axes[2]]])), 
                w = acm$call$row.w[data.ellipses[, ellipses] == 
                  g]), scale = c(sqrt(wtd.var(temp[, names(data.ellipses)[axes[1]]], 
                weights = acm$call$row.w[data.ellipses[, ellipses] == 
                  g])), sqrt(wtd.var(temp[, names(data.ellipses)[axes[2]]], 
                weights = acm$call$row.w[data.ellipses[, ellipses] == 
                  g]))), centre = c(weighted.mean(temp[, names(data.ellipses)[axes[1]]], 
                w = acm$call$row.w[data.ellipses[, ellipses] == 
                  g]), weighted.mean(temp[, names(data.ellipses)[axes[2]]], 
                w = acm$call$row.w[data.ellipses[, ellipses] == 
                  g])))), group = g))
        }
        if (!(is.na(coloriage)) & (coloriage == ellipses)) {
            p <- p + geom_path(data = df_ell, aes(x = x, y = y, 
                linetype = group, colour = group)) + scale_linetype_discrete(name = ellipses)
        }
        else {
            p <- p + geom_path(data = df_ell, aes(x = x, y = y, 
                linetype = group)) + scale_linetype_discrete(name = ellipses)
        }
    }
    if (mod & is.na(coloriage)) 
        p <- p + geom_point(aes(colour = var, shape = var), size = 4) + 
            geom_dl(aes(label = label, colour = var), method = "smart.grid", 
                show_guide = FALSE) + scale_colour_discrete(name = "Variables") + 
            scale_shape_discrete(name = "Variables")
    if (mod & !(is.na(coloriage))) 
        p <- p + geom_point(aes(shape = var), size = 4) + geom_dl(aes(label = label), 
            method = "smart.grid", show_guide = FALSE) + scale_shape_discrete(name = "Variables")
    print(p)
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
