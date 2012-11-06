\name{fortify.MCA}
\alias{fortify.MCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fortify.MCA(model, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (model, data, ...) 
{
    .e <- environment()
    df <- as.data.frame(cbind(model$var$coord, model$var$contrib))
    names(df) <- c(paste(dimnames(model$var$coord)[[2]], ".coord", 
        sep = ""), paste(dimnames(model$var$contrib)[[2]], ".contrib", 
        sep = ""))
    modalites <- apply(model$call$X, 2, FUN = function(x) levels(as.factor(x)))
    k <- 0
    for (i in 1:length(modalites)) {
        modalites[[i]] <- row.names(df)[(k + 1):(k + length(modalites[[i]]))]
        k <- k + length(modalites[[i]])
    }
    k <- 0
    variables <- unlist(lapply(modalites, length))
    for (i in 1:length(variables)) {
        df[(k + 1):(k + variables[i]), "var"] <- names(variables[i])
        df[(k + 1):(k + variables[i]), "label"] <- modalites[[i]]
        k <- k + variables[i]
    }
    df[, paste("size", 1:acm$call$ncp, sep = "")] <- acm$var$contrib
    df$type <- "variable"
    individus <- as.data.frame(cbind(model$ind$coord, model$ind$contrib))
    names(individus) <- c(paste(dimnames(model$var$coord)[[2]], 
        ".coord", sep = ""), paste(dimnames(model$var$contrib)[[2]], 
        ".contrib", sep = ""))
    individus[, c("var", "label")] <- NA
    individus[, paste("size", 1:acm$call$ncp, sep = "")] <- acm$call$row.w
    individus$type <- "individu"
    df <- rbind(df, individus)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
