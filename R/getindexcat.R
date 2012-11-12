#' @title Lists the categories of a forthcoming MCA
#' @details may be useful before a specific MCA to identify the indexes of the "junk" categories
#' @export

getindexcat <- function(data) {
  temp <- discret(data)
  print(colnames(temp))
}

discret <- function(Y) {
  if(!is.data.frame(Y)) Y <- data.frame(Y)
  res <- matrix(nrow=nrow(Y),ncol=length(levels(Y[,1])))
  for(i in 1:ncol(Y)) {
    if(is.factor(Y[,i])==FALSE) Y[,i] <- factor(Y[,i])
    nlevels <- length(levels(Y[,i]))
    z <- matrix(nrow=nrow(Y),ncol=nlevels)
    for(j in 1:nlevels) z[,j] <- ifelse(Y[,i]==levels(Y[,i])[j],1,0)
    colnames(z) <- paste(names(Y)[i],levels(Y[,i]),sep=".")
    if(i==1) res <- z else res <- cbind(res,z)
  }
  res <- as.data.frame(res)
  res
}