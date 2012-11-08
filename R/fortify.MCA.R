#' @title Prepares the data for \code{ggplot.model}. 
#'
#' @param model an model of class MCA
#' @param data not used.
#' @param ... other parameters to be passed.
#' @return a dataframe suitable for \code{autoplot.MCA}.
#' @keywords MCA, ggplot2, graphics
#' @seealso \code{\link{autoplot.MCA}} 
#' @export
#' @examples
#' library(FactoMineR)
#' data(tea)
#' tea.mca <- MCA(tea[,1:18], graph=FALSE)
#' autoplot(tea.mca)


fortify.MCA <- function(model, data, ...) {
  ## model doit être un objet de classe MCA
  
  .e <- environment()
    
  df <- as.data.frame(cbind(model$var$coord,model$var$contrib)) ## on récupère les coordonnées et les contributions des variables seulement
  names(df) <- c(paste(dimnames(model$var$coord)[[2]],".coord",sep=""),paste(dimnames(model$var$contrib)[[2]],".contrib",sep=""))
  modalites <- apply(model$call$X, 2, FUN=function(x) levels(as.factor(x)))
  k <- 0
  for (i in 1:length(modalites)) {
    modalites[[i]] <- row.names(df)[(k+1):(k+length(modalites[[i]]))]
    k <- k + length(modalites[[i]])
  }
  k <- 0
  variables <- unlist(lapply(modalites, length))
  for (i in 1:length(variables)) {
    df[(k+1):(k+variables[i]),"var"] <- names(variables[i])
    df[(k+1):(k+variables[i]),"label"] <- modalites[[i]]
    k <- k + variables[i]
  }
  df[,paste("size",1:model$call$ncp,sep="")] <- model$var$contrib
  df$type <- "variable"
  
  individus <- as.data.frame(cbind(model$ind$coord, model$ind$contrib))
  names(individus) <- c(paste(dimnames(model$var$coord)[[2]],".coord",sep=""),paste(dimnames(model$var$contrib)[[2]],".contrib",sep=""))
  individus[,c("var", "label")] <- NA
  individus[,paste("size",1:model$call$ncp,sep="")] <- model$call$row.w
  individus$type <- "individu"
  df <- rbind(df, individus)
  return(df)
}

fortify.MCAlist <- fortify.MCA