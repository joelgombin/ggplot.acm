#' @title \code{ggplot} output for MCA objects. 
#' @details This method takes an object of class MCA, and produces a \code{ggplot2} graphical representation of a factorial plan, with several options. 
#' @param object a MCA-class object.
#' @param axes a 2-length vector. Selects which dimensions should be displayed. 
#' @param mod whether the variables modalities should be represented.
#' @param quali.sup whether the supplementary (illustrative) variables' modalities should be represented. 
#' @param ind whether the individuals should be represented.
#' @param filtre indicates the value of the contribution above which modalities should be represented. If it takes the value "moyenne", then the mean of the contributions is used.
#' @param axis.plot whether the axes should be plotted.
#' @param alpha the alpha parameter for the individual points.
#' @param point.type controls the size of individual points. Can be "petit" (small) or "gros" (big).
#' @param ellipses a variable name. Ellipses are drawn for each modality of this variable.
#' @param coloriage a variable name. Individual points are colored acoordingly to the modalities of this variable. In that case, the variables' modalities are drawn in black.
#' @param taille whether the individual (resp. modalities) points' size should be proportional to their weight (resp. contribution).   
#' @param dl.method the method to be used for direct labeling. See \url{http://directlabels.r-forge.r-project.org/docs/index.html}.
#' @return a \code{ggplot2} object, which is also printed. 
#' @keywords MCA, ggplot2, graphics
#' @seealso \code{\link{fortify.MCA}} 
#' @export
#' @examples
#' library(FactoMineR)
#' data(tea)
#' tea.mca <- MCA(tea[,1:18], graph=FALSE)
#' autoplot(tea.mca)
autoplot.MCA <- function(object, axes=c(1,2), mod=TRUE,quali.sup=TRUE, ind=FALSE, filtre=0, axis.plot=TRUE, alpha=1, point.type="petit", ellipses=NA, coloriage=NA, taille=FALSE,dl.method="smart.grid") {
  
  .e <- environment()
  toLoad <- c("ggplot2", "directlabels", "rgrs", "boot", "ellipse")
  erreur <- unlist(lapply(toLoad, require, character.only = TRUE))
  toInstall <- toLoad[which(erreur %in% FALSE)]
  if (length(toInstall) > 0) {
    install.packages(toInstall, repos = "http://cran.r-project.org")
    lapply(toInstall, require, character.only = TRUE)
  }
  
  df <- fortify(object)
  df[df$type %in% "variable", "size"] <- df[df$type %in% "variable",sprintf("size%s", axes[1])] + df[df$type %in% "variable",sprintf("size%s", axes[2])]
  df[df$type %in% "individu", "size"] <- (df[df$type %in% "individu",sprintf("size%s", axes[1])] + df[df$type %in% "individu",sprintf("size%s", axes[2])]) / mean((df[df$type %in% "individu",sprintf("size%s", axes[1])] + df[df$type %in% "individu",sprintf("size%s", axes[2])])) * 4
  df[df$type %in% "quali.sup", "size"] <- mean(df[df$type %in% "variable","size"]) # on donne une taille moyenne aux modalités des variables supplémentaires
  
  if (filtre %in% "moyenne") {filtre <- 100/dim(df[df$type %in% "variable",])[1]}
  
  pt <- "."
  if (point.type %in% "gros") pt <- 16
  
  ## sélection de la partie du df pertinente
  
  if (mod & quali.sup & ind) {
    cond <- (df$type %in% "quali.sup") | (df$type %in% "individu") | ((df$type %in% "variable") & (df[,paste("Dim ",axes[1],".contrib",sep="")] > filtre)) | (df$type %in% "variable" & df[,paste("Dim ",axes[2],".contrib",sep="")] > filtre)
  }
  if (mod & quali.sup & !ind) {
    cond <- (df$type %in% "quali.sup") | ((df$type %in% "variable") & (df[,paste("Dim ",axes[1],".contrib",sep="")] > filtre)) | (df$type %in% "variable" & df[,paste("Dim ",axes[2],".contrib",sep="")] > filtre)
  }
  if (mod & !quali.sup & !ind) {
    cond <- ((df$type %in% "variable") & (df[,paste("Dim ",axes[1],".contrib",sep="")] > filtre)) | (df$type %in% "variable" & df[,paste("Dim ",axes[2],".contrib",sep="")] > filtre)
  }
  if (mod & !quali.sup & ind) {
    cond <- ((df$type %in% "variable") & (df[,paste("Dim ",axes[1],".contrib",sep="")] > filtre)) | (df$type %in% "variable" & df[,paste("Dim ",axes[2],".contrib",sep="")] > filtre) | (df$type %in% "individu")
  }
  if (!mod & quali.sup & ind) {
    cond <- (df$type %in% "quali.sup") | (df$type %in% "individu")
  }
  if (!mod & quali.sup & !ind) {
    cond <- (df$type %in% "quali.sup")
  }
  if (!mod & !quali.sup & ind) {
    cond <- (df$type %in% "individu")
  }
  if (!mod & !quali.sup & !ind) {
    stop('Nothing to plot!')
  }
  
  p <- ggplot(df[cond,], aes(x=get(eval(names(df)[axes[1]])), y=get(eval(names(df)[axes[2]]))),environment=.e) # la base
  variable <- df[cond, "var"]
  p <- p + xlab(paste("Dimension ",axes[1]," - ",round(object$eig[axes[1],"percentage of variance"],2)," %",sep="")) # légende de l'axe des abcisses
  p <- p + ylab(paste("Dimension ", axes[2]," - ",round(object$eig[axes[2],"percentage of variance"],2)," %",sep="")) # légende de l'axe des ordonnées
  
  if (axis.plot) {p <- p + geom_vline(xintercept=0, colour="dark gray") + geom_hline(yintercept=0, colour="dark gray")} # représenter les axes
  
  if (ind & is.na(coloriage)) { # représentation des individus sans coloriage
    p <- p + geom_point(data=df[df$type %in% "individu",],aes(x=get(names(df)[axes[1]]), y=get(names(df)[axes[2]])), shape=pt, alpha=min(1,alpha*1000/dim(df[df$type %in% "individu",])[1]), environment=.e) + scale_shape_manual(values=1:length(unique(pt)))
    } 
  
  if (ind & !(is.na(coloriage)) ) { # représentation des individus avec coloriage
    data2 <- as.data.frame(cbind(df[df$type %in% "individu",],as.data.frame(object$call$X[,coloriage])))
    names(data2) <- c(names(df),coloriage)
p <- p + geom_point(data=data2,aes(x=get(eval(names(data2)[axes[1]])), y=get(eval(names(data2)[axes[2]])), colour=get(eval(coloriage))), shape=pt, alpha=min(1,alpha*1000/dim(df[df$type %in% "individu",])[1])) + scale_colour_discrete(name=coloriage) + guides(colour = guide_legend(override.aes = list(alpha = 1))) + scale_shape_manual(values=1:length(unique(pt)))
  }
  
  if (ind & !(is.na(ellipses))) { ## pour représenter les individus et des ellipses de concentration
    ## code emprunté à http://stackoverflow.com/questions/2397097/how-can-a-data-ellipse-be-superimposed-on-a-ggplot2-scatterplot
    data.ellipses <- data.frame(df[df$type %in% "individu",],object$call$X[,ellipses])
    names(data.ellipses)[(dim(df[df$type %in% "individu",])[2]+1):(dim(df[df$type %in% "individu",])[2]+length(ellipses))] <- ellipses
    df_ell <- data.frame()
    for (g in levels(data.ellipses[,ellipses])) {
      temp <- data.ellipses[data.ellipses[,ellipses]==g,]
      df_ell <- rbind(df_ell, cbind(as.data.frame(ellipse(corr(as.matrix(data.frame(temp[,names(data.ellipses)[axes[1]]],temp[,names(data.ellipses)[axes[2]]])), w=object$call$row.w[data.ellipses[,ellipses]==g]), 
                                                          scale=c(sqrt(wtd.var(temp[,names(data.ellipses)[axes[1]]],weights=object$call$row.w[data.ellipses[,ellipses]==g])),sqrt(wtd.var(temp[,names(data.ellipses)[axes[2]]],weights=object$call$row.w[data.ellipses[,ellipses]==g]))), 
                                                          centre=c(weighted.mean(temp[,names(data.ellipses)[axes[1]]],w=object$call$row.w[data.ellipses[,ellipses]==g]), weighted.mean(temp[,names(data.ellipses)[axes[2]]],w=object$call$row.w[data.ellipses[,ellipses]==g])))),group=g))
    }
    if (!(is.na(coloriage)) & (coloriage == ellipses)){
      p <- p + geom_path(data=df_ell,aes(x=x, y=y,linetype=group,colour=group)) + scale_linetype_discrete(name = ellipses)
    } else {
      p <- p + geom_path(data=df_ell,aes(x=x, y=y,linetype=group)) + scale_linetype_discrete(name = ellipses)
    }
  }
  
  if ((mod | quali.sup) & is.na(coloriage)) {
    if (taille) {
      p <- p + geom_point(aes(colour=var, shape=var, size=size)) + geom_dl(aes(label=label,colour=var, size=size), method=dl.method, show_guide=FALSE) + scale_colour_discrete(name = "Variables") + scale_shape_manual(name = "Variables",values=1:length(unique(variable))) + scale_size_continuous(guide=FALSE)
      }
    else {
      p <- p + geom_point(aes(colour=var, shape=var), size=4) + geom_dl(aes(label=label,colour=var), method=dl.method, show_guide=FALSE) + scale_colour_discrete(name = "Variables") + scale_shape_manual(name = "Variables",values=1:length(unique(variable)))
    }
  }
  
  if ((mod |quali.sup) & !(is.na(coloriage))) {
    if (taille) {
      p <- p + geom_point(aes(shape=var,size=size)) + geom_dl(aes(label=label), method=dl.method, show_guide=FALSE) + scale_shape_manual(name = "Variables",values=1:length(unique(var))) + scale_size_continuous(guide=FALSE)
    }
    else {
      p <- p + geom_point(aes(shape=var), size=4) + geom_dl(aes(label=label), method=dl.method, show_guide=FALSE) + scale_shape_manual(name = "Variables",values=1:length(unique(variable)))
    }
  }
  
  # print(p)
  return(p)
}

autoplot.MCAlist <- autoplot.MCA