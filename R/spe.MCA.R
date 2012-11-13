#' @title Specific MCA
#' @name spe.MCA
#' @param data a data frame with n rows (individuals) and p columns (categorical variables)
#' @param excl a vector indicating the indexes of the "junk" categories (see the function \code{\link{getindexcat}})
#' @param ncp number of dimensions kept in the results (by default 5)
#' @param quali.sup index of the supplementary variables
#' @param excl.sup list of indexes of "junk" categories of supplementary variables
#' @return an object of class 'MCA' or 'spMCA', in a similar way as FactoMineR MCA function
#' @export
#' @author Nicolas Robette & Joël Gombin <joel.gombin@@gmail.com>

#' 

spe.MCA <- function(data,excl=NULL,ncp=5, quali.sup=0,excl.sup=list(0)) {
  if (is.null(excl)) {
    acm <- MCA(X=data[,-quali.sup],ncp=ncp,graph=FALSE)
  } else {
    acm <- spMCA(data=data[,-quali.sup],excl=excl,ncp=ncp)    
  }
  if (quali.sup != 0) {
    sup <- list()
    for (i in 1:length(quali.sup)) {
      sup[[i]] <- varsup(acm,data[,quali.sup[i]],excl.sup=excl.sup[[i]])
    }
    extract <- function(i) lapply(sup, function (y) y[[i]])
    acm$quali.sup <- sapply(c("coord","cos2","var","v.test"), function(i) do.call(rbind, extract(i)),simplify=FALSE)
    acm$call$quali <- (1:dim(data)[2])[-quali.sup]
    acm$call$quali.sup <- quali.sup
    if (!identical(excl.sup,list(0))) {
      acm$call$excl.sup <- excl.sup
      class(acm) <- c("es.MCA",class(acm))
    }
    acm$call$X <- data
  }
  return(acm)
}


spMCA <- function(data,excl=NULL,ncp=5) {
  n <- nrow(as.data.frame(data))
  Q <- ncol(as.data.frame(data))
  Z <- tab.disjonctif(data)
  K <- ncol(Z)
  eI <- matrix(rep(1,length=n),ncol=1)
  eK <- matrix(rep(1,length=K),ncol=1)
  Z0 <- Z-(1/n)*crossprod(t(eI))%*%Z
  NK <- diag(colSums(Z))
  Z0t <- Z0[,-excl]
  NKp <- NK[-excl,-excl]
  H0t <- (1/sqrt(Q))*Z0t%*%diag(1/sqrt(colSums(Z)[-excl]))
  svd <- svd(H0t)
  dims <- paste('dim',1:ncp,sep='.')
  noms <- vector(length=ncol(Z))
  id=0
  for(i in 1:Q) {
    for(j in 1:length(levels(data[,i]))) {
      id=id+1
      noms[id] <- paste(colnames(data)[i],levels(data[,i])[j],sep='.')
    }}
  YIt <- sqrt(n)*svd$u%*%diag(svd$d)
  YKpt <- sqrt(n*Q)*diag(1/sqrt(colSums(Z)[-excl]))%*%svd$v%*%diag(svd$d)
  eig <- list(svd$d*svd$d)
  eig[[2]] <- round(eig[[1]]/sum(eig[[1]])*100,2)
  eig[[3]] <- cumsum(eig[[2]])
  coord <- YIt[,1:ncp]
  contrib <- 100/n*coord*coord/matrix(rep(eig[[1]][1:ncp],times=n),ncol=ncp,nrow=n,byrow=T)
  dimnames(coord) <- list(1:n,dims)
  dimnames(contrib) <- list(1:n,dims)
  ind <- list(coord=coord,contrib=round(contrib,6))
  coord <- YKpt[,1:ncp]
  fK <- colSums(Z)[-excl]/n
  contrib <- 100*(fK/Q)*coord*coord/matrix(rep(eig[[1]][1:ncp],times=ncol(Z0t)),ncol=ncp,nrow=ncol(Z0t),byrow=T)
  s <- vector()
  for(i in 1:Q) s <- c(s,rep(i,times=length(levels(data[,i]))))
  s <- s[-excl]
  v.contrib <- aggregate(contrib,list(s),sum)[,-1]
  dimnames(v.contrib) <- list(colnames(data),dims)
  ctr.cloud <- data.frame(100*(1-fK)/(ncol(Z0t)-Q))
  colnames(ctr.cloud) <- 'ctr.cloud'
  vctr.cloud <- aggregate(ctr.cloud,list(s),FUN=sum)[-1]
  dimnames(vctr.cloud) <- list(colnames(data),'vctr.cloud')
  cos2 <- coord*coord/((1/fK)-1)
  dimnames(coord) <- list(noms[-excl],dims)
  dimnames(contrib) <- list(noms[-excl],dims)
  dimnames(cos2) <- list(noms[-excl],dims)
  eta2 <- matrix(nrow=Q,ncol=ncp)
  for(j in 1:Q) {
    vrc <- aggregate(ind$coord,list(data[,j]),var)[,-1]
    # for(i in 1:ncol(x)) x[is.na(x[,i]),i] <- 0
    wi <- apply(vrc,2,weighted.mean,w=as.numeric(table(data[,j])))
    be <- eig[[1]][1:ncp]-wi
    eta2[j,] <- be/eig[[1]][1:ncp]
  }
  dimnames(eta2) <- list(colnames(data),dims)
  v.test <- sqrt(cos2)*sqrt(n-1)*(((abs(coord)+coord)/coord)-1)
  var <- list(coord=coord,contrib=round(contrib,6),cos2=round(cos2,6),v.test=round(v.test,6),eta2=round(eta2,6),v.contrib=v.contrib)
  X <- data
  marge.col <- colSums(Z[,-excl])/(n*Q)
  names(marge.col) <- noms[-excl]
  marge.row <- rep(1/(n*Q),times=n)
  names(marge.row) <- 1:n
  quali <- 1:Q
  call <- list(X=X,mar.col=marge.col,marge.row=marge.row,ncp=ncp,quali=quali,excl=excl)
  RES <- list(eig=eig,call=call,ind=ind,var=var,svd=list(vs=svd$d,U=svd$u,V=svd$v))
  attr(RES,'class') <- c('spMCA','MCA','list') # rajout de la classe MCA pour pouvoir utiliser les mêmes méthodes 
  RES
}



varsup <- function(resmca,var,excl.sup=0) {
  # excl.sup : vecteur d'entiers indiquant les modalités à ne pas représenter
  filtre <- !(var %in% levels(var)[excl.sup])
  var <- var[!(var %in% levels(var)[excl.sup])]
  
  var <- factor(var)
  FK <- as.numeric(table(var))/length(var)
  if(attr(resmca,'class')[1] %in% c('MCA','spMCA')) {
    v <- var
    coord <- aggregate(resmca$ind$coord[filtre,],list(v),mean)[-1]
    for(i in 1:resmca$call$ncp) coord[,i] <- coord[,i]/resmca$svd$vs[i]
    cos2 <- coord*coord/((1/FK)-1)
  }
  if(attr(resmca,'class')[1]== 'csMCA') {
    v <- var[resmca$call$var==resmca$call$lev]
    fK <- as.numeric(table(v))/length(v)
    n <- nrow(resmca$call$X[resmca$call$var==resmca$call$lev,])
    coord <- aggregate(resmca$ind$coord,list(v),sum)[-1]/n/FK
    for(i in 1:resmca$call$ncp) coord[,i] <- coord[,i]/resmca$svd$vs[i]
    cos2 <- coord*coord*FK*FK/fK/(1-fK)
  }
  rownames(coord) <- levels(var)
  rownames(cos2) <- levels(var)
  vrc <- aggregate(resmca$ind$coord[filtre,],list(v),var)[-1]
  wi <- apply(vrc,2,weighted.mean,w=as.numeric(table(v)))
  be <- resmca$eig[[1]][1:resmca$call$ncp]-wi
  eta2 <- be/resmca$eig[[1]][1:resmca$call$ncp]
  vrc <- rbind(vrc,wi,be,resmca$eig[[1]][1:resmca$call$ncp],eta2)
  vrc <- round(vrc,6)
  rownames(vrc) <- c(levels(var),'within','between','total','eta2')
  coord <- round(coord,6)
  v.test <- sqrt(cos2)*sqrt(length(var)-1)
  v.test <- (((abs(coord)+coord)/coord)-1)*v.test
  list(coord=coord,cos2=round(cos2,6),var=round(vrc,6),v.test=round(v.test,6))
}
