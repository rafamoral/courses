

sumPartition <- function(dat,z){
  k <-length(unique(z))
  nobs <- tapply(1:nrow(dat),z,length)
  withinSS <- tapply(1:nrow(dat),z,function(i){
    if (length(i)==1) 0
    else {x<- dat[i,]
    xm <- scale(x, scale=F)
    sum(xm^2)
    }
  })
  aveD <- tapply(1:nrow(dat),z,function(i){
    if (length(i)==1) 0
    else {x<- dat[i,]
    xm <- scale(x, scale=F)
    xm <- apply(xm, 1, function(y) sqrt(sum(y^2)))
    mean(xm)
    }
  })
  
  maxD <- tapply(1:nrow(dat),z,function(i){
    if (length(i)==1) 0
    else {x<- dat[i,]
    xm <- scale(x, scale=F)
    xm <- apply(xm, 1, function(y) sqrt(sum(y^2)))
    max(xm)
    }
  })
  
  part<- data.frame("N.obs"=nobs, "Within clus SS" = withinSS, "Ave dist  Centroid" = aveD,
                    "Max dist centroid" =maxD)
  rownames(part)<- paste("Cluster", 1:k)
  
  
  
  cCen <- cbind(simplify2array(tapply(1:nrow(dat),z,function(i){
    x<- dat[i,]
    if (length(i)==1) x
    else {
      colMeans(x)
    }
  })), colMeans(dat))
  colnames(cCen)<- c(paste("Cluster", 1:k), "Grand centrd")
  
  cCenD <- as.matrix(dist(t(cCen[, -ncol(cCen)])))
  cat("Final Partition\n")
  cat("\n")
  cat(paste("Number of clusters ", k))
  cat("\n")
  
  cat("\n")
  print(part, quote=F)
  cat("\n")
  cat("\n")
  cat("Cluster centroids\n")
  cat("\n")
  print(cCen,quote=F)
  cat("\n")	
  cat("\n")
  cat("Distances between Cluster centroids\n")
  cat("\n")
  print(cCenD,quote=F)		
}