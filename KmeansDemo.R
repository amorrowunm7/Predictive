library(gplots)
library(fpc)

#computes cosine distance function
cosineDist <- function(x){
  as.dist( 1-x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#computes pearson correlation distance function
dist2 <- function(x, ...)
  as.dist(1-cor(t(x),method="pearson"))

KMEANS <- function(x,k){
d <- dist(x,method ="euclidean")
#d <-  cosineDist(as.matrix(x))
res <- pam(d, k,diss=TRUE)
clusters <- data.frame(res$clustering)[,1]
list(cluster=clusters)
}


pdf("kmeans_pam.pdf")
#load data
data <- read.table("USArrests.txt",sep="\t",header=T,row.names =1)

#compute distance metric
d <- dist(data,method ="euclidean")
library(cluster)

kmax=10
sil <- rep(0, kmax)
see <- rep(0, kmax)
gap <- rep(0,kmax)
for(k in 2:kmax)
{
 res <- pam(d, k,diss=TRUE)
 sil[k]= res$silinfo$avg.width
 see[k] = res$clusinfo[,3]
}
# Plot the  average silhouette width
plot(2:kmax, sil[2:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Avg silhouette")
abline(v = which.max(sil[2:kmax]), lty = 2)

plot(2:kmax, see[2:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="SSE")
#abline(v = which.min(see[2:kmax]), lty = 2)

#gap statistic (requires custom R function )
gap <- rep(0,kmax)
gap_stat <- clusGap(data,FUNcluster = KMEANS,K.max=10)
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:kmax, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:kmax, gap-error/sqrt(nrow(data)), 1:kmax, gap+error/sqrt(nrow(data)),length=0.05, angle=90, code=3)

dev.off()

#NbClust to determine the number of clusters (limited only to kmeans and hierarchical)





