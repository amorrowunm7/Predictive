library(stats)
library(gplots)
library(cluster)
library(ggfortify)


#computes cosine distance function
cosineDist <- function(x){
  as.dist( 1-x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Pearson
dist2 <- function(x, ...)
  as.dist(1-cor(t(x),method="pearson"))

hierarchical <- function(x,k){
d <- dist(data,method ="euclidean")
#d <-  cosineDist(as.matrix(data))
h_com <- hclust(d, method = "ward")
clusters <- cutree(h_com, k = k)
list(cluster=clusters)
}

#load data
lung <- read.table("lungCancer.txt",sep="\t",header=T)
head(lung)
dim(lung)
lung.ts<-data.frame(t(lung))
names(lung.ts) <- as.matrix(lung.ts[1, ])
lung.ts <- lung.ts[-1, ]
lung.ts[] <- lapply(lung.ts, function(x) type.convert(as.character(x)))
dim(lung.ts)
str(lung.ts)
lung.ts.var<-apply(lung.ts, 2, var)
lung.ts.var.sort<-sort(lung.ts.var)
length(lung.ts.var.sort)
lung.ts.var.high<-lung.ts.var.sort[(20501-999):20501]
length(lung.ts.var.high)
lung.data<-lung.ts[, c(names(lung.ts.var.high))]



#compute distance metric
d <- dist(lung.data,method ="euclidean")

#hierarchical clustering
h_com <- hclust(d, method = "complete")
h_avg <- hclust(d, method = "average")
h_single <- hclust(d, method = "single")
h_ward <- hclust(d,method="ward.D2")
#plot the hierarchical tree
plot(h_com)
plot(h_avg)
plot(h_single)
plot(h_ward)


#Cut tree at 100 ~ T
clusters1 <- data.frame(cutree(h_ward, h = 400))


#cut tree by providing the desired number of clusters
clusters2 <- data.frame(cutree(h_com, k = 3))


autoplot(prcomp(lung.data))

# visualize the dissimilarity matrix
#visualize as a heatmap
#orginal distance matrix before clustering
heatmap.2(as.matrix(d),scale="none",dendrogram="none",trace="none",Rowv=FALSE,Colv=FALSE)
#distance matrix after clustering
heatmap.2(as.matrix(d),scale="none",dendrogram="both",trace="none",Rowv=as.dendrogram(h_ward),Colv=as.dendrogram(h_ward))

#OR
dst <- data.matrix(d)
dst <- dst[h_com$order,h_com$order]
heatmap.2(as.matrix(dst),scale="none",dendrogram="none",trace="none",Rowv=FALSE,Colv=FALSE)


#visualizing the actual data as a heatmap
#heatmap.2(as.matrix(data),scale="col",col=bluered,dendrogram="row",trace="none",Rowv=as.dendrogram(h_com),Colv=FALSE)

#getting average silhouette and sse for different numbers of clusters
#needs cluster library


kmax=10
sil <- rep(0, kmax)
see <- rep(0, kmax)
dst <- data.matrix(d)
for(k in 2:kmax)
{
 clusters <- cutree(h_ward, k = k)
 si <- silhouette(clusters,dmatrix=dst)
 sil[k]= mean(si[,3])
 for(i in 1:k)
 {
  dd <- dst[clusters==i,]
  dd <- dd[,clusters==i]
  see[k] = see[k] + sum(sum(dd))/2 
 }
}
# Plot the  average silhouette width
plot(1:kmax, sil[1:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Avg silhouette")
abline(v = which.max(sil[1:kmax]), lty = 2)

plot(1:kmax, see[1:kmax], type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="SSE")
#abline(v = which.min(see[1:kmax]), lty = 2)




#gap statistic (requires custom R function )
gap <- rep(0,kmax)
gap_stat <- clusGap(data,FUNcluster = hierarchical,K.max=kmax)
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:kmax, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:kmax, gap-error, 1:kmax, gap+error,length=0.05, angle=90, code=3)


#NbClust to determine the number of clusters (limited only to kmeans and hierarchical)

dev.off()



