library(stats)
library(gplots)
library(hopach)

#computes cosine distance function
cosineDist <- function(x){
  as.dist( 1-x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#computes pearson correlation distance function
dist2 <- function(x, ...)
  as.dist(1-cor(t(x),method="pearson"))




pdf("hopach_yeast2.pdf")
#load data
data <- read.csv("vehicle-2.csv",sep=",",header=T)

#compute distance metric
gene.dist_e<-distancematrix(data,"euclid")
gene.dist_cos<-distancematrix(data,"cosangle")
gene.hobj<-hopach(data,dmat=gene.dist_cos) #cluster the rows
#cluster the columns
#colresult<-hopach(t(data),dmat=distancematrix(t(rzs),d="cosangle"))

labels <- c(gene.hobj$clustering$labels[gene.hobj$clust$order])
print(table(labels))# get cluster sizes
uniq_labels <- unique(labels)
print("number of clusters")
print(length(uniq_labels))

#write results to file
write.table(data.frame(row = row.names(data),cluster=gene.hobj$clustering$labels),file="hopach_clusters.txt",quote=FALSE,sep="\t",row.names=FALSE)


#visualization of distance matrix
dplot(gene.dist_cos,gene.hobj,ord="final") # hopach inbuilt function to display 
gene.dist_cos_ordered <- gene.dist_cos[gene.hobj$clust$order,gene.hobj$clust$order] #order the matrix using the clustering result
heatmap.2(as.matrix(gene.dist_cos_ordered),scale="none",dendrogram="none",trace="none",Rowv=FALSE,Colv=FALSE) # display as a heatmap

#bootstrap
bobj<-boothopach(data,gene.hobj,B=100) #might take some time
bootplot(bobj,gene.hobj,ord="bootp",showclusters=TRUE)

dev.off()




