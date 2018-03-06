library(igraph)

data <- read.table("yeast.txt",sep="\t",header=T,row.names=1)
#computes cosine distance function
cosineDist <- function(x){
  as.dist( 1-x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#computes pearson correlation distance function
dist2 <- function(x, ...)
  as.dist(1-cor(t(x),method="pearson"))


fb <- as.undirected(read_graph("facebook_combined.txt"))
fb_com <- cluster_louvain(fb, weights = NULL)



plot(fb_com, as.undirected(fb),layout=layout_with_fr,vertex.label=NA,vertex.size=3)

V(fb)$color <- fb_com$membership+1
fb <- set_graph_attr(fb, "layout", layout_with_kk(fb))
plot(fb, vertex.label.dist=1.5)