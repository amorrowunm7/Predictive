library(mclust)
#mclust with up to 10 clusters
lung.mclust<-Mclust(lung.data, G = c(2,10))
lung.mclust$classification
#mclust results
lung.table<-table(lung.mclust$classification)
#
#
#
lung.mclust$BIC
#Top 3 models based on the BIC criterion:
#  VEI,9   VEI,10    VEI,8 
#-2202313 -2202641 -2204310 
# VEI,9 @ -2202313 is best model
plot(lung.mclust, what = "BIC")
