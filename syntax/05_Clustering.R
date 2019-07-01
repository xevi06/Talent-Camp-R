load('data/dd.Rdata')

numericas = unlist(lapply(dd, is.numeric))
ddnum = dd[sample(1:nrow(dd),5000),numericas]

#hclust
clusters = hclust(dist(ddnum), method = "ward.D2")
plot(clusters, labels=FALSE)
clustersCut = cutree(clusters,3)

for(x in colnames(ddnum)){
  for(y in colnames(ddnum)){
    plot(ddnum[,x], ddnum[,y], col = clustersCut, xlab = x, ylab = y)
  }
}