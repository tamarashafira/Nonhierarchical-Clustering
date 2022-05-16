data=read.delim("clipboard")
View(data)
head(data)
str(data)

#empty data
summary(is.na(data)

library(MVN)
hz.test <- mvn(data[,c(2:5)], mvnTest = "mardia", multivariatePlot = 'qq')
hz.test       

library(PerformanceAnalytics)
chart.Correlation(data)        
data.new= data[,-3]

#transformasi z score
trans= scale(data.new[,c(2:4)])

library(factoextra)
distance= get_dist(trans)
distance
fviz_dist(get_dist(distance))

set.seed(1234)
clusterr= kmeans(trans, centers = 4, nstart = 100)
clusterr
clusterr$betweenss

#visualization of cluster
fviz_cluster(clusterr, data=trans)
fviz_nbclust(trans,kmeans, method="wss")
fviz_nbclust(trans,kmeans, method="silhouette")
fviz_nbclust(trans,kmeans, method="gap_stat")

##Segmentasi
result=data.frame(trans, clusterr$cluster)
head(result)

cluster1=subset(result, clusterr.cluster==1)
cluster2=subset(result, clusterr.cluster==2)
cluster3=subset(result, clusterr.cluster==3)
cluster4=subset(result, clusterr.cluster==4)
cluster1
cluster2
cluster3
cluster4
cluster$size

k1=sapply(kluster1[,-4], mean)
k2=sapply(kluster2[,-4], mean)
k3=sapply(kluster3[,-4], mean)
k4=sapply(kluster4[,-4], mean)
meantotal=rbind(k1,k2,k3,k4)
barplot(t(meantotal), beside=T)
