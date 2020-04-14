# for average
mydata1<-Universities # import your excel file
mydata <- scale(mydata1[,2:7])#
d<-dist(mydata,method = "euclidean")  #computing the distance matrics
fit <- hclust(d,method = "average")   #building algorith
plot(fit)#display dendogram
groups <- cutree(fit,k=4) # cut tree into 4 clusters
rect.hclust(fit,k=4,border = "red")   # draw dendogram with red borders arount the 4 cluster

clusters=data.frame('Uni'=mydata1[,1],'Cluster'=groups)  # attach the cluster number to uni
#for centroid

mydata <- scale(mydata1[,2:7])
d<-dist(mydata,method = "euclidean")
fit <- hclust(d,method = "centroid")
plot(fit)
groups <- cutree(fit,k=4)
rect.hclust(fit,k=4,border = "red")
clusters=data.frame('Uni'=mydata1[,1],'Cluster'=groups)


#K means ckustering
install.packages("plyr")
library(plyr)
x <- runif(50) #random uniform distribution
y <- runif(50)
data<-cbind(x,y)
plot(data)
#Elbow chart
wss<-c() # difining vector
for (i in 2:15)
wss[i]<-sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab="no of cluster",ylab = "avg distance")#"b" means lins and dots both
#cluster algorithm building
km <- kmeans(data,10)
km$centers
km$cluster
#animation
install.packages("animation")
library(animation)
windows() # will open in new window
km <- kmeans.ani(data,5)

