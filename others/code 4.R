#install.packages("psych")
#install.packages("ggplot2")
library(psych)
library(ggplot2)
library(reshape2)

#3a
write.table(milk,file="milk.csv")
milk<-read.table("milk.csv")
milk

#summary
describe(milk[,2:11],skew=F,ranges=F)

#standardization
milk1<-scale(milk[,2:11])
head(milk1)

#Squared Euclidean distance
dist<-dist(milk1,method="euclidean")^2
dist

#Single linkage
#use "ward.D"
fit <- hclust(dist, method="ward.D")

#clustering history
history<-cbind(fit$merge,fit$height)
history

#distance plot
ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

#dendrogram
par(mar=c(1,4,1,1))
plot(fit,labels=milk$id,hang=-1,main="")
axis(side = 2, at = seq(0, 16, 2))

#3b
cluster<-cutree(fit,k=3)
cluster
id<-milk$id
sol <- data.frame(cluster,milk1,id)
sol
sol[ order(sol$cluster),c(1,12)]
tb<-aggregate(x=sol[,2:11], by=list(cluster=sol$cluster),FUN=mean)
tb
print(tb,digits=2)
table(sol[,1])
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")
milk

#3c
#rerun K-means with seeds from the Ward's method
dist<-dist(milk1,method="euclidean")^2
fit2 <- hclust(dist, method="ward.D")
ward.sol<-cutree(fit2,k=3)
ward.sol
tb<-aggregate(milk1[,1:10],by=list(ward=ward.sol),FUN=mean)
tb

#use the centers as seeds
fit1<-kmeans(x=milk1[,1:10],centers=tb[,2:11],algorithm="Hartigan-Wong")
fit1

#cluster centers
tb<-fit1$centers
tb
print(tb,digits=3)

tb<-data.frame(cbind(tb,cluster=1:3))
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")

#3d
#socio-demographic characteristics and motives of 3 clusters
sol1 <- data.frame(cluster,milk1,milk[,12:15])
sol1
sol1[order(sol1$cluster),c(1,12:15)]
tb1<-aggregate(sol1[,12:15],by=list(cluster=sol1$cluster),FUN=mean)
print(tb1)
