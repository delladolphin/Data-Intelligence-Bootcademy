#3a)
library(ggplot2)
CityA<-read.table(text="
No.of.salespeople Sales.volume
1 400
2 480
3 500
4 600
6 750
8 800
", header=T)
CityB<-read.table(text="
No.of.salespeople Sales.volume
0 50 
2 200
3 450 
4 550 
6 750
10 850 
11 900
", header=T)

#normalization
CityA$nNo.of.salespeople<- CityA$No.of.salespeople/3
CityA$nSales.volume <- CityA$Sales.volume/500
CityA
CityB$nNo.of.salespeople <- CityB$No.of.salespeople/4
CityB$nSales.volume <- CityB$Sales.volume/550
CityB

#3b)
#plot the normalized data if two cities
ggplot(data = CityA,mapping=aes(x=nNo.of.salespeople,y=nSales.volume))+
  geom_line() + 
  geom_point()
ggplot(data=CityB,aes(x=nNo.of.salespeople, y=nSales.volume))+
  geom_line() + 
  geom_point()

#3c)
#logistic model for city A 
fit <- nls(f=nSales.volume~a/(1+exp(-(b+c*nNo.of.salespeople)))+d, data = CityA, 
            start=list(a=0.8, b=-3.325, c=2.51, d=0.8))

summary(fit)
#R2
sse <- sum(residuals(fit)^2)
sst <- sum((fit$m$lhs()-mean(fit$m$lhs()))^2)
r2 <- 1-(sse/sst)
r2

#logistic model for city B
fit1 <- nls(f=nSales.volume~a/(1+exp(-(b+c*nNo.of.salespeople)))+d, data = CityB, 
            start=list(a=1.54, b=-1.9, c=1.9, d=0.091))
summary(fit1)
#R2
sse <- sum(residuals(fit1)^2)
sst <- sum((fit1$m$lhs()-mean(fit1$m$lhs()))^2)
r2 <- 1-(sse/sst)
r2

#3d)
ggplot(data=CityA, mapping=aes(x=nNo.of.salespeople))+
  geom_point(mapping = aes(y=nSales.volume))+
  geom_line(mapping=aes(y=predict(fit)))+
  labs(x="nNo.of.salespeople",y="nSales.volume")

ggplot(data=CityB, mapping=aes(x=nNo.of.salespeople))+
  geom_point(mapping = aes(y=nSales.volume))+
  geom_line(mapping=aes(y=predict(fit1)))+
  labs(x="nNo.of.salespeople",y="nSales.volume")

#3e)
net<- function(nNo.of.salespeople,fit) {
  nd<-data.frame(nNo.of.salespeople=nNo.of.salespeople)
  pnSales.volume <-predict(fit, newdata=nd)
  pnSales.volume
  
  margin<-32
  pSales_volume <-pnSales.volume*500
  gross <- margin*pSales_volume
  NSpeople<- nNo.of.salespeople*3
  cost<-NSpeople*1500
  net<-(gross-cost)
  print (cbind(pSales_volume,NSpeople))
  return(net)
}
net(7/3,fit)

net1<- function(nNo.of.salespeople,fit1) {
  nd<-data.frame(nNo.of.salespeople=nNo.of.salespeople)
  pnSales.volume <-predict(fit1, newdata=nd)
  pnSales.volume
  
  margin<-50
  pSales_volume <-pnSales.volume*550
  gross <- margin*pSales_volume
  NSpeople<- nNo.of.salespeople*4
  cost<-NSpeople*1000
  net1<-(gross-cost)
  print (cbind(pSales_volume,NSpeople))
  return(net1)
}
net1(5/4,fit1)

#3f
net2<-function(nsf) {
  nd<-data.frame(nNo.of.salespeople=nsf[1])
  nsalesa<-predict(fit,nd)
  margina<-32
  salesa<-nsalesa*500
  sfa<-nsf[1]*3
  costa<-sfa*1500
  grossa<-margina*salesa
  neta<-grossa-costa
  
  nd<-data.frame(nNo.of.salespeople=nsf[2])
  nsalesb<-predict(fit1,nd)
  marginb<-50
  salesb<-nsalesb*550
  sfb<-nsf[2]*4
  costb<-sfb*1000
  grossb<-marginb*salesb
  netb<-grossb-costb
  
  net2=neta+netb
  
  print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,costa,costb,grossa,grossb,neta,netb,net2))
  return(net2)
}
net2(c(3,2))
op<-optim(par=c(3,2),fn=net2,control=list(fnscale=-1))
op
op$par[1]*3 
op

net2(op$par)
op<-constrOptim(theta=c(1,1),f=net3,
                control=list(fnscale=-1),
                ui=c(-4500,-4000),ci=-15000,grad=NULL)
op

net3(op$par)
op$par[1]*3
op$par[2]*4

