#1a
#install.packages("forecast")
library(forecast)
library(plyr)
library(ggplot2)

load(file='adopt.RData')

ggplot(data=adopt, aes(x=date))+
  geom_line(aes(y=cnt,color="A"))+
  geom_point(aes(y=cnt),color="red")+
  geom_line(aes(y=10*nt,color="B"))+
  geom_point(aes(y=10*nt),color="blue") +
  scale_y_continuous(sec.axis = sec_axis(~.*1/10,name="nt")) +
  scale_color_manual(labels = c("A", "B"), values = c("red", "blue")) +
  theme(legend.position = "right")+
  theme(legend.title = element_blank())

ggplot(data = adopt, aes(x=date, y=nt))+
  geom_line() + 
  geom_point()
ggplot(data = adopt, aes(x=date, y=cnt))+
  geom_line() + 
  geom_point()

#1b
date<-c("2006-06-01", "2006-07-01","2006-08-01","2006-09-01","2006-10-01",
        "2006-11-01","2006-12-01","2007-01-01","2007-02-01","2007-03-01",
        "2007-04-01","2007-05-01","2007-06-01","2007-07-01","2007-08-01",
        "2007-09-01","2007-10-01","2007-11-01","2007-12-01","2008-01-01",
        "2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01",
        "2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01",
        "2008-12-01")
nt<-c(0,1,2,0,0,2,4,2,3,7,9,4,1,1,6,17,31,21,29,64,36,50,40,36,19,19,39,31,29,
      38,14)
cnt<-c(0,0,1,3,3,3,5,9,11,14,21,30,34,35,36,42,59,90,111,140,204,240,290,330,
       366,385,404,443,474,503,541)
adopt1<-data.frame(date,nt,cnt)
adopt1
fit <- lm(f=nt~cnt+I(cnt^2),data=adopt1)
fit
summary(fit)

#1d
date2<-c("2009-01-01", "2009-02-01","2009-03-01","2009-04-01","2009-05-01",
        "2009-06-01","2009-07-01","2009-08-01","2009-09-01",
        "2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01")
nt2<-c(17,20,22,48,45,85,124,150,108,127,78,70,72,47)
cnt2<-c(555,572,592,614,662,707,792,916,1066,1174,1301,1379,1449,1521)
adopt2<-data.frame(date2,nt2,cnt2)
adopt2
differenceof2cnts <- (adopt2$cnt2-555)
differenceof2cnts
fit2 <- nls(f=nt~a*n+(b-a)*cnt-b/n*(cnt^2),
            start=list(a=0.00264277,b=0.277846, n=150),
            data = adopt2)
summary(fit2)

#r2
r2<-1-(sum(residuals(fit2)^2)/sum((fit2$m$lhs()-mean(fit1$m$lhs()))^2))
r2


