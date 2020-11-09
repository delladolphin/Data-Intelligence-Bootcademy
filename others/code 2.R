#Q2a
library(forecast)
library(ggplot2)
room<-read.csv(file="room.csv",header=T)
room
room$occupy <- room$Number_hotel_rooms*room$rate
room
#ts create time series
ts.room <- ts(data=room[1:46,"occupy"],frequency = 12, start = c(2015,3))
ts.room

#2b
#Holt_s_linear exponential 
fit2 <- holt(y=room$occupy, h=6, initail="simple",
             beta=NULL)
summary(fit2)
sse <- sum(residuals(fit2)^2)
r2 <-1-sse/sum((fit2$x-mean(fit2$x))^2)
r2


#Holt-Winters additive model 
fit3 <-hw(y=room$occupy, seasonal="add",h=24,initial="simple")
summary(fit3)
#R2
sse <- sum(residuals(fit3)^2)
r2 <-1-sse/sum((fit3$x-mean(fit3$x))^2)
r2

#Holt_winter_multiplicative_model
fit4 <-hw(y=room$occupy, seasonal="mul",h=24,initial="simple")
summary(fit4)
#R2
sse <- sum(residuals(fit4)^2)
r2 <-1-sse/sum((fit4$x-mean(fit4$x))^2)
r2

# 2c 
#i) forecast value from January 2019 to December 2019
forecast(fit4,h=12)
forecast(fit4,h=12)$mean
# 2c ii) plot the  forecast values
plot(forecast(fit4,h=12)$mean)

#2di)
#Holt-Winters additive model 
#trend effect
plot(fit3)
#seasonal effect
plot(decompose(ts.room, type = "additive"))

