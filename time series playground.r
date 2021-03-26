# Time series

data=read.csv(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/varicelle.csv") 
plot(data$x)
# We indicate to R the specificity of the timeseries (ts) object 
varicelle<-ts(data$x,start=c(1931,1),end=c(1972,6),freq=12) 
plot(varicelle)


install.packages("forecast")
install.packages("ggplot2")
install.packages("ffp")
install.packages("ffp2")
library(forecast) 
library(ggplot2) 
autoplot(varicelle) +
  ggtitle('Number of varicella per months')+ xlab('year')+
  ylab('Number of varicella')

ggseasonplot(varicelle,year.labels= TRUE,year.labels.left=TRUE)

ggseasonplot(varicelle,polar=TRUE)

mean(varicelle)
var(varicelle)

tmp=acf(varicelle,type="cov",plot = FALSE) 
tmp$acf[1:3,1,1]

plot(tmp)


tmp=acf(varicelle,type="cor",plot = FALSE) 
tmp$acf[1:3,1,1]

plot(tmp)

serie=2*(1:100)+4 
par(mfrow=c(1,2)) 
plot(ts(serie)) 
acf(serie)


serie=cos(2*pi/12*(1:100))
par(mfrow=c(1,2)) 
plot(ts(serie)) 
acf(serie)


tmp=pacf(varicelle,type="cor",plot = FALSE) 
tmp$acf[1:3,1,1]

plot(tmp)

x=rep(0,41)
for (i in 0:40) 
  x[i+1]<-sum(varicelle[(1+12*i):(12*(i+1))]) 
plot(x,type='l',xaxt='n',xlab='')
axis(1,at = 0:40,labels = 1931:1971)


#ex2
plot.ts(co2)

sanfran <-read.csv(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/sanfran.dat", skip=1) 
View(sanfran)
sanfran <- scan(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/sanfran.dat", skip=1)

plot(sanfran)

plot(sanfran,type='l',xlim=c(1,120),ylim=c(1,80),xlab='time',ylab='')

data("AirPassengers")
autoplot(AirPassengers, series="Data") + 
  autolayer(ma(AirPassengers,6), series="6-MA") + 
  autolayer(ma(AirPassengers,12), series="12-MA") + 
  xlab("Year") + 
  ylab("AirPassengers concentration") + 
  ggtitle("AirPassengers") + 
  scale_colour_manual(values=c("Data"="grey50","6-MA"="red","12-MA"="blue"), breaks=c("Data","6-MA","12-MA"))

autoplot(decompose(AirPassengers,type="additive"))+ xlab('Year')

autoplot(decompose(AirPassengers,type="multiplicative"))+ xlab('Year')

# we can see that there is a dependancy in residuals (trend)

tmp = decompose(co2)
plot(tmp)

# how to know there is a correlation between residuals? let's see autocorrelation

acf(tmp$random[-which(is.na(tmp$random))])

#there is a correlation in residuals 

pacf(tmp$random[-which(is.na(tmp$random))])

#still we can see there is autocorrelation if order 1, 3,4,5 etc 


air = decompose(AirPassengers)
plot(air)
acf(air$random[-which(is.na(air$random))])
pacf(air$random[-which(is.na(air$random))])
#there is autocorrelation in residuals too

a= decompose(AirPassengers, type='multi')

acf(a$random[-which(is.na(a$random))])
pacf(a$random[-which(is.na(a$random))])


data("AirPassengers")

par(mfrow=c(2,1))
plot(AirPassengers) 
plot(diff(AirPassengers,differences=1))

par(mfrow=c(2,1))
plot(AirPassengers) 
plot(diff(AirPassengers,differences=2))


par(mfrow=c(2,1))
plot(AirPassengers) 
plot(diff(AirPassengers,lag=12,differences=1))


par(mfrow=c(3,1))
plot(AirPassengers) 
plot(diff(AirPassengers,lag=12,differences=1)) 
plot(diff(diff(AirPassengers,lag=12)))

Box.test(diff(AirPassengers,lag=12,differences=1),lag=10,type="Ljung-Box")

install('fpp2')
library(fpp2)
plot(goog200)


#simulate an AR(p) AutoRegression of order p
par(mfrow=c(3,1)) 
model<-list(ar=c(0.8)) 
ar1<-arima.sim(model,5000) 
plot.ts(ar1)
acf(ar1) 
pacf(ar1)


#simulate an MAq. Observe the auto-correlations (partial or not)
modele<-list(ma=c(2)) 
ma1<-arima.sim(modele,1000) 
plot.ts(ma1)
acf(ma1)
pacf(ma1)




autoplot(uschange[,c("Income","Unemployment")])

Arima(uschange[,"Income"],order=c(2,0,2))  
Arima(uschange[,"Unemployment"],order=c(2,0,2))

auto.arima(uschange[,"Income"])
auto.arima(uschange[,"Unemployment"])

autoplot(uschange[,"Income"]) +
  xlab("Year") + ylab("Quarterly percentage change")

ggAcf(uschange[,"Income"])
ggPacf(uschange[,"Income"])

Arima(uschange[,"Income"],order=c(0,0,0))

Arima(uschange[,"Income"],order=c(0,0,3))

Arima(uschange[,"Unemployment"],order=c(2,0,0))
Arima(uschange[,"Unemployment"],order=c(1,0,2))


fit=Arima(uschange[,"Income"],order=c(0,0,0)) 
autoplot(forecast(fit,h=10))

fit=Arima(uschange[,"Unemployment"],order=c(1,0,2)) 
autoplot(forecast(fit,h=10))
