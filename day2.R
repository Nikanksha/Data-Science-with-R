datasets::airquality
airquality<-datasets::airquality
head(airquality)
tail(airquality)
airquality[,c(1,2)]
airquality$Ozone
summary(airquality$Temp)
summary(airquality)
plot(airquality$Ozone)
plot(airquality$Ozone,airquality$Temp)
plot(airquality)
plot(airquality$Ozone,type = "l")#p:points,1:lines,b:both

#plot
plot(airquality$Ozone,xlab = 'ozone Concentration',
     ylab = 'no of instance',main = 'ozone level in NY city',
     col = 'blue')
#horizontal bar plot
barplot(airquality$Ozone, main = 'ozone level in NY city', xlab = 'ozone levels',
    col = 'blue',horiz = FALSE)

#Histogram
hist(airquality$Solar.R)
hist(airquality$Solar.R, main = 'solar Radiation values in air', 
     xlab = 'solar red',
        col = 'blue',)

#single box plot
boxplot(airquality$Solar.R)

#multiple box plot
boxplot(airquality[1:4],main='multiple')
mean = 60
std = 10
x = 70
pnorm(x,mean,std)
#to calculte left area of curve
pnorm(680,711,29)
1-pnorm(750,711,29) 
sqrt(140)

1990-(1.960)*(2500/11.83)
1990+(1.960)*(2500/11.83)
qnorm(0.975)
1900+1.98*(2833/ 11.83)     
1900-1.98*(2833/ 11.83)
n = 15
me = 28.06
std = 5.84
xbar+/- t(1-alpha,df) * std/sqrt(n)
28.06+((2.14)*5.84/sqrt(15))
28.06-((2.14)*5.84/sqrt(15))
qt(0.975,)

#for departure delay calculate confrence interval
library(gmodels)
#ci(sample)
install.packages('nycflights13')
data<-nycflights13::flights
dep_delay<-data$dep_delay
ar_delay<-data$arr_delay
dep_delay1<-dep_delay[!is.na(dep_delay)]

#ar_delay1<-ar_delay[!is.na(ar_delay)]
ci(dep_delay1)
#ci(ar_delay1)
