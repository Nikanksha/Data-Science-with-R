xbar=60
std=10
x=70
pnorm(70,xbar,std)
1+1
2+3*4
3^2
exp(1)
sqrt(10)
pi
2*pi*6378
x <- 2
y<- 5
x*y
dbl_var<-c(1,2.5,4.5)
vect<-c(2,1,5,4)
x<-c(2,0,0,4)
y<-c(1,9,9,9)
x+y
x*4
sqrt(x)
vect[1]
vect[4]
vect[-1]
vect[1]
x<-c(2,0,0,4)
x[1]
x[1]<-3;x
x[-1]<-5;x
y<9
y[4]=1
y<9
y[y<9]= 2
y
df <- data.frame(x = 1:3, y = c("a","b","c"))
df
data.frame(height=c(150,60),weight=c(65,72))
df[1,2]
df[2,c(1,2)]# second row

df[c(1,3),c(1,2)]#1st row and 3rd row
df[c(1,3), c(1,0)]# 1 and 3

data("airquality")
airquality <- datasets::airquality
head(airquality)
tail(airquality)
airquality[,c(1,2)]
airquality$Wind
summary(airquality$Temp)
summary(airquality)
