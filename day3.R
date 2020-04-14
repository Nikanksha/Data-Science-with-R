pt(2.23,79)
sqrt(80)
pt(0.225,79)
sqrt(50)
.2*7/3
pt(-.46,49)
1-.67
pt(0,49)
2*.32
x<-c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)
t.test(x,alternative = "greater",mu=0.3)
control<-c(91,87,99,)
treat=c(101,110,103,93,99,104)

#linear Regration
model<-lm(sunday-daily,data = NewspaperData)
Summary(model)
sun=13.84 + (1.34*200)
sun

NewspaperData