EPI_data <- read.csv("C:\\Users\\kiernm\\Downloads\\epi2024results06022024.csv") 
View(EPI_data)
attach(EPI_data)
NAs<-is.na(EPI.new)
EPI.new.noNAs<-EPI.new[!NAs]
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new,seq(20.,80.,1.0),prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
boxplot(EPI.new,APO.new)
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)
x<-seq(20,80,1)
q<-dnorm(x,mean=42,sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65,sd=5,log=FALSE)
lines(x,.12*q)

#exercise two
#given variable
plot(ecdf(EPI.new),do.points=FALSE,verticals=TRUE)
qqnorm(EPI.new);qqline(EPI.new)
qqplot(rnorm(250),EPI.new,xlab="Q-Q plot for norm dsn")
qqline(EPI.new)
qqplot(rt(250,df=5),EPI.new,xlab="Q-Q plot for t dsn")
qqline(EPI.new)

#variable APO.new

plot(ecdf(APO.new),do.points=FALSE,verticals=TRUE)
qqnorm(APO.new);qqline(APO.new)
qqplot(rnorm(250),EPI.new,xlab="Q-Q plot for norm dsn")
qqline(APO.new)
qqplot(rt(250,df=5),APO.new,xlab="Q-Q plot for t dsn")
qqline(APO.new)

#variable SPI.new

plot(ecdf(SPI.new),do.points=FALSE,verticals=TRUE)
qqnorm(SPI.new);qqline(SPI.new)
qqplot(rnorm(250),EPI.new,xlab="Q-Q plot for norm dsn")
qqline(SPI.new)
qqplot(rt(250,df=5),SPI.new,xlab="Q-Q plot for t dsn")
qqline(SPI.new)

