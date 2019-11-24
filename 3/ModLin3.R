## ZAD 1 ##
X=runif(100)
Y=1+.3*X+rnorm(100)

summary(lm(Y~X))
abs(summary(lm(Y~X))$coefficients[2,3]^2-
summary(lm(Y~X))$fstatistic[1])

qf(0.05,1,20)

## ZAD 3 ##

# a
t=read.table(url("http://www.math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/table1_6.TXT"))
head(t)
attach(t)
summary(lm(V2~V3))
summary(lm(V2~V3))$r.squared
plot(V2~V3)
abline(lm(V2~V3), col='red')

# b
predict(lm(V2~V3),data.frame(V3=100),interval="prediction",level=0.9)

# c
matplot(min(t$V3):max(t$V3),predict(lm(V2~V3),data.frame(V3=min(t$V3):max(t$V3)),interval="prediction"),type="l",lty=2, main="Przedziały prognozy", xlab="IQ", ylab="Srednia ocen")
points(V3,V2)

max(t$V5)
min(t$V5)

## ZAD 4 ##

# a i b
summary(lm(V2~V5))
summary(lm(V2~V5))$r.squared

# c
predict(lm(V2~V5),data.frame(V5=60),interval="prediction",level=.9)

# d
matplot(min(t$V5):max(t$V5),predict(lm(V2~V5),data.frame(V5=min(t$V5):max(t$V5)),interval="prediction"),type="l",lty=2, main="Przedziały prognozy", xlab="Wyniki testu Piersa-Harrisa", ylab="Srednia ocen")
points(V5,V2)

# e
summary(lm(V2~V3))
summary(lm(V2~V5))


## ZAD 5 ##
t=read.table(url("http://www.math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/CH01PR20.txt"))
head(t)
attach(t)
plot(t[2:1])
m1=lm(V1~V2)
abline(m1,col="red")
str(summary(m1))

# a
summary(m1)$residuals
residuals(m1)
V1-m1$fitted.values
r=V1-m1$coefficients[1]-m1$coefficients[2]*V2
sum(r)
sum(summary(lm(V1~V2))$residuals)

# b
plot(V2,r, ylab="reszta")
# c
plot(r)
# d
h <- hist(r, main= "Histogram") 
xfit <- seq(min(r), max(r), length = 40) 
yfit <- dnorm(xfit, mean = mean(r), sd = sd(r)) 
yfit <- yfit * diff(h$mids[1:2]) * length(r) 

lines(xfit, yfit, col = "black", lwd = 2)

qqnorm(r)


## ZAD 6 ##

head(t)

s=t
s[1,1]=2000
heas(s)

# a
f=function(m) {v=c(m$coefficients,
summary(m)$coefficients[2,4],
summary(m)$r.squared,
summary(m)$sigma^2); names(v)=c("b0","b1","p","R2","sigma2"); v}

f(m1)

plot(s[2:1],ylim=c(-20,200))
m2=lm(s$V1~s$V2)
abline(m1,col="red")

f(m2)

View(rbind(f(m1),f(m2)))

plot(V2,residuals(m2))
identify(V2,s$V1)

# b
r2 = summary(m2)$residuals
plot(V2,r2)
plot(r2)

h <- hist(r2, xlab = "Accuracy") 
xfit <- seq(-500, max(r2), length = 40) 
yfit <- dnorm(xfit, mean = mean(r), sd = sd(r2)) 
yfit <- yfit * diff(h$mids[1:2]) * length(r2) 

lines(xfit, yfit, col = "black", lwd = 2)

qqnorm(r2)

## ZAD 7 ##

u=read.table(url("http://www.math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/CH03PR15.txt"))
head(u)

detach(t)

detach(u)
attach(u)
m3=lm(V1~V2)
summary(m3)

plot(V1~V2, xlab = "czas", ylab="stężenie roztworu")
abline(m3,col="black" )

## ZAD 8 ##

p=predict(m3,data.frame(V2=seq(0,10,.2)),interval="prediction")
matplot(seq(0,10,.2),p,add=TRUE,type="l")

cor(V1,predict(m3))
## ZAD 9 ##

library(MASS)
boxcox(m3)


## ZAD 10 ##
logy=log(V1)
m4=lm(logy~V2)
summary(m4)
plot(logy~V2, xlab="czas")
abline(m4,col="blue")

p=predict(m4,data.frame(V2=seq(0,10,.2)),interval="prediction")
matplot(seq(0,10,.2),p,add=TRUE,type="l")
cor(logy,predict(m4))


## ZAD 11 ##
plot(V1~V2, xlab="czas")
p=predict(m4,data.frame(V2=seq(0,10,.2)),interval="prediction")
matplot(seq(0,10,.2),exp(p),add=TRUE,type="l")
cor(V1,predict(m4))


## ZAD 12 ##
t1 = V2^(-1/2)
m5 = lm(V1~t1)
plot(V1~t1)
summary(m5)
abline(m5,col="black")

p=predict(m5,data.frame(t1=seq(0,1.2,.005)),interval="prediction")
matplot(seq(0,1.2,.005),p,add=TRUE,type="l")
cor(V1,predict(m5))



1-pchisq(2.09, 12)

chisq.test(c(1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0))
13*124/207


