
## ZAD 1 ##
alfa = 0.05
df = 10
tc = qt(1-alfa/2, df)
Fc = qf(1-alfa, 1, df)
tc^2
Fc
abs(tc^2-Fc)

## ZAD 1 INACZEJ ##

X=runif(100)
Y=1+.3*X+rnorm(100)

summary(lm(Y~X))
abs(summary(lm(Y~X))$coefficients[2,3]^2-
      summary(lm(Y~X))$fstatistic[1])

## ZAD 3 ##


t = read.table(url("http://www.math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/CH01PR20.txt"))
#names(t) <- c("Y", "X")
attach(t)

m = lm(V1~V2)
plot(V2, V1)
abline(m, col='red')
#reszty
summary(m)$residuals
r = residuals(m)
sum(r)

plot(V2, r)
plot(r)
hist(r)
qqnorm(r)

s = t
s[1,1] = 2000

f = function (m){c(
  m$coefficients,
  summary(m)$coefficients[2,4],
  summary(m)$r.squared,
  summary(m)$sigma^2)
}

f(m)
identify(s$V2, s$V1)
plot(s$V2, s$V1, ylim = c(-20, 200))
m2 = lm(s$V1~s$V2)
abline(m2, col='red')
f(m2)
rbind(f(m), f(m2))




u=read.table(url("http://www.math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/CH03PR15.txt"))
head(u)
m3 = lm(u$V1~u$V2)
plot(m3)
plot(u$V1~u$V2)

# zad 8
predict(m3)

library(MASS)
boxcox(m3)
  








sigma2 = 10
n = 10000
m=10000
sr = rep(0, m)
for(i in 1:m){
  sr[i] = sum(rnorm(n, 0, sigma2))/n
  
}
mean(sr)
abs(var(sr) - sigma2/n)






































