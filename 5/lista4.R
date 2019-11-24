



### ZAD 3 ###
# a)
library(mvtnorm)
sigma = matrix(c(1,0.9,0.9,1), 2,2)
X = rmvnorm(100,c(0,0),sigma/100)
X1 = X[,1]
X2 = X[,2]
eps = rnorm(100,0,1)
beta1 = 3
Y = beta1*X1+eps



# b)
m31 = lm(Y~X1)
e31=residuals(m31)
confint(m31)
summary(m31)$coefficients[2,1]

m32 = lm(Y~X1+X2)
e32=residuals(m31)
confint(m32)
summary(m32)
# c)
Y = cbind(rep(1,100), X1) %*% matrix(c(0,beta1),2,1)  + eps
Xprim = cbind(rep(1,100),X1)
H = Xprim %*% solve(matrix(t(Xprim) %*% Xprim ,2,2)) %*% t(Xprim)
sum(diag(H))
e = Y - H %*% Y
s1 = (t(e) %*% e) / ((100-2) * solve(matrix(t(Xprim) %*% Xprim ,2,2))[2,2])


Y = beta1*X1+eps
Xprim = cbind(rep(1,100), X1, X2)
H = Xprim %*% solve(matrix(t(Xprim) %*% Xprim ,3,3)) %*% t(Xprim)
sum(diag(H))
e = Y - H %*% Y
s2 = (t(e) %*% e) / (100-3) * solve(matrix(t(Xprim) %*% Xprim ,3,3))[2,2]

sqrt(s1)
sqrt(s2)
1-pf(qf(.95, 1, 98) , 1, 98, ncp=(3/sqrt(s1))^2)
1-pf(qf(.95, 2, 97) , 2, 97, ncp=(3/sqrt(s2))^2)


# d)
N=5000
p1 = rep(0,N)
p2 = rep(0,N)
beta11 = rep(0,N)
beta12 = rep(0,N)
for(i in 1:N){
  eps = rnorm(100,0,1)
  Y = beta1*X1+eps
  m10001 = lm(Y~X1)
  m10002 = lm(Y~X1+X2)
  beta11[i] = summary(m10001)$coefficients[2,1]
  beta12[i] = summary(m10002)$coefficients[2,1]
  
  p1[i] = summary(m10001)$coefficients[2,4]
  p2[i] = summary(m10002)$coefficients[2,4]
  
}
sd(beta11)
sd(beta12)
length(p1[p1<0.05]) / N * 100 #w procentach
length(p2[p2<0.05]) / N * 100 # w procentach

### ZAD 4 ###


X = matrix(rnorm(1000*950,0,1),1000,950)
eps = rnorm(1000)
beta = c(rep(3,5), rep(0,945))
Y = X %*% beta+eps

k=2
k
#a = summary(m10)$coefficients[1:k+1,1]
#a[order(a, decreasing=TRUE)[1:k]]
m10 = lm(Y~X[, order(a, decreasing=TRUE)[1:k]])


m10 = lm(Y~X[,1:k])
# suma reszta
sum((summary(m10)$residuals)^2)
# 
#anova(m10)[[2]][2]

# mean square error
sum((cbind(rep(1,1000),X[,1:k]) %*% m10$coefficients - X[,1:k] %*% (beta[1:k]))^2)
# MSE
sum((m10$fitted.values - X[,1:k] %*% beta[1:k])^2)
# AIC
AIC(m10)
# p-wartości
summary(m10)$coefficients[2:3,4]

# fałszywe
length(summary(m10)$coefficients[1:k+1,4][summary(m10)$coefficients[1:k+1,4] < 0.05]) -5

##szukamy najmniejszej wartosci AIC

## rozklad odwrotny Wischarta
## https://en.wikipedia.org/wiki/Inverse-Wishart_distribution


## ZAD 5  ##

data=read.table(url("http://math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/CH06PR15.txt"))
colnames(data) <- c("age", "severity", "anxiety", "satisfaction")
attach(data)
head(data)
m5 = lm(satisfaction~age+severity+anxiety)
summary(m5)
anova(m5)
str(m5)

## ZAD 6 ##

confint(lm(satisfaction~age))
summary(lm(satisfaction~age))

confint(lm(satisfaction~severity))
summary(lm(satisfaction~severity))

confint(lm(satisfaction~anxiety))
summary(lm(satisfaction~anxiety))


## zad 7, predoicted - fitted values - igrek z daszkiem. Powinny byc cztery wykresy

plot(m5$fitted.values, m5$residuals, ylab="reszty")
plot(age, m5$residuals, ylab="reszty", xlab="age")
plot(anxiety, m5$residuals, ylab="reszty", xlab="anxiety")
plot(satisfaction, m5$residuals, ylab="reszty", xlab="satisfaction")

## ZAD 8  ##
shapiro.test(m5$residuals)
qqnorm(m5$residuals)
qqline(m5$residuals, col = 3)
hist(m5$residuals)

## ZAD 9 ##

data2=read.table(url("http://math.uni.wroc.pl/~mbogdan/Modele_Liniowe/Dane/csdata.dat"))
colnames(data2) <- c("ID", "GPA", "HSM", "HSS", "HSE", "SATM", "SATV", "SEX")
attach(data2)
head(data2)

m91 = lm(GPA~HSM+HSS+HSE)
m92 = lm(GPA~SATM+SATV+HSM+HSS+HSE)
summary(m92)

## a)
SSE = sum(m91$residuals**2) - sum(m92$residuals**2)
df = m91$df - m92$df
MSE_F = sum(m92$residuals**2) / m92$df
F = (SSE/df)/MSE_F  ## - wzorek ze strony M.Bogdan, wykład 7
## b)
anova(m91,m92)

### ZAD 10 ###

m10 = lm(GPA~SATM+SATV+HSM+HSE+HSS)
# install.packages("car")
library(car)
anova(m10)
Anova(m10, type="II")
# a)
m101 = lm(GPA~SATM+SATV+HSM)
m102 = lm(GPA~SATM+SATV)
sum((m101$fitted.values-mean(m101$fitted.values))^2) - sum((m102$fitted.values-mean(m102$fitted.values))^2)
# b)
## tak są, dla HSS, z tego samego powodu, co w zadaniu 2 (ten znak zapytania)


### ZAD 11 ###
SAT = SATM + SATV
m11 = lm(GPA~SATM+SATV+SAT)
summary(m11)
anova(m11)

### ZAD 12 ###


m12 = lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX)
plot(m12)
avPlot(m12, SATM)
avPlot(m12, SATV)
avPlot(m12, HSM)
avPlot(m12, HSE)
avPlot(m12, HSS)
avPlot(m12, SEX)


### ZAD 13 ###
plot(rstudent(m12), ylim = c(-3.5, 3.5), xlab='Numer obserwacji', main='Studentyzowane reszty')
rstudent(m12)[abs(rstudent(m12))>3]
abline(3, 0, col='red')
abline(-3, 0, col='red')

### ZAD 14 ###
plot(dffits(m12), ylab='DFFITS', xlab='Numer obserwacji', main='DFFITS')
abline(2*sqrt(p/n), 0, col='red')
abline(-2*sqrt(p/n), 0, col='red')
abline(2*sqrt((p+1)/(n-p-1)), 0, col='green')
abline(-2*sqrt((p+1)/(n-p-1)), 0, col='green')
p = sum(hatvalues(m12)) ##liczba parametrów (liczba bet) lub suma wartości na przekątnej HAT matrix
n = length(m12$fitted.values) ##liczba obserwacji

dffits(m12)[abs(dffits(m12)) > 2*sqrt(p/n)]
dffits(m12)[abs(dffits(m12)) > 2*sqrt((p+1)/(n-p-1))]

### ZAD 15 ###
library('car')
1/vif(m12)

### ZAD 16 ###

step(lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX), direction="both")

logn = log(sum(lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX)$fitted.values))
step(lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX), direction="both", k=logn)
#step(lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX), direction="forward")
#step(lm(GPA~SATM+SATV+HSM+HSE+HSS+SEX), direction="both")





X = matrix(rnorm(1000*950,0,1),1000,950)
eps = rnorm(1000)
beta = c(rep(3,5), rep(0,945))
Y = X %*% beta+eps
k=950
k
#a = summary(m10)$coefficients[1:k+1,1]
#a[order(a, decreasing=TRUE)[1:k]]
X = X[, order(a, decreasing=TRUE)[1:k]]

m10 = lm(Y~X)
# suma reszta
sum((summary(m10)$residuals)^2)

# MSE
sum((m10$fitted.values - X %*% beta[1:k])^2)
# AIC
AIC(m10)
# p-wartości
summary(m10)$coefficients[2:3,4]


# fałszywe
length(summary(m10)$coefficients[1:k+1,4][summary(m10)$coefficients[1:k+1,4] < 0.05]) -5

















