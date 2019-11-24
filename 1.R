
library(mvtnorm)

##### ZAD 1 #####


n=100
X = rmvnorm(n, mean=c(0,0), sigma=matrix(c(1,0,0,1), 2, 2) )
plot(X, pch=46)

X = matrix(0,100,2)
for (i in 1:100){
  X[i,] = rnorm(2)
}
plot(X, main='Rozkład')


##### ZAD 2 #####

S1 = matrix(c(1, 0.9, 0.9, 1), 2, 2)
S2 = matrix(c(1, -0.9, -0.9, 1), 2, 2)
S3 = matrix(c(9, 0, 0 ,1), 2, 2)

sqrt(0.19)
A1 = t(chol(S1))
A2 = t(chol(S2))
A3 = t(chol(S3))

mu = c(4,2)

Y = t(A1%*%t(X)+mu)
plot(Y)
plot(rmvnorm(n, mean=mu, sigma=S1 ))

Y = t(A2%*%t(X)+mu)
plot(Y)
plot(rmvnorm(n, mean=mu, sigma=S2 ))

Y = t(A3%*%t(X)+mu)
plot(Y)
plot(rmvnorm(n, mean=mu, sigma=S3 ))


##### ZAD 3 #####

my_rmvnorm=function(mu,Sigma){
  r = length(mu)
  L = t(chol(Sigma)) 
  Z = rnorm(r)
  return(L %*% Z + mu)
}

n = 200
m = 100

Sigma = diag(m)
mu = rep(0,m)

X = matrix(0,m,n)
for(i in 1:n){
 X[,i] = my_rmvnorm(mu, Sigma)
}
X
X = t(X)

x = rep(0,m)
for(i in 1:m){
  x[i] = mean(X[,i])
}
x
mean(x)
mean(X)

sigma = matrix(0.9,m,m) + 0.1*diag(m)

A = t(chol(sigma))
Y = A%*%t(X)

Y
plot(t(Y))

hist(var(Y), main='Histogram próbkowych wariancji')


a = matrix(0, 100, 100)
for (i in 1:100){
  for (j in 1:100){
    a[i,j] = cov(Y[i,], Y[j,])
  }
}
a
hist(a, main='Kowariancja próbkowa')


