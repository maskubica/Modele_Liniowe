dane = read.table("/home/michal/Dokumenty/Studia/UWr/5/Modele_Liniowe/CH01TA01.txt")
names(dane) <- c("time", "copiers")
plot(dane$time)
plot(dane$copiers)
head(dane)
dane
plot(dane$copiers,dane$time)

m1 = lm(dane$time~dane$copiers)
str(m1)

abline(m1, col='black') 

confint(m1, level=0.95)[2,]
summary(m1)$coefficients[2,3]
summary(m1)$df[2]
summary(m1)$coefficients[2,4]

x=11
x*confint(m1, level=0.95)[2,1]+confint(m1, level=0.95)[1,1]
x*0.2301084-1.8582511
x*confint(m1, level=0.95)[2,2]+confint(m1, level=0.95)[1,2]
abline(-1.8582511,0.2301084, col='red')

(confint(m1, level=0.95)[2,1] + confint(m1, level=0.95)[2,2])/2 - 
(confint(m1, level=0.95)[1,1] + confint(m1, level=0.95)[1,2])/2


b1 = (sum((dane$copiers-mean(dane$copiers))*(dane$time - mean(dane$time) ) )) / sum( (dane$copiers-mean(dane$copiers))^2 )
b0 = mean(dane$time) - b1*mean(dane$copiers)

Yi = b0+b1*dane$copiers
s = sqrt( sum( (dane$time-Yi)^2  ) / 23 )

est_odch = s^2*(1+ 1/25 + (x-mean(dane$copiers))^2 / sum( (dane$copiers-mean(dane$copiers))^2 ))
b0+b1*x - 2.069*sqrt(est_odch)
b0+b1*x + 2.069*sqrt(est_odch)


14.85372



p1 = predict(m1,data.frame(x=1:25),interval="confidence") #prognoza
p1
hp = data.frame(dane$copiers, p1[,1])
hp = hp[order(hp),]

lines(hp[,1],hp[,2], col='black')


hp = data.frame(dane$copiers, p1[,2])
hp = hp[order(hp),]

lines(hp[,1],hp[,2], col='red')


hp = data.frame(dane$copiers, p1[,3])
hp = hp[order(hp),]

lines(hp[,1],hp[,2], col='red')

# ZAD 3 - o co cho

  #przedzial ufnosci dla wartosci oczekiwanej Y gdy X=11

# ZAD 4 - o co cho

  #przedzial (prognozy), ze zmienna losowa z tego rozklad bedzie z
  #zadanym pstwem bedzie nalezala do tego rozkladu


# ZAD 5 - przedzial prognozy dla poszczegolnych X-ów

p = predict(m1, data.frame(time=100:550), interval = 'confidence')

plot(dane$copiers,p[,1])
points(dane$copiers, p[,1], add=TRUE, col='red')
points(dane$copiers, p[,2], add=TRUE, col='black')
points(dane$copiers, p[,3], add=TRUE, col='black')



p2 = predict(m1, data.frame(time=time), interval = "confidence")

plot(dane$copiers,dane$time)
arrows(dane$copiers, p[,2], dane$copiers, p[,3], angle=90, code=3)

#obczaj funkcje arrows (rysuje strzalki)
plot(-1:1)
arrows(2,0, 2,0.5, angle=90,code=3)


t=seq(-10,10,0.0001)
plot(t,1/(t^2+1), pch=46)
## ZAD 7

##uzyc apply albo sapply
#epsilonami wypelnic cala macierz
#do kazdej kolumny dodaj X*beta+5
#funkcje dzialajacej na kolumnach
#i wtedy by szybciej dzialaja
#wrzucic to wszystko w jakas funkcje i baaang xD
#we wnioskach zastanowic sie czy jak odejdziemy od zalozenia od normalnosci to co sie dzieje z bledem 1 i 2 rodzaju baaaang xddd
#jak abrdzo blad 1,2 rodzaju zachowuja sie w sytuacji kiedy odejdziemy od zalozenia od normalnosci 
#funkcje mozna podac jako argument i to mozna wykorzystac do napisania tylko jednej funkcji
#?eval - nazwe rozkladu uczynimy parametrem

beta = 0
alfa=0.05
wyniki = numeric(1000)

X = rnorm(200, 0, 1/200)
#X = rexp(200, 1)
for (i in 1:1000) {
  eps = rnorm(200,0,1)
  Y = 5+beta*X+eps
  wyniki[i] <- (summary(lm(Y~X))$coefficients[2,4] < alfa)
}
mean(wyniki)
plot(wyniki)

dim = 200
n=1000
beta1=0
alfa=0.05

fssx<-function(A){
  sum = 0
  sr = colMeans(A)
  for( i in 1:dim){
    sum = sum + sum((A[i,] - sr)^2)
  }
  
  return(sum)
}


f<-function(dim, n, beta1, dist)
{
  X = rnorm(dim, 0, 1/dim)
  if( dist == 1) {
    model = matrix(rnorm(dim*n, 0, 1), dim, n)
    } else 
    {
      model = matrix(rexp(dim*n, 1), dim, n)
    }
  
  model=apply(model, 2, function(eps){beta1*X+5+eps})
  model=apply(model, 2, function(Y){(summary(lm(Y~X))$coefficients[2,4] < alfa)})


  
  return(sum(model)/n)
}

f(200, 1000, 0, 1)
f(200, 1000, 0, 2)
f(200, 1000, 1.5, 1)
f(200, 1000, 1.5, 2)










## ZAD 6

n=40
sig2=120 
ssx=1000
alpha=0.05

sig2b1=sig2/ssx;
tc=qt(1-alpha/2, n-2)
beta1=1
delta=beta1/sqrt(sig2b1)
1-pt(tc,n-2,delta)+pt(-tc,n-2,delta)


beta1=seq(-2, 2, 0.01)
delta=beta1/sqrt(sig2b1)
moc=1-pt(tc,n-2,delta)+pt(-tc,n-2,delta)
plot(beta1,moc, main="Moc testu", type='l')



qt(1-0.05/2,18)





d = matrix(c(2,3,4,5,6,7,8,9,10,11, 1000, 800,200,80,40,25,20,10,7,5), 10,2)
plot(d)
m=nls(d[,2]~a/d[,1]+b, start=list(a=1,b=1))
lines(d[,1],predict(m),col='red')

