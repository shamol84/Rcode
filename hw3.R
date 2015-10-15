### EPBI hw 3

laplace11 = function(mu, s){
  u = runif(100,0,1)
  return(mu + s*log(2*u))
}

laplace12 = function(mu, s){
  u = runif(100)
  return(mu + s*log(2*(1-u)))
}

library("distr", lib.loc="H:/Documents/R")
?DExp
D <- DExp(rate = 1
)
r(D)(100)
a=r(D)(100)
ks.test(x,a)

## 
?ks.test
## critical value = 1.36/10

## D must be less than critical value

D=0.26 reject null 

####bessel-vonmises
## g(x) = exp(k*cos(theta))/besselI(k,k, expon.scaled = FALSE)
c=0.95
theta = runif(100,0,2*pi)
k=0.1
y = exp(k*cos(theta))/besselI(k,k, expon.scaled = FALSE)
z= (pi^0.5)*exp(k*cos(theta))/besselI(k,k, expon.scaled = FALSE)
b=runif(1,0,1)
y/z/c
plot(theta,y)

k1=0.2
y1 = exp(k1*cos(theta))/besselI(k1,k1, expon.scaled = FALSE)
plot(theta,y1)

k2=0.3
y2 = exp(k2*cos(theta))/besselI(k2,k2, expon.scaled = FALSE)
plot(theta,y2)

k3=0.4
y3 = exp(k3*cos(theta))/besselI(k3,k3, expon.scaled = FALSE)
plot(theta,y3)


plot(theta,y,ylim=range(c(1.0,3)),ylab="")
par(new=TRUE)
plot(theta,y1,ylim=range(c(1.0,3)),pch=16,ylab="")
par(new=TRUE)
plot(theta,y2,ylim=range(c(1.0,3)),pch=3,ylab="")
par(new=TRUE)
plot(theta,y3,ylim=range(c(1.0,3)),pch=5,ylab="f_k(theta)")
legend('topright',c("k=0.1","k=0.2","k=0.3","k=0.4"),pch=c(1,16,3,5))


