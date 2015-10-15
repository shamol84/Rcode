pball=function(d,p){        # set the function
  u=runif(d+1)     
  z=qgamma(u[-1],1/p)
  y=z/sum(z)
  x=(u[1]*y)^(1/p)
  x}


pballm=function(d,p,m=10){
  x=x2=x3=x4=matrix(0,m,d)           # initialize a m by d matrix with zeros
  for (i in 1:m) {
    u=runif(d+1)
    z=qgamma(u[-1],1/p)
    y=z/sum(z)
    x[i,]=(u[1]*y)^(1/p)
  }
  for (i in 1:m) {
    u=runif(d+1)
    z=qgamma(u[-1],1/p)
    y=z/sum(z)
    x2[i,]=(u[1]*y)^(1/p)
  }
  for (i in 1:m) {
    u=runif(d+1)
    z=qgamma(u[-1],1/p)
    y=z/sum(z)
    x3[i,]=(u[1]*y)^(1/p)
  }
  for (i in 1:m) {
    u=runif(d+1)
    z=qgamma(u[-1],1/p)
    y=z/sum(z)
    x4[i,]=(u[1]*y)^(1/p)
  }
  x2[,1]=-x2[,1]
  x3=-x3
  x4[,2]=-x4[,2]
  d=list(x,x2,x3,x4)
  d=do.call(rbind,d)}



pball2=function(d,p,m){
  mat=list()
  for (j in 1:length(p)){
    x=x2=x3=x4=matrix(0,m,d)
    
#  x=matrix(0,m,d)           # initialize a m by d matrix with zeros
  for (i in 1:m) {
    u=runif(d+1)
    z=qgamma(u[-1],1/p[j])
    y=z/sum(z)
    x[i,]=(u[1]*y)^(1/p[j])
  }
for (i in 1:m) {
  u=runif(d+1)
  z=qgamma(u[-1],1/p[j])
  y=z/sum(z)
  x2[i,]=(u[1]*y)^(1/p[j])
}
for (i in 1:m) {
  u=runif(d+1)
  z=qgamma(u[-1],1/p[j])
  y=z/sum(z)
  x3[i,]=(u[1]*y)^(1/p[j])
}
for (i in 1:m) {
  u=runif(d+1)
  z=qgamma(u[-1],1/p[j])
  y=z/sum(z)
  x4[i,]=(u[1]*y)^(1/p[j])
}
x=x[order(x[,1],x[,2],decreasing=TRUE),]
x2=x2[order(x2[,1],x2[,2],decreasing=TRUE),]
x3=x3[order(x3[,1],x3[,2],decreasing=TRUE),]
x4=x4[order(x4[,1],x4[,2],decreasing=TRUE),]
  x2[,1]=-x2[,1]
  x3=-x3
  x4[,2]=-x4[,2]
  e=list(x,x2,x3,x4)
  e=do.call(rbind,e)
  mat[[j]]=e
  }

  mat}






buffon <- function(n, L=1, D=2){
  x <- D*runif(n)/2
  y <- L*sin(pi*runif(n)/2)/2
  length(x[x<y])/n
}

# Plot Buffon's Needle Estimates

buffplot <- function(N=10000, L=1, D=2){
  n <- seq(100, N, 100)
  u1 <- runif(N)
  u2 <- runif(N)
  x <- D*u1/2
  y <- L*sin(pi*u2/2)/2
  z <- n
  for (i in 1:length(n))
    z[i] <- length(x[1:n[i]][x[1:n[i]]<y[1:n[i]]])/n[i]
  r <- 2*L/D
  plot(n, r/z, xlab="N", ylab="Est(pi)")
  abline(h=pi)
}