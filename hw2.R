cnt=0;
cnt1=0;
pi=3.1416;
D=4
L=1
n=0
a=rep(0,6)
b=rep(0,6)
m=c(50,200,600,2000,4500,7500)
p=rep(1,6)
pi1=rep(1,6)
for (j in 1:length(m)){
  if (j>1){n=m[j]-m[j-1]}
  else {n=m[j]}
for (i in 1:n){
x=runif(1)*(pi/2);
y=runif(1)*D;
if (y <= (L*sin(x))){
cnt=cnt+1;
}
}
#n=m[j]
a[j]=cnt
b[j]=n
cnt=cnt
p[j]=cnt/m[j]
pi1[j]=2*0.25/p[j]
#cnt1=cnt
}


p
pi1


p=cnt/n;
2*0.25/p
cnt

cnt=0;
pi=3.1416;
D=4
L=1
for (i in 1:3000){
  x=runif(1)*(pi/2);
  y=runif(1)*D;
  if (y <= (L*sin(x))){
    cnt=cnt+1;
  }
}
p=cnt/4000;
2*0.25/p
cnt

cnt=0;
pi=3.1416;
D=4
L=1
for (i in 1:2000){
  x=runif(1)*(pi/2);
  y=runif(1)*D;
  if (y <= (L*sin(x))){
    cnt=cnt+1;
  }
}
p=cnt/1000;
2*0.25/p
cnt

## ibm RANDU
a1= 65539
c1=3
x0=1
m=2**31

##
xn=rep(0,20002)
for ( i in 1:20002){
  xn[i]=(a*x0+c)%%m
  x0=xn[i]
}

##
xn=rep(0,1002)
for ( i in 1:1002){
  xn[i]=(a*x0+c)%%m
  x0=xn[i]
}

seed <- as.double(1)
RANDU = function() {
  seed <<- ((2^16 + 3) * seed) %% (2^31)
  #seed/2^31
}

x = rep(0,1002)
for(i in 1:1002) {
  x[i] <- c(RANDU())
  
}


y = rep(0,20002)
for(i in 1:20002) {
  y[i] <- c(RANDU())
  
}

library(rgl)
plot3d(y[seq(1,20002,by=3)],y[seq(2,20002,by=3)], y[seq(3,20002,by=3)], xlab="",ylab="",zlab="")
plot3d(x[seq(1,1002,by=3)],x[seq(2,1002,by=3)], y[seq(3,1002,by=3)], xlab="",ylab="",zlab="")


###########
## maximal t-test ##
##########

# divide 20002 RANDU numbers in 274 equal parts

z=matrix(0,ncol=73,nrow=274)
m=rep(0,274)
for (i in 1:274){
  m[i]=max(z[i,])
}

#########




FUN <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  factors <- list(neg = -factors, pos = factors)
  return(factors)
}

for (i in xx){
  if (y[i]> 0.5*y[i-1] && y[i]<0.51*y[i+1])
    xx2[i] =  i
}


n=2400000
k=500
xx2=1-1/n
xx= n*xx2^k*(1-n*xx2^k)-n*(1-2/n)^k*(1-n)
xx=n*(1-1/n)^k
xx1=n*(1-2/n)^k
xx3=n*n*(1-2/n)^k
xx4=n*n*(1-1/n)^(2*k)
xx5= ((3/n-1/n^2-1)*(1-1/n)^k+(1-2/n)^k*(1-1/n))*n^2
