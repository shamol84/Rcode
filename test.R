#assigmnet 4
##2
## n = no. of deviates
mix.dist = function(sigma,miu){
  p=0.05
  p1=p2=0.4
  p3=0.2
  n=500
  f1=f2=data.frame(value=rep(0,n))
  f1[,2]="f1-sigma1"
  f2[,2]="f2-sigma1"
 # f3=rep(0,1000)
  for(i in 1:n) {
    x=runif(1)
    f1[i,1] = (1-p)*(exp(-0.5*x^2)/(2*pi)^0.5)+(p/sigma)*(exp(-0.5*(x/sigma)^2)/(2*pi)^0.5)
    #m=i+500
    f2[i] = p1*(exp(-0.5*x^2)/(2*pi)^0.5)+p2*(exp(-0.5*(x-miu)^2)/(2*pi)^0.5)+p3*(exp(-0.5*(x+miu)^2)/(2*pi)^0.5)
  }
  
  f=rbind(f1,f2)
  return(f)
}