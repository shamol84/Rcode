#assigmnet 4
##2
## n = no. of deviates
mix.dist = function(sigma,miu,n){
  p=0.05
  p1=p2=0.4
  p3=0.2
  f1=f2=f3=f4=f5=f6=rep(0,n)
  #f3=rep(0,3000)
  for(i in 1:n) {
  x=runif(1)
  f1[i] = (1-p)*(exp(-0.5*x^2)/(2*pi)^0.5)+(p/sigma[1])*(exp(-0.5*(x/sigma[1])^2)/(2*pi)^0.5)
  f2[i] = (1-p)*(exp(-0.5*x^2)/(2*pi)^0.5)+(p/sigma[2])*(exp(-0.5*(x/sigma[2])^2)/(2*pi)^0.5)
  f3[i] = (1-p)*(exp(-0.5*x^2)/(2*pi)^0.5)+(p/sigma[3])*(exp(-0.5*(x/sigma[3])^2)/(2*pi)^0.5)
  
  
  f4[i] = p1*(exp(-0.5*x^2)/(2*pi)^0.5)+p2*(exp(-0.5*(x-miu[1])^2)/(2*pi)^0.5)+p3*(exp(-0.5*(x+miu[1])^2)/(2*pi)^0.5)
  f5[i] = p1*(exp(-0.5*x^2)/(2*pi)^0.5)+p2*(exp(-0.5*(x-miu[2])^2)/(2*pi)^0.5)+p3*(exp(-0.5*(x+miu[2])^2)/(2*pi)^0.5)
  f6[i] = p1*(exp(-0.5*x^2)/(2*pi)^0.5)+p2*(exp(-0.5*(x-miu[3])^2)/(2*pi)^0.5)+p3*(exp(-0.5*(x+miu[3])^2)/(2*pi)^0.5)
  }
 
  f=data.frame(f1,f2,f3,f4,f5,f6)
  return(f)
}

