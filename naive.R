theta0=1/2-1/pi*atan(2) 
theta0 
#[1] 0.1475836

#Naive method
naive=function(n)
{
  x <- rcauchy(n)
  theta1 <- length(x[x > 2])/n
  se1 <- sqrt((theta1 * (1 - theta1))/n)
  se.true <- sqrt((theta0 * (1 - theta0))/n)
  cbind(theta1, se1, se.true)
}


#Importance sampling
imp=function(n)
{
  xin <- runif(n)/2
  y <- 1/(2 * pi * (1 + xin^2))
  cbind(mean(y), sqrt(var(y)/n), 9.55253e-05)
}


int1=function (x){x^4*sin(pi*(x+1))*exp(-x^2/2)}
xx= exp(-x^2/2)/sqrt(2*pi)

pimc=function(n){ # n is the number of simulations
  x=rnorm(n,0,2) # generate a sequence of uniform U random numbers
  #x= length(u[u>2])/n
  #x=subset(y,y>2)
  #x= x[x>2]
  #n1 = length(x)
  g=sqrt(2*pi)*(x^4)*sin(pi*(x+1)) # g(U)
  pimc=mean(g) # pi estimator
  
  var1 = sqrt(var(g)/n)
  cbind(pimc,var1,var(g))
}




int2 = function(x){2*(1/x^6)*sin(pi*(1/x+1))*exp(-1/(2*x*x))}
int3 = function(x){2*((1/x^6)*sin(pi*(1/x+1))*exp(-1/(2*x*x))^2)}

pimc1=function(n){ # n is the number of simulations
  x=runif(n,0,0.5) # generate a sequence of uniform U random numbers
  #x= length(u[u>2])/n
  #x=subset(x,x>2)
  g= (1/x^6)*sin(pi*(1+1/x))*exp(-1/(2*x*x)) # g(U)
  pimc=mean(g) # pi estimator
  
  var1 = sqrt(var(g)/n)
  cbind(pimc,var1,var(g))
}


