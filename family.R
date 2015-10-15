family.distribution = function(p){
  
  E=matrix(rep(0,36),nrow=6,ncol=6)
  d=s=0:5
  
  for (i in 1:6){
    E[1,i]=d[i]/(1-p)
    E[i,1]=s[i]/p
  }
    
  for (i in 2:6){
    for (j in 2:6){
      E[i,j]=1+p*E[i-1,j]+(1-p)*E[i,j-1]
    }
    
    
  }
  
  return(E) 
}

