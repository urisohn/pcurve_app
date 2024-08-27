
  
#Get ncp for 33% power
  library(pwr)
  n33=10000
  d33 = pwr.t.test(n=n1,power=1/3)$d
  ncp33=sqrt(n33/2)*d33
  
  #This is approx, but good enough for purposes of testing
  
  
#Simulate chi-square with 33% power
  simtot=500   #number of simulations
  n=100        #number of studies per simulation

#Store results
  res=matrix(nrow=simtot,ncol=11)
  
  for (simk in 1:simtot)
  {
    
    set.seed(simk+8759)
      
  #Produce tests at random
    c1 = rchisq(n,df=1,ncp=ncp33^2)
    
  #Get the p-values
    p1 = 1-pchisq(c1,df=1)
    
  #Keep the significant ones
    c1.sig=c1[p1<.05]
  
  #Write it in the format expected by the app
    chi.text = paste0("chi2(1)=",round(c1.sig,4))
    writeLines(chi.text, "c:/temp/t1.txt")
  
  #Counter
    cat(simk, "\n")
  #Run the p-curve
    px = pcurve_app("t1.txt","c:/temp/")
    res[simk,]=px
    
  }
  
 
#half p-curve 33%
par(mfrow=c(1,2))
  plot(ecdf(res[,7]),main='Full p-curve',col=adjustcolor('blue',.85))   #full p-curve
  abline(a=0,b=1)
  plot(ecdf(res[,11]),main='Half p-curve',col=adjustcolor('blue',.85))   #half p-curve
  abline(a=0,b=1)
  
#Check that effective power was 33%
  mean(res[,2])/n  #Share of significant results over total studies attempted
  
  
#_----------------------------------------------------------------

# True null
  
#Simulate chi-square with 33% power
  simtot=500   #number of simulations
  n=500        #number of studies per simulation

#Store results
  res=matrix(nrow=simtot,ncol=11)
  
  for (simk in 1:simtot)
  {
    
    set.seed(simk+8759)
      
  #Produce tests at random
    c1 = rchisq(n,df=1)
    
  #Get the p-values
    p1 = 1-pchisq(c1,df=1)
    
  #Keep the significant ones
    c1.sig=c1[p1<.05]
  
  #Write it in the format expected by the app
    chi.text = paste0("chi2(1)=",round(c1.sig,4))
    writeLines(chi.text, "c:/temp/t1.txt")
  
  #Counter
    cat(simk, "\n")
  #Run the p-curve
    px = pcurve_app("t1.txt","c:/temp/")
    res[simk,]=px
    
  }
  
 
#half p-curve 33%
par(mfrow=c(1,2))
  plot(ecdf(res[,7]),main='Full p-curve',col=adjustcolor('blue',.85))   #full p-curve
  abline(a=0,b=1)
  plot(ecdf(res[,11]),main='Half p-curve',col=adjustcolor('blue',.85))   #half p-curve
  abline(a=0,b=1)
  
#Check that effective power was 33%
  mean(res[,2])/n  #Share of significant results over total studies attempted
  
  