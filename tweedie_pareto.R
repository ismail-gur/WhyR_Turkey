library(insuranceData)
library(fitdistrplus)
library(tweedie)
library(actuar)

data(WorkersComp)

data_test=WorkersComp$LOSS/1000000
hist(data_test,breaks = 10)
summary(data_test)
#only tweedie
loglik_tweedie<-function(parms){ 
  p=parms[1]
  mu=exp(parms[2])
  phi=exp(parms[3])
  llk <- -sum(log(dtweedie(data_test, p, mu, phi)))
  llk
}
ini <- c(1.5,1,1)
zop <- nlminb(ini,loglik_tweedie, lower =c(1+1e-6,-Inf,-Inf),upper =c(2-1e-6,Inf,Inf))
print(zop)

#pareto fit
data_test_par=data_test
loglik_par<-function(par){ 
  scale=par[1]
  shape=par[2]
  llk <- -sum(log(dpareto(data_test_par, scale=scale,shape=shape)))
  llk
}
ini <- c(1,1)
zop_par <- nlminb(ini,loglik_par, lower =c(0,0),upper =c(Inf,Inf))
print(zop_par)

#Composite Fit
loglik_composite <- function(par) {
  p=par[1]
  mu=exp(par[2])
  phi=exp(par[3])
  scale=par[4]
  shape=par[5]
  ll=-sum(log(dcomposite(data_test, spec1="tweedie",arg1=list(power=p, mu=mu, phi=phi), spec2="pareto",arg2=list(shape=shape,scale=scale))))
  return(ll)
}


ini <- c(zop$par[1],zop$par[2],zop$par[3],zop_par$par[1],zop_par$par[2])
zop_comp_test <- nlminb(ini,loglik_composite, lower =c(1+1e-6,0,0,0,2),upper =c(2-1e-6,Inf,Inf,Inf,Inf))
print(zop_comp_test)

p=zop_comp_test$par[1]
mu=exp(zop_comp_test$par[2])
phi=exp(zop_comp_test$par[3])
scale=zop_comp_test$par[4]
shape=zop_comp_test$par[5]


rcomp_test=rcomposite(length(data_test), spec1="tweedie",arg1=list(power=p, mu=mu, phi=phi), spec2="pareto",
                 arg2=list(shape=shape,scale=scale)) 


par(mfrow=c(1,1))
hist(rcomp_test,breaks=20)
hist(data_test,breaks=20)

qcomp_test=qcomposite(c(seq(0,1,0.01)), spec1="tweedie",arg1=list(power=p, mu=mu, phi=phi), spec2="pareto",
                        arg2=list(shape=shape,scale=scale)) 


q_empirical=quantile(data_test,c(seq(0,1,0.01)))
q.theoretical=qcomp_test
plot(q.theoretical,q_empirical, xlab="Empirical Quantiles", ylab="Theoretical Quantiles", main="Q-QPlot using plot function")
abline(a=0,b=1,lty="dotted") #adding a reference line


