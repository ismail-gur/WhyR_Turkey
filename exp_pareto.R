library(insuranceData)
library(fitdistrplus)
library(gendist)

data(WorkersComp)

data_test=WorkersComp$LOSS/1000000
hist(data_test,breaks = 10)
summary(data_test)
#only exponential

fit_params <- fitdistr(data_test,"exponential")
rate=fit_params$estimate

#pareto fit
fp <- fitdist(data_test, "pareto", start=list(shape = 1, scale = 500))
shape=fp$estimate[1]
scale=fp$estimate[2]

par=rbind(rate,shape,scale)
#Composite Fit
loglik <- function(par) {
  rate=par[1]
  shape=par[2]
  scale=par[3]
  ll=-sum(log(dcomposite(data_test, spec1="exp",arg1=list(rate=rate), spec2="pareto",arg2=list(shape=shape,scale=scale))))
  return(ll)
}

ini=rbind(rate,shape,scale)
zop_comp_test <- nlminb(ini,loglik , lower =c(0,0,0),upper =c(5,4,4))
print(zop_comp_test)

library(numDeriv)
est <- zop_comp_test$par
names(est) <- c("rate","shape","scale")
hess<-hessian(loglik,est)
se <-sqrt(diag(solve(hess)))
print(cbind(est,se))

rate=zop_comp_test$par[1]
shape=zop_comp_test$par[2]
scale=zop_comp_test$par[3]



rcomp_test=rcomposite(length(data_test),spec1="exp",arg1=list(rate=rate), spec2="pareto",arg2=list(shape=shape,scale=scale))


par(mfrow=c(1,2))
hist(rcomp_test,breaks=20)
hist(data_test,breaks=20)

qcomp_test=qcomposite(c(seq(0,0.99,0.01)),spec1="exp",arg1=list(rate=rate), spec2="pareto",arg2=list(shape=shape,scale=scale))


q_empirical=quantile(data_test,c(seq(0,0.99,0.01)))
q.theoretical=qcomp_test
par(mfrow=c(1,1))
plot(q.theoretical,q_empirical, xlab="Empirical Quantiles", ylab="Theoretical Quantiles", main="Q-QPlot- Exp-Pareto Distribution")
abline(a=0,b=1,lty="dotted") #adding a reference line

