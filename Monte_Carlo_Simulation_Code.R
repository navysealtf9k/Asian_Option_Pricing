install.packages('sde')
library(sde)

mu=0.16; sigma=0.2; P0=40; T = 1/12 ##1 month
nt=50; n=2^(8)
#############Generate nt trajectories
dt=T/n; t=seq(0,T,by=dt)
X=matrix(rep(0,length(t)*nt), nrow=nt)
for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T,N=n)}
##Plot
ymax=max(X); ymin=min(X) #bounds for simulated prices
plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
     ylab="Price P(t)",xlab="time t")
for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
#
