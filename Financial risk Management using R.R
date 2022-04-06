library(quantmod)
data1=getSymbols("WILL5000IND", src="FRED",auto.assign =FALSE)
data1=na.omit(data1)
data1=data1["1979-12-31/2017-12-31"]
names(data1)="TER"
#Afficher les trois premiers colonnes
head(data1,3)
tail(data1,3)
plot(data1)
#Logarithm
#RETURN
'Le return (terme anglais) correspond à la performance globale 
dun investissement. Ce terme se rencontre surtout dans le domaine
financier. Il sapplique, par exemple, aux actions pour lesquelles
le return va prendre en compte le montant du dividende et,
également la plus ou moins value réalisée, pour appréhender 
la performance effective du placement effectué.'
logreturn=diff(log(data1$TER))
head(logreturn, 3)
logret=diff(log(data1$TER))[-1]
round(head(logret,3),6)
plot(logret)
tail(logret)
#Estimation des paramétres (Moyenne et Ecart type)
mean=round(mean(logret),8)
sigma=round(sd(logret),8)
#Valeur of normal distribution 
VAR=round(qnorm(0.05, mean, sigma),6)
VAR
#The Value at risk
PropVAR=1000*(exp(VAR)-1) 
#en million de dollard
#Calcul du expected shortfall
# Calcule du expected short d'un distribution normal
ES1=mean-sigma*dnorm(qnorm(0.05,0,1),0,1)/0.05
ES1
#Simulation de calcule de var et ES-1
alpha=0.05
set.seed(123789)
rvec=rnorm(100000,mean,sigma)
var1=quantile(rvec,alpha)
ES=mean(rvec[rvec<var1])
round(ES,6)
round(var1,6)

#Simulation de calcule de var et ES-2
alpha=0.05
set.seed(123789)
rvec=sample(as.vector(logret),100000,replace=TRUE)
var1=quantile(rvec,alpha)
ES=mean(rvec[rvec<var1])
round(ES,6)
round(var1,8)

"No normal distribbution"
#Skewness
library(moments)
rvec=as.vector(logret)
round(skewness(rvec),2)
#Kurtosis
library(moments)
rvec=as.vector(logret)
round(kurtosis(rvec),2)
#Normality test call jarques Bera Test
library(moments)
rvec=as.vector(logret)
jarque.test(rvec)
#Estimation par maximum de vrai semblance de certain paramétre
library(MASS)
rvec=as.vector(logret)
t.fit=fitdistr(rvec,"t")
round(t.fit$estimate, 6)
#Estimate VaR and ES for student-t
alpha=0.05
set.seed(123789)
library(metRology)
rvec=rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR=quantile(rvec, alpha)
ES=mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
# a Simulated from estimated student-t distribution
alpha=0.05
set.seed(123789)
library(metRology)
rvec=rep(0,100000)
for (i in 1:10){
  rvec=rvec+rt.scaled(100000, mean=t.fit$estimate[1], sd=t.fit$estimate[2], df=t.fit$estimate[3])
}
VaR=quantile(rvec,alpha)
ES=mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
# b IID simulating from empirical distribution
alpha=0.05
set.seed(123789)
library(metRology)
rvec=rep(0,100000)
for (i in 1:10){
  rvec=rvec+sample(as.vector(logret),100000,replace=TRUE)
}
VaR=quantile(rvec,alpha)
ES=mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

# c Block distribution from empirical distribution
alpha <- 0.05
set.seed(123789)
rdat=as.vector(logret)
rvec=rep(0,100000)
posn=seq(from=1,to=length(rdat)-9,by=1)
rpos=sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR=quantile(rvec,alpha)
ES= mean(rvec[rvec<VaR]) 
round(VaR,6)
round(ES,6)

#Semaine 4, Graphe of acf corrélation
acf(logret)
acf(abs(logret))
library(rugarch)
#Model Garch
garch.N=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0),include.mean=TRUE),
                   distribution.model="norm")
                   
fit.garch.N=ugarchfit(spec=garch.N, data=logret)
fit.garch.N

# Save fitted values
save1=cbind(logret[,1], fit.garch.N@fit$sigma, fit.garch.N@fit$z)
save1    
acf(fit.garch.N@fit$z)
acf(abs(fit.garch.N@fit$z))
#GARCH (1,1)-t
garch.t=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0),include.mean=TRUE),
                   distribution.model="std")
fit.garch.t=ugarchfit(spec=garch.t, data=logret)
fit.garch.t
#Save fitted values
save1=cbind(logret, fit.garch.t@fit$sigma, fit.garch.t@fit$z)
names(save1)=c("logret","s", "z")
parm1=fit.garch.t@fit$coef
save1
names
parm1
#AFC of fitted values
acf(save1$z)
acf(abs(save1$z))

#We use the R function “ugarchboot” to simulate 1-day outcomes:
set.seed(123789) 
boot.garch=ugarchboot(fit.garch.t,
                      method=c("Partial","Full")[1],
                      sampling="raw", 
                      n.ahead=1,
                      n.bootpred=100000,
                      solver="solnp")

rvec= boot.garch@fseries
VaR=quantile(rvec,0.05)
ES= mean(rvec[rvec<VaR])
VaR
ES
#Rolling 1 day VaR with ugarchroll
n2016=length(logret["1980-01-01/2016-12-31"])
roll.garch=ugarchroll(spec=garch.t,
          data=logret,
          n.ahead=1,
          forecast.length=1,
          n.start=n2016,
          refit.every=1,
          refit.window="recursive",
          calculate.VaR=TRUE,
          VaR.alpha=0.05,
          keep.coef=TRUE)
roll.garch


#e use the R function “ugarchboot” to simulate 1-day outcomes:
