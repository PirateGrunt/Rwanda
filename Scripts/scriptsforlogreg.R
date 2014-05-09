### read the data:


### make a plot of CHD variable against age of subject:
par(mar=c(4,4,1,1))
plot(sa$age,sa$chd,xlab="Age of subject",ylab="CHD",pch="|")

## estimate the probability function for CHD as a function of age:
m1=glm(sa$chd~sa$age,family=binomial)

## get a summary table to see if the relationship is statistically significant:
summary(m1)

## add fitted probabilities to the plot:
points(sa$age,fitted.values(m1),col=2)

## instead of points, add a nice curve to the plot:
xpl=10:70
eta=-3.522+.0641*xpl;mu=exp(eta)/(1+exp(eta))
lines(xpl,mu,col=2,lwd=3)

#### challenge: modify the above code to analyze probability of CHD by adiposity  (you write the code)

## now make a model using both predictors:

summary(glm(sa$chd~sa$age+sa$adiposity,family=binomial))

## make a model using log(LDL) and age as predictors

summary(glm(sa$chd~log(sa$ldl)+sa$age,family=binomial))

## make a 3-d plot of the fit:

x1=0:20/20*(max(log(sa$ldl))-min(log(sa$ldl)))+min(log(sa$ldl))
x2=0:20/20*(max(sa$age)-min(sa$age))+min(sa$age)
etamat=matrix(nrow=21,ncol=21)
for(i in 1:21){
    for(j in 1:21){
      etamat[i,j]=-4.767+1.015*x1[i]+.0575*x2[j]
    }
}
mumat=exp(etamat)/(1+exp(etamat))
par(mar=c(1,1,1,1))
persp(x1,x2,mumat,tick="detailed",xlab="log(LDL)",ylab="age",zlab="Prob(CHD)",theta=-30)

## add family history variable to the model:

fam=1:462*0
fam[sa$famhist=="Present"]=1
summary(glm(sa$chd~log(sa$ldl)+sa$age+fam,family=binomial))

## make a 3-d plot with both surfaces:

x1=0:20/20*(max(log(sa$ldl))-min(log(sa$ldl)))+min(log(sa$ldl))
x2=0:20/20*(max(sa$age)-min(sa$age))+min(sa$age)
etamat=matrix(nrow=21,ncol=21)
for(i in 1:21){
   for(j in 1:21){
     etamat[i,j]=-4.852+.914*x1[i]+.0538*x2[j]
   }
}
mumat1=exp(etamat)/(1+exp(etamat))
mumat2=exp(etamat+.8715)/(1+exp(etamat+.8715))
par(mar=c(1,1,1,1))
persp(x1,x2,mumat1,tick="detailed",xlab="log(LDL)",ylab="age",zlab="Prob(CHD)",theta=-30,zlim=c(0,1),col="tan")
par(new=TRUE)
persp(x1,x2,mumat2,tick="detailed",xlab="log(LDL)",ylab="age",zlab="Prob(CHD)",theta=-30,zlim=c(0,1),col="tan1")