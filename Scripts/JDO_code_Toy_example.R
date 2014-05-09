library(survey)

### Toy example: introduction to using survey package

## Create toy population
Farm=seq(1:4)
Size=c(4, 6, 6, 20)
Coffee=c(1, 3, 5, 15)
Toy.frame <- data.frame(Farm, Size, Coffee)

## Scenario 1: simple random sampling
# draw sample and create sample data file
toy.sample1 <- sample(1:4, 2)
Toy.data1 <- Toy.frame[toy.sample1,]
Weight=c(2,2)
Toy.data1 <- cbind(Toy.data1,Weight)

# create the design object (first without FPC, then with FPC)
Toy.design1.wr <- svydesign(~1, weights=~Weight, data=Toy.data1)
Pop.size=rep(4,2)
Toy.data1 <- cbind(Toy.data1,Pop.size)
Toy.design1 <- svydesign(~1, weights=~Weight, fpc=~Pop.size, data=Toy.data1)

# compute estimates
coffee.wr<-svytotal(~Coffee,Toy.design1.wr)
coffee<-svytotal(~Coffee,Toy.design1)
vcov(coffee)
coef(coffee)

# check unbiasedness of estimator and variance estimator
B=1000
total.wr.sim=rep(0,B)
var.wr.sim=total.wr.sim
total.sim=total.wr.sim
var.sim=total.wr.sim
for(b in 1:B)
	{ toy.sample1 <- sample(1:4, 2)
	Toy.data1 <- Toy.frame[toy.sample1,]
	Weight=c(2,2)
	Toy.data1 <- cbind(Toy.data1,Weight)
	Toy.design1.wr <- svydesign(~1, weights=~Weight, data=Toy.data1)
	Pop.size=rep(4,2)
	Toy.data1 <- cbind(Toy.data1,Pop.size)
	Toy.design1 <- svydesign(~1, weights=~Weight, fpc=~Pop.size, data=Toy.data1)
	coffee.wr<-svytotal(~Coffee,Toy.design1.wr)
	coffee<-svytotal(~Coffee,Toy.design1)
	total.wr.sim[b]<-coef(coffee.wr)
	var.wr.sim[b]<-vcov(coffee.wr)
	total.sim[b]<-coef(coffee)
	var.sim[b]<-vcov(coffee)
	}
par(mfrow=c(2,2))
hist(total.wr.sim, breaks=2*(1:21))
hist(var.wr.sim, breaks=10*(0:81))
hist(total.sim, breaks=2*(1:21))
hist(var.sim, breaks=10*(0:81))
mean(total.sim)
var(total.sim)*(B-1)/B
mean(var.wr.sim)
mean(var.sim)

## Scenario 2: stratified simple random sampling
# draw sample and create sample data file
toy.sample2 <- c(sample(1:3, 1),4)
Toy.data2 <- Toy.frame[toy.sample2,]
Weight=c(3,1)
Stratum=c("Small farms","Large farms")
Stratum.size=c(3,1)
Toy.data2 <- cbind(Toy.data2, Weight, Stratum, Stratum.size)

# create the design object
Toy.design2 <- svydesign(~1, strata=~Stratum, weights=~Weight, fpc=~Stratum.size, data=Toy.data2)

# compute estimates
options(survey.lonely.psu = "adjust")
coffee<-svytotal(~Coffee,Toy.design2)

# check unbiasedness of estimator
B=1000
total.srs.sim=rep(0,B)
total.strat.sim=total.srs.sim
var.strat.sim=total.srs.sim
for(b in 1:B)
	{ toy.sample1 <- sample(1:4, 2)
	Toy.data1 <- Toy.frame[toy.sample1,]
	Weight=c(2,2)
	Pop.size=rep(4,2)
	Toy.data1 <- cbind(Toy.data1,Weight,Pop.size)
	Toy.design1 <- svydesign(~1, weights=~Weight, fpc=~Pop.size, data=Toy.data1)
	coffee<-svytotal(~Coffee,Toy.design1)
	total.srs.sim[b]<-coef(coffee)
	toy.sample2 <- c(sample(1:3, 1),4)
	Toy.data2 <- Toy.frame[toy.sample2,]
	Weight=c(3,1)
	Stratum=c("Small farms","Large farms")
	Stratum.size=c(3,1)
	Toy.data2 <- cbind(Toy.data2, Weight, Stratum, Stratum.size)
	Toy.design2 <- svydesign(~1, strata=~Stratum, weights=~Weight, fpc=~Stratum.size, data=Toy.data2)
	coffee<-svytotal(~Coffee,Toy.design2)
	total.strat.sim[b]<-coef(coffee)
	var.strat.sim<-vcov(coffee)
	}
par(mfrow=c(2,1))
hist(total.srs.sim, breaks=2*(1:21))
hist(total.strat.sim, breaks=2*(1:21))
mean(total.strat.sim)
var(total.strat.sim)*(B-1)/B
mean(var.strat.sim)

## Scenario 3: cluster sampling
# draw sample and create sample data file
Neighborhood=c(1,1,2,3)
Toy.frame.c=cbind(Toy.frame,Neighborhood)
toy.sample3 <- sample(1:3, 2)
Toy.data3 <- Toy.frame.c[(Neighborhood %in% toy.sample3),]
Weight=rep(1.5,length(Toy.data3$Farm))
Cluster.size=rep(3,length(Toy.data3$Farm))
Toy.data3 <- cbind(Toy.data3, Weight, Cluster.size)

# create the design object
Toy.design3 <- svydesign(~Neighborhood, weights=~Weight, fpc=~Cluster.size, data=Toy.data3)

# compute estimates
coffee<-svytotal(~Coffee,Toy.design3)

# check unbiasedness of estimator
B=1000
total.srs.sim=rep(0,B)
total.strat.sim=total.srs.sim
total.c.sim=total.srs.sim
var.c.sim=total.srs.sim
for(b in 1:B)
	{ toy.sample1 <- sample(1:4, 2)
	Toy.data1 <- Toy.frame[toy.sample1,]
	Weight=c(2,2)
	Pop.size=rep(4,2)
	Toy.data1 <- cbind(Toy.data1,Weight,Pop.size)
	Toy.design1 <- svydesign(~1, weights=~Weight, fpc=~Pop.size, data=Toy.data1)
	coffee<-svytotal(~Coffee,Toy.design1)
	total.srs.sim[b]<-coef(coffee)
	toy.sample2 <- c(sample(1:3, 1),4)
	Toy.data2 <- Toy.frame[toy.sample2,]
	Weight=c(3,1)
	Stratum=c("Small farms","Large farms")
	Stratum.size=c(3,1)
	Toy.data2 <- cbind(Toy.data2, Weight, Stratum, Stratum.size)
	Toy.design2 <- svydesign(~1, strata=~Stratum, weights=~Weight, fpc=~Stratum.size, data=Toy.data2)
	coffee<-svytotal(~Coffee,Toy.design2)
	total.strat.sim[b]<-coef(coffee)
	toy.sample3 <- sample(1:3, 2)
	Toy.data3 <- Toy.frame.c[(Neighborhood %in% toy.sample3),]
	Weight=rep(1.5,length(Toy.data3$Farm))
	Cluster.size=rep(3,length(Toy.data3$Farm))
	Toy.data3 <- cbind(Toy.data3, Weight, Cluster.size)
	Toy.design3 <- svydesign(~Neighborhood, weights=~Weight, fpc=~Cluster.size, data=Toy.data3)
	coffee<-svytotal(~Coffee,Toy.design3)
	total.c.sim[b]<-coef(coffee)
	var.c.sim[b]<-vcov(coffee)
	}
par(mfrow=c(3,1))
hist(total.srs.sim, breaks=2*(1:21))
hist(total.strat.sim, breaks=2*(1:21))
hist(total.c.sim, breaks=2*(1:21))
mean(total.c.sim)
var(total.c.sim)*(B-1)/B
mean(var.c.sim)