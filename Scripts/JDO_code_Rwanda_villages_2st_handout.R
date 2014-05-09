library(survey)

Rwanda.frame <- read.csv("Rwanda_frame.csv")
summary(Rwanda.frame)

set.seed(1234)
N.Village=sum(Rwanda.frame$HH>1)
Rwanda.frame$Dist.Water<-((Rwanda.frame$Prov.Name=="Kigali")*rnorm(N.Village,0.75,0.3)+(Rwanda.frame$Prov.Name!="Kigali")*rnorm(N.Village,2,0.75))
Rwanda.frame$Dist.Water<-(Rwanda.frame$Dist.Water>0)*Rwanda.frame$Dist.Water+(Rwanda.frame$Dist.Water<=0)*0
Rwanda.frame$Vaccinated<-(Rwanda.frame$Prov.Name=="Kigali")*rnorm(N.Village,0.963,0.05)+(Rwanda.frame$Prov.Name=="West")*rnorm(N.Village,0.809,0.1)+(Rwanda.frame$Prov.Name=="North")*rnorm(N.Village,0.936,0.06)+(Rwanda.frame$Prov.Name=="South")*rnorm(N.Village,0.928,0.05)+(Rwanda.frame$Prov.Name=="East")*rnorm(N.Village,0.928,0.05)
Rwanda.frame$Vaccinated<-(Rwanda.frame$Vaccinated<=1)*Rwanda.frame$Vaccinated+(Rwanda.frame$Vaccinated>1)*1
Rwanda.frame$HH.Size<-(Rwanda.frame$Prov.Name=="Kigali")*rnorm(N.Village,4.3,1.5)+(Rwanda.frame$Prov.Name=="West")*rnorm(N.Village,5,2)+(Rwanda.frame$Prov.Name=="North")*rnorm(N.Village,4.6,1.5)+(Rwanda.frame$Prov.Name=="South")*rnorm(N.Village,4.5,1.3)+(Rwanda.frame$Prov.Name=="East")*rnorm(N.Village,4.6,1.75)
Rwanda.frame$HH.Size<-(Rwanda.frame$HH.Size>=1)*Rwanda.frame$HH.Size+(Rwanda.frame$HH.Size<1)*1
Rwanda.frame$HH.Size.Adult<-Rwanda.frame$HH.Size-rnorm(N.Village,3.5,0.5)
Rwanda.frame$HH.Size.Adult<-(Rwanda.frame$HH.Size.Adult>=1)*Rwanda.frame$HH.Size.Adult+(Rwanda.frame$HH.Size.Adult<1)*1
Rwanda.frame$Prim.School.M<-(Rwanda.frame$Prov.Name=="Kigali")*rnorm(N.Village,0.121,0.2)+(Rwanda.frame$Prov.Name=="West")*rnorm(N.Village,0.076,0.05)+(Rwanda.frame$Prov.Name=="North")*rnorm(N.Village,0.119,0.05)+(Rwanda.frame$Prov.Name=="South")*rnorm(N.Village,0.09,0.08)+(Rwanda.frame$Prov.Name=="East")*rnorm(N.Village,0.1,0.05)
Rwanda.frame$Prim.School.M<-(Rwanda.frame$Prim.School.M>0)*Rwanda.frame$Prim.School.M+(Rwanda.frame$Prim.School.M<=0)*0
Rwanda.frame$Prim.School.F<-(Rwanda.frame$Prov.Name=="Kigali")*rnorm(N.Village,0.103,0.2)+(Rwanda.frame$Prov.Name=="West")*rnorm(N.Village,0.08,0.05)+(Rwanda.frame$Prov.Name=="North")*rnorm(N.Village,0.111,0.05)+(Rwanda.frame$Prov.Name=="South")*rnorm(N.Village,0.095,0.03)+(Rwanda.frame$Prov.Name=="East")*rnorm(N.Village,0.086,0.05)
Rwanda.frame$Prim.School.F<-(Rwanda.frame$Prim.School.F>0)*Rwanda.frame$Prim.School.F+(Rwanda.frame$Prim.School.F<=0)*0
Rwanda.frame$Birth.Weight<-3.5-0.25*(Rwanda.frame$Dist.Water-mean(Rwanda.frame$Dist.Water))+0.003*min((Rwanda.frame$HH-mean(Rwanda.frame$HH)),1000)+0.25*rnorm(N.Village)

## Two-stage sampling

# Create 1st stage sample from village sampling frame
Provinces=unique(Rwanda.frame$Prov.Name)
n.1st=10
n.2st=10
N.Village=nrow(Rwanda.frame)
set.seed(4587)
village.pps=c()
village.labels=1:N.Village
for(h in 1:length(Provinces))
	{	village.pps <- c(village.pps,sample(village.labels[(Rwanda.frame$Prov.Name==Provinces[h])],n.1st,prob=Rwanda.frame$HH[(Rwanda.frame$Prov.Name==Provinces[h])]), replace=F)
		}
Village.data.1st <- Rwanda.frame[village.pps,]
Weight.1st=1/(n.1st*Village.data.1st$HH)
Strat.Size=rep(0,length(Village.data.1st$HH))
for(h in 1:length(Provinces))
	{	Strat.Size[(Village.data.1st$Prov.Name==Provinces[h])]=sum(Rwanda.frame$Prov.Name==Provinces[h])
		Weight.1st[(Village.data.1st$Prov.Name==Provinces[h])]=Weight.1st[(Village.data.1st$Prov.Name==Provinces[h])]*sum(Rwanda.frame$HH[(Rwanda.frame$Prov.Name==Provinces[h])])
		}
Village.data.1st <- cbind(Village.data.1st, Strat.Size, Weight.1st)

# Create 2nd stage sample from 1st stage sample
Village.data.2st <- Village.data.1st[rep(1:nrow(Village.data.1st),each=n.2st),]	
Village.data.2st$HH.ID<-seq(1:length(Village.data.2st$Prov.Name))
for (k in 1:nrow(Village.data.2st))
	{	Village.data.2st$Dist.Water[k] <- rnorm(1, Village.data.2st$Dist.Water[k], 0.5)
		Village.data.2st$Vaccinated[k] <- rbinom(1,1,Village.data.2st$Vaccinated[k])
		Village.data.2st$HH.Size[k] <- rpois(1,Village.data.2st$HH.Size[k])
		Village.data.2st$HH.Size.Adult[k] <- Village.data.2st$HH.Size[k]- rpois(1,3)
	}
Village.data.2st$Dist.Water <-(Village.data.2st$Dist.Water>0)* Village.data.2st$Dist.Water+(Village.data.2st$Dist.Water<=0)*0
Village.data.2st$HH.Size <-(Village.data.2st$HH.Size>=1)* Village.data.2st$HH.Size +(Village.data.2st$HH.Size <1)*1
Village.data.2st$HH.Size.Adult <-(Village.data.2st$HH.Size.Adult>=1)* Village.data.2st$HH.Size.Adult +(Village.data.2st$HH.Size.Adult <1)*1	
Village.data.2st$Weight.2st=Village.data.2st$HH/n.2st
Village.data.2st$Weight=Village.data.2st$Weight.1st*Village.data.2st$Weight.2st
Village.data.2st <- cbind(Village.data.2st, rep(1,length(Village.data.2st$Prov.Name)))
names(Village.data.2st)[ncol(Village.data.2st)]<-"ones"

# Different versions of the design object for 2-stage sampling
Village.design.2st<-svydesign(~Village, strata=~Prov.Name, nest=T, weights=~Weight,data=Village.data.2st)
Village.design.2st<-svydesign(~Village, strata=~Prov.Name, nest=T, fpc=~Strat.Size, weights=~Weight,data=Village.data.2st)
Village.design.2st<-svydesign(~Village+HH.ID, strata=~Prov.Name+ones, nest=T, fpc=~Strat.Size+HH, weights=~Weight,data=Village.data.2st)
Village.design.2st<-svydesign(~Village+HH.ID, strata=~Prov.Name+ones, nest=T, fpc=~Strat.Size+HH, weights=~Weight.1st+Weight.2st,data=Village.data.2st)