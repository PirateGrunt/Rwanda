library(survey)
setwd("~/Dropbox/rwanda_course/MemoryStick/Data")
Rwanda.frame <- read.csv("Rwanda_frame_expand.csv")

## Draw a Stratified PPS sample from Rwanda village frame
set.seed(5678)
Provinces=unique(Rwanda.frame$Prov.Name)
n=100
N.Village=nrow(Rwanda.frame)
village.pps=c()
village.labels=1:N.Village
for(h in 1:length(Provinces))
	{	village.pps <- c(village.pps,sample(village.labels[(Rwanda.frame$Prov.Name==Provinces[h])],n,prob=Rwanda.frame$HH[(Rwanda.frame$Prov.Name==Provinces[h])]))
		}
mean(Rwanda.frame$HH)
mean(Rwanda.frame$HH[village.pps])
Village.data <- Rwanda.frame[village.pps,]

## Create weights
Weight=1/(n*Village.data$HH)
Strat.Size=rep(0,length(Village.data$HH))
for(h in 1:length(Provinces))
	{	Strat.Size[(Village.data$Prov.Name==Provinces[h])]=sum(Rwanda.frame$Prov.Name==Provinces[h])
		Weight[(Village.data$Prov.Name==Provinces[h])]=Weight[(Village.data$Prov.Name==Provinces[h])]*sum(Rwanda.frame$HH[(Rwanda.frame$Prov.Name==Provinces[h])])
		}
Village.data <- cbind(Village.data, Strat.Size, Weight)

## Create design object
Village.design <- svydesign(~1, weights=~Weight, strata=~Prov.Name, fpc=~Strat.Size, data=Village.data)

## Analysis
summary(Village.design)
Village.means <- svymean(~Prov.Name+Prim.School.M+Prim.School.F,Village.design)
svycontrast(Village.means, quote(Prim.School.F-Prim.School.M))
Village.design <- update(Village.design, Prim.School.Diff=Prim.School.F-Prim.School.M)
svymean(~Prim.School.Diff,Village.design)
svycontrast(Village.means, quote(Prim.School.F/Prim.School.M))
School.ratio<-svyratio(~Prim.School.F,~Prim.School.M,Village.design)
coef(School.ratio)
vcov(School.ratio)

Village.quantiles<-svyquantile(~Dist.Water, Village.design, c(0.1,0.25, 0.5, 0.75, 0.9), ci=T)
Village.quantiles$quantiles
Village.quantiles$quantiles[1]

Village.design.largeHH<-subset(Village.design, HH>200)
svymean(~Prov.Name+HH.Size,Village.design.largeHH)
svyby(~HH.Size,~(HH>200),Village.design,svymean)
svyby(~HH.Size,~Prov.Name,Village.design,svymean)
svyby(~HH.Size,~Prov.Name+(HH>200),Village.design,svymean)

Village.design <- update(Village.design, Vill.pop=HH*HH.Size)
Village.total <- svytotal(~Vill.pop,Village.design)
svyby(~Vill.pop,~Prov.Name,Village.design,svytotal)
svyby(~Prim.School.F,~Prov.Name,Village.design,denominator=~Prim.School.M,svyratio)