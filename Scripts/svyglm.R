library(survey)
Village.data.2st=read.table("Village_data_2st.csv", header=T, sep=",")
summary(Village.data.2st)

Village.design.2st<-svydesign(~Village+HH.ID, strata=~Prov.Name+ones, nest=T, fpc=~Strat.Size+HH, weights=~Weight,data=Village.data.2st)
summary(Village.design.2st)

## linear regression for survey data
m1=svyglm(Birth.Weight~Dist.Water,Village.design.2st)

svyplot(Birth.Weight~Dist.Water, Village.design.2st, style="transparent", xlab="Dist.Water", ylab="Birth.Weight")
lines(Village.data.2st$Dist.Water,predict(m1))

## logistic regression for survey data
m2=svyglm(Vaccinated~HH.Size,Village.design.2st, family="quasibinomial")

svyplot(Vaccinated~HH.Size, Village.design.2st, style="transparent", xlab="HH.Size", ylab="Vaccinated")
lines(Village.data.2st$HH.Size,predict(m2, type="response"))
