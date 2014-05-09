###############################################################################
# LOAD THE LIBRARIES WE WILL NEED
library(survey)

###############################################################################
# SET WORKING DIRECTORY AND OPEN DATA
getwd()
setwd("C:/Users/bfannin/Dropbox/rwanda_course/MemoryStick/Data/")
Village.Data = read.table("Village.Data.csv", header=TRUE, sep=",")

###############################################################################
# CHECK OUR DATA
names(Village.Data)
summary(Village.Data)

###############################################################################
# CREATE THE SURVEY DESIGN OBJECT
Village.design <- svydesign(~1, weights=~Weight, strata=~Prov.Name
                            , fpc=~Strat.Size, data=Village.Data)

###############################################################################
# ESTIMATE A POPULATION MEAN
# svymean(formula, design)
svymean(~HH.Size, Village.design)

###############################################################################
# YOUR TURN
# ESTIMATE THE MEAN Birth.Weight
###############################################################################

###############################################################################
# Use the update function to add a column for population
# update(object, formulat)
Village.design = update(Village.design, Village.Pop = HH * HH.Size)
svytotal(~Village.Pop, Village.design)
svymean(~Village.Pop, Village.design)

###############################################################################
# svyby
# sbyby(formula, by, design, function)
AvgPopByProvince = svyby(~Village.Pop, ~Prov.Name, Village.design, svymean)
AvgPopByProvince

TotalPopByProvince = svyby(~Village.Pop, ~Prov.Name, Village.design, svytotal)
TotalPopByProvince

###############################################################################
# DRAW THE RESULTS
barplot(AvgPopByProvince, main="Average village size by province")
barplot(TotalPopByProvince, main="Total population by province")

###############################################################################
# YOUR TURN
# COMPUTE AVERAGE Birth.Weight BY PROVINCE
# DISPLAY THE RESULTS IN A BAR CHART
###############################################################################

###############################################################################
# DOT CHARTS
dotchart(AvgPopByProvince, main="Average village size by province")
dotchart(TotalPopByProvince, main="Total population by province")

###############################################################################
# YOUR TURN
# DISPLAY AVERAGE Birth.Weight BY PROVINCE AS A DOT CHART
###############################################################################

###############################################################################
# PLOTTING TWO VARIABLES

# VERSION 1 - NOT VERY PRETTY
svyplot(Birth.Weight~Dist.Water, Village.design, style="transparent")

# VERSION 2 - MUCH PRETTIER
svyplot(Birth.Weight~Dist.Water, Village.design, style="transparent"
        , xlab = "Distance to water(km)", ylab="Birth weight(kg)"
        , main="Birth weight(kg) vs. Distance to water(km)"
        , alpha=c(0.5,1), pch=19)

svyplot(HH.Size~Dist.Water, Village.design, style="transparent"
        , xlab = "Average household size (people)", ylab="Birth weight(kg)"
        , main="Birth weight(kg) vs. Average household size(people)"
        , alpha=c(0.5,1), pch=19)

###############################################################################
# LATTICE PLOTS
svycoplot(Birth.Weight~Dist.Water|Prov.Name, Village.design, style="transparent"
          , xlab = "Distance to water(km)", ylab="Birth weight(kg)"
          , main="Birth weight(kg) vs. Distance to water(km)"
          , alpha=c(0.5,1))

svycoplot(Birth.Weight~Dist.Water|(Prov.Name=="Kigali"), Village.design, style="transparent"
          , xlab = "Distance to water(km)", ylab="Birth weight(kg)"
          , main="Birth weight(kg) vs. Distance to water(km)"
          , alpha=c(0.5,1))
