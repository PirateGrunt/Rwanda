setwd("C:/Users/bfannin/Dropbox/rwanda_course/BAF Files/Data/")

library(sp)
library(maptools)

mapRwanda = readShapePoly("RWA")

plot(mapRwanda)

svyRwanda <- svydesign(~1, weights=~Weight, strata=~Prov.Name, fpc=~Strat.Size, data=Village.Data)

byProvince = svyby(~Dist.Water, ~Prov.Name, svyRwanda, svymean)

byProvince$Color = "red"
byProvince$Color[byProvince$Dist.Water < 2] = "yellow"
byProvince$Color[byProvince$Dist.Water < 1] = "green"

dfMerge = merge(mapRwanda, byProvince, by.x="ADM1", by.y="Prov.Name", all.x=TRUE)

mapRwanda@data = dfMerge

plot(mapRwanda, col=mapRwanda$Color)
title("Distance to water by province")

legend("topleft", legend=c(">2", "1-2", "<1"), title="Distance to water", pch=19, col=c("red", "yellow", "green"))

