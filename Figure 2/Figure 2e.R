library(data.table)
library(dplyr)
library(rworldmap)
library(ggplot2)
data=read.csv("figure 2.csv")%>%as.data.table()
data[,birth:=2020-breakeven]
data[,birth_lag:=2020-breakeven_lag]

age=read.csv('Age structure.csv');
age0=setDT(age[age$SSP==c("SSP4"),3:5]);
setnames(age0,c("X2020"),c("Population"))

cbn=merge(data,age0,by.x=c("Country"),by.y=("ISO3"),allow.cartesian=TRUE)

popsum=cbn[,.(pop=sum(Population)),by=c("Country")]
popunder=cbn[age<=birth,.(popunder=sum(Population)),by=c("Country")]
popunder_lag=cbn[age<=birth_lag,.(popunder=sum(Population)),by=c("Country")]

pop=left_join(popsum,popunder,by=c("Country"))%>%setDT()
pop=left_join(pop,popunder_lag,by=c("Country"))
setnames(pop,c("popunder.x","popunder.y"),c("popunder","popunder_lag"))
pop=pop[is.na(popunder),popunder:=0]
pop_per=pop[,.(Country=Country,per=popunder/pop,per_lag=popunder_lag/pop)]


mapped_data <- joinCountryData2Map(pop_per, joinCode = "ISO3", 
                                    nameJoinColumn = "Country",
                                    projection = map_data)

new_world <- subset(mapped_data, continent != "Antarctica" & ADMIN != "Greenland")

catmethod=c(0,0.25,0.5,0.75,1)

color=c(rgb(102/255,0,0),
        rgb(214/255,47/255,39/255),
         #rgb(235/255,110/255,75/255),
         rgb(247/255,164/255,116/255),
         #rgb(1,227/255,166/255),
         rgb(1,1,191/255)
        )

map<-mapCountryData(new_world,
                     nameColumnToPlot = "per_lag",
                     catMethod=catmethod,
                     colourPalette = rev(color),
                     missingCountryCol = "Gray90",
                     addLegend=FALSE,
                     mapTitle = "")

do.call(addMapLegend,c(map,
                       legendIntervals = "page",
                       legendLabels="all",
                       legendWidth=0.3,
                       tcl=0,
                       labelFontSize=0.8,
                       legendMar=3))
