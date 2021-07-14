library(rworldmap)
library(dplyr)
library(ggplot2)
library(scales)
library(rgeos)
library(rgdal)
library(openxlsx)
library(data.table)
library(ggpubr)



map_data=read.csv("politics.csv")%>%as.data.table()
map_data[income=="High",breakeven_high:=breakeven]
map_data[income=="Upper middle",breakeven_upper:=breakeven]
map_data[income=="Lower middle",breakeven_lower:=breakeven]
map_data[income=="Low",breakeven_low:=breakeven]

map_data[income=="High",breakeven_high_lag:=breakeven_lag]
map_data[income=="Upper middle",breakeven_upper_lag:=breakeven_lag]
map_data[income=="Lower middle",breakeven_lower_lag:=breakeven_lag]
map_data[income=="Low",breakeven_low_lag:=breakeven_lag]



mapped_data <- joinCountryData2Map(map_data, joinCode = "ISO3", 
                                    nameJoinColumn = "Country",
                                    projection = map_data)
new_world <- subset(mapped_data, continent != "Antarctica" & ADMIN != "Greenland")


catmethod=c(1940,1970,1980,1990,2000,2010,2020,2030)

color=c(rgb(102/255,0,0),
         rgb(214/255,47/255,39/255),
         rgb(235/255,110/255,75/255),
         rgb(247/255,164/255,116/255),
         rgb(1,227/255,166/255),
         rgb(1,1,51/255),
         rgb(1,1,204/255))

map_h<-mapCountryData(new_world,
                     nameColumnToPlot = "breakeven_high",
                     catMethod=catmethod,
                     colourPalette = rev(color),
                     missingCountryCol = "Gray90",
                     addLegend=FALSE,
                     mapTitle = "")

map_u<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_upper",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

map_lm<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_lower",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

map_l<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_low",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

do.call(addMapLegend,c(map_l,
                       legendIntervals = "page",
                       legendLabels="all",
                       legendWidth=0.4,
                       tcl=0,
                       labelFontSize=0.8,
                       legendMar=3))

map_h_lag<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_high_lag",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

map_u_lag<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_upper_lag",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

map_lm_lag<-mapCountryData(new_world,
                       nameColumnToPlot = "breakeven_lower_lag",
                       catMethod=catmethod,
                       colourPalette = rev(color),
                       missingCountryCol = "Gray90",
                       addLegend=FALSE,
                       mapTitle = "")

map_l_lag<-mapCountryData(new_world,
                      nameColumnToPlot = "breakeven_low_lag",
                      catMethod=catmethod,
                      colourPalette = rev(color),
                      missingCountryCol = "Gray90",
                      addLegend=FALSE,
                      mapTitle = "")

do.call(addMapLegend,c(map_l_lag,
                       legendIntervals = "page",
                       legendLabels="all",
                       legendWidth=0.4,
                       tcl=0,
                       labelFontSize=0.8,
                       legendMar=3))
