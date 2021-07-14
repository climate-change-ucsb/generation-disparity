library(openxlsx)
library(data.table)
data=read.xlsx("Energy Investment.xlsx")

setDT(data)

Investment=data[(Scenario=="CPol" | Scenario=="2C") & Variable=="Total energy investment",]

ID=Investment[,c(1:3)]
name=colnames(data[,6:23])
Investment <- Investment[,lapply(.SD,as.numeric),.SDcols=name]
Investment <- cbind(ID,Investment)

ID_delta=Investment[Scenario=="2C",c(1:2)]
delta=Investment[Scenario=="2C",c(4:21)]-Investment[Scenario=="CPol",c(4:21)]
delta <- cbind(ID_delta,delta)


name=colnames(Investment[,4:21])

#median
Investment_median <- Investment[,lapply(.SD,median),.SDcols=name,by=c("Region","Scenario")]
Investment_CPol <- Investment_median[Scenario=="CPol"]
Investment_2C <- Investment_median[Scenario=="2C"]

delta_median <- delta[,lapply(.SD,median),.SDcols=name,by=c("Region")]


#Max
Investment_max <- Investment[,lapply(.SD,max),.SDcols=name,by=c("Region","Scenario")]
Investment_CPol_max <- Investment_max[Scenario=="CPol"]
Investment_2C_max <- Investment_max[Scenario=="2C"]

delta_max <- delta[,lapply(.SD,max),.SDcols=name,by=c("Region")]

#Min
Investment_min <- Investment[,lapply(.SD,min),.SDcols=name,by=c("Region","Scenario")]
Investment_CPol_min <- Investment_min[Scenario=="CPol"]
Investment_2C_min <- Investment_min[Scenario=="2C"]

delta_min <- delta[,lapply(.SD,min),.SDcols=name,by=c("Region")]

wb <- createWorkbook()
addWorksheet(wb, "CPol")
addWorksheet(wb, "2C")
addWorksheet(wb,"delta")
addWorksheet(wb, "CPol_max")
addWorksheet(wb, "2C_max")
addWorksheet(wb,"delta_max")
addWorksheet(wb, "CPol_min")
addWorksheet(wb, "2C_min")
addWorksheet(wb,"delta_min")
writeData(wb,"CPol",Investment_CPol)
writeData(wb,"2C",Investment_2C)
writeData(wb,"delta",delta_median)

writeData(wb,"CPol_max",Investment_CPol_max)
writeData(wb,"2C_max",Investment_2C_max)
writeData(wb,"delta_max",delta_max)

writeData(wb,"CPol_min",Investment_CPol_min)
writeData(wb,"2C_min",Investment_2C_min)
writeData(wb,"delta_min",delta_min)

saveWorkbook(wb, "Investment.xlsx", overwrite = TRUE)




library(countrycode)
library(stringr)

ssp_csv = file.path('SspDb_country_data_2013-06-12.csv')
sspdb = fread(ssp_csv, header = T)
match=grepl("Population.*Female.*Aged|Population.*Male.*Aged",sspdb$VARIABLE)

SSP=sspdb[VARIABLE %in% VARIABLE[match],]
donotwant=grepl("Aged.*Education",SSP$VARIABLE)
SSP=SSP[VARIABLE %in% VARIABLE[!donotwant],]

SSP  = melt(SSP,
            id.vars = 1:5,
            measure.vars = paste(seq(2020,2100,by = 5)),
            variable.name = "year",
            na.rm = T,
            variable.factor = F)
setnames(SSP,"REGION","ISO3")
SSP[, year := as.numeric(year)]
SSP[, Country := countrycode(ISO3, "iso3c", "country.name")]
SSP[, SSP := str_extract(SCENARIO,"SSP\\d")]
#SSP[, Age := sub(".*Aged","",VARIABLE)]
SSP[, Age := str_extract(VARIABLE, "[0-9]+")]
SSP[,Age:=as.numeric(Age)]

POP=SSP[,.(population=sum(value)),by=c("SSP","ISO3","year","Age")]

setorder(POP,ISO3,SSP,year,Age)
fyears=c(2020:2100)
popyear=POP[,approx(year,population,fyears),by=c("ISO3","SSP","Age")]
setnames(popyear,c("x","y"),c("year","pop"))
popyear[,poptotal:=sum(pop),by=c("year","ISO3","SSP")]
fAge=c(0:100)
popyear_age=popyear[,approx(Age,pop,fAge),by=c("ISO3","SSP","year","poptotal")]
setnames(popyear_age,c("x","y"),c("age","pop"))
popyear_age=popyear_age[,pop_no:=sum(pop),by=c("ISO3","SSP","year")]
popyear_age=popyear_age[,pop:=pop*poptotal/pop_no]

popyear_age_output=popyear_age[,c("SSP","ISO3","age","year","pop")]
output=dcast(popyear_age_output,
             SSP+ISO3+age ~ year,
             value.var = c("pop"))
write.csv(output,file="Age structure.csv")