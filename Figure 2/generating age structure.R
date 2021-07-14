library(openxlsx)
library(data.table)
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
