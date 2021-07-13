# Compute country-level social cost of carbon
# The code is created based on the Ricke et al. https://github.com/country-level-scc/cscc-paper-2018 
# Outputs are expressed in USD 2018

library(data.table)
library(docopt)
library(openxlsx)
library(triangle)
library(dplyr)
# Future years
impulse_year = 2020
fyears <- impulse_year:2100

distribution=read.xlsx("income distribution.xlsx")%>%setDT()
distribution=melt(distribution,
                  id.vars=c("ISO3"),
                  value.name=c("income_ratio"),
                  variable.name=c("project_age"),
                  variable.factor=FALSE)
distribution[,project_age:=as.numeric(project_age)]

region_allocate=read.xlsx("Region.xlsx")

lifeyears=0:100

lifeexpect=setDT(read.xlsx("LifeExpectancy.xlsx",sheet="Data",rows=2:19,cols=5:26));
lifeexpect=lifeexpect[,lyear:=seq(2020,2100,by=5)]
setnames(lifeexpect,"100+","100")

lifeexpect=melt(lifeexpect,
                id.vars=c("lyear"),
                value.name = c("expectancy"),
                variable.name = c("age"),
                variable.factor = FALSE)

lifeexpect2=lifeexpect[,approx(lyear,expectancy,fyears),by=c("age")]
setnames(lifeexpect2,c("x","y"),c("year","expectancy"))

lifeexpect3=lifeexpect2[,approx(age,expectancy,lifeyears),by=c("year")]
setnames(lifeexpect3,c("x","y"),c("age","expectancy"))

lifeexpect_country=read.xlsx('Life expectancy at birth.xlsx')
n <- 9

lifeexpect_country=do.call("rbind", replicate(n, lifeexpect_country, simplify = FALSE))
setDT(lifeexpect_country)
base=as.numeric(lifeexpect3[age==0 & year==2020,3])
lifeexpect_country=lifeexpect_country[,r:=Life/base]

ffyears=seq(2020,2100,by=10)
lifeexpect_country2=lifeexpect_country[,year:=ffyears,by=c("r")]

life_expectancy=merge(lifeexpect_country2,lifeexpect3,by=c("year"),allow.cartesian = TRUE)
life_expectancy=life_expectancy[,.(ISO3=ISO,year=year,age=age,expectancy=expectancy*r)]
life_expectancy=life_expectancy[,start:=max(year-age,2020),by=c("ISO3","year","age")]
life_expectancy=life_expectancy[,end:=min(year+floor(expectancy),2100),by=c("ISO3","year","age")]
setnames(life_expectancy,c("year"),c("time"))

project_gap=function(SD){
  .net=SD$net>0
  breakeven=sum(.net)-1
  if (breakeven<0)
  {
    breakeven=-10
  }
  return(breakeven)
}

rcpgroup=c("rcp45","rcp60","rcp85")
sspgroup=c("SSP1","SSP2","SSP3","SSP4","SSP5")
for (rcpV in rcpgroup){
  for (sspV in sspgroup){
'usage: generate_cscc.R -s <ssp> -c <rcp> [ -r <runid> -p <type> -l <clim> -f <name>] [-a] [-o] [-d] [-w]

options:
 -s <ssp>   SSP baseline (random(default), SSP1, SSP2,..., SSP5)
 -c <rcp>   RCP (random(default), rcp45, rcp60, rcp85)
 -r <runid> Bootstart run for the damage function parameter, 0 is estimates (0<=id<=1000)
 -p <type>  projection type (constant (default),horizon2100)
 -l <clim>  climate models (ensemble (default), mean[-ensemble])
 -o         does not allow for out-of-sample damage prediction (default, allows)
 -d         rich/poor damage function specification (default, pooled)
 -a         5-lag damage function specification (default, 0-lag)
 -f <name>  damage function (default=bhm (Burke et al.), djo (Dell et al.))
 -w         save raw data' -> doc

opts <- docopt(doc, "-s SSP4 -c rcp60 -r 1 -l mean -w")

# Some tests
#opts <- docopt(doc, "-s SSP2 -c rcp60 -w") # Default case
#opts <- docopt(doc, "-s SSP3 -c rcp85 -r 1 -w -a -d")
#opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -l mean -w -a -d")
#opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -w -d -f djo")

t0 <- Sys.time()

# GLOBAL VARIABLES
if (is.null(opts[["s"]])) {
  ssp = sample(paste0("SSP",1:5),1) # SSP{1,2,3,4,5}
} else {
  ssp = sspV
}
if (is.null(opts[["c"]])) {
  .rcp = sample(c("rcp26","rcp45","rcp60","rcp85"),1)
} else {
  .rcp = rcpV
}
if (is.null(opts[["r"]])) {
  dmg_func = "estimates" # dmg function
  runid = 0
} else {
  print(paste("r:",opts['r']))
  runid = as.integer(max(0,min(1000,as.numeric(opts['r']))))
  if (runid == 0) {
    dmg_func = "estimates"
  }else{
    dmg_func = paste0("bootstrap")
  }
}
if (is.null(opts[["p"]])) {
  project_val = "constant" # growth rate constant
} else {
  project_val = as.character(opts["p"])
}
if (is.null(opts[["l"]])) {
  clim = "ensemble"
} else {
  clim = "mean"
  if (runid != 0) {
    dmg_func = "bootstrap"
    runid = 1:1000
  }
}
if (is.null(opts[["f"]])) {
  dmg_ref = "bhm"
} else {
  dmg_ref = as.character(opts["f"])
}

out_of_sample = !opts[['o']]
rich_poor = opts[['d']]
lag5 = opts[['a']]
save_raw_data = opts[['w']]

very_last_year = 2100

preffdir = "res"
dollar_scale = 1e12 # convert to trillion
reftemplastyear = F

if (dmg_ref == "djo") {
  rich_poor = T
  out_of_sample = T
  lag5 = F
  dmg_func = "estimates"
  reftemplastyear = T
}

# Print simulation paramters
print(paste("SSP: ",ssp))
print(paste("RCP: ",.rcp))
print(paste("dmg_func: ",dmg_func))
print(paste("last year: ",very_last_year))
print(paste("prefix dir: ",preffdir))
print(paste("climate ensemble: ",clim))
print(paste("impulse year: ",impulse_year))
print(paste("projection post2100: ",project_val))
print(paste("out_of_sample: ",out_of_sample))
print(paste("richpoor: ",rich_poor))
print(paste("LR (lag5): ", lag5))
print(paste("damage function:",dmg_ref))

if (dmg_ref == "bhm") {
  dmg_ref = ""
}else{
  dmg_ref = paste0("_",dmg_ref)
}

resdir = paste0(preffdir,"_stat",dmg_ref)
if (!out_of_sample) {resdir = paste0(resdir,"_30C")}
if (rich_poor) {resdir = paste0(resdir,"_richpoor")}
if (lag5) {resdir = paste0(resdir,"_lr")}

resboot = paste0(preffdir,"_boot")
if (!out_of_sample) {resboot = paste0(resboot,"_30C")}
if (rich_poor) {resboot = paste0(resboot,"_richpoor")}
if (lag5) {resboot = paste0(resboot,"_lr")}

if (dmg_func == "bootstrap" & clim == "ensemble") {
  ddd = file.path(resboot,paste0(ssp,"-",.rcp))
  filename = file.path(ddd,paste0("store_scc_",project_val,"_",runid,dmg_ref,".RData"))
  if (file.exists(filename)) {
    stop("already computed")
  }
}

# Load data 
source("modules/gdpssp.R")
if (dmg_ref == "") {
  source("modules/bhm_replica.R")
} else {
  source("modules/djo_replica.R")
}
source("modules/cmip5.R")
print(Sys.time() - t0)

# All combination of models available (CC x GCM for each RCP)

temp_Burke=read.csv('temp.csv')
temp_new=temp_Burke[.rcp]
rcp26_new=temp_Burke["rcp26"];
basetemp_new=temp_Burke["basetemp"];



# Project all scenarios
project_gdpcap <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdpcap_cc <- SD$gdpcap
  
  .gdprate <- SD$gdpr
  .gdprate_cc <- SD$gdpr
  
  
  .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  .gdpcap_tm1_cc <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
  
  .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
  if (!reftemplastyear) {.ref_temp <- SD$basetemp[1]}
  
  for (i in seq_along(c(fyears))) {
    # 2 degree climate change
    .gdprate[i] <- SD$gdpr[i] + warming_effect(SD$rcp26_temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
    .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
    .gdpcap_tm1 <- .gdpcap[i]
    
    # With climate change
    .gdprate_cc[i] <- SD$gdpr[i] + warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
    .gdpcap_cc[i] <- .gdpcap_tm1_cc * (1 + .gdprate_cc[i])
    .gdpcap_tm1_cc <- .gdpcap_cc[i]
    
    if (reftemplastyear) {.ref_temp <- SD$temp[i]}
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdpcap_cc = .gdpcap_cc,
              gdprate_cc = .gdprate_cc
  ))
}




lcscc = NULL
lwscc = NULL
lcohort=NULL

lcohort_gap=NULL

for (nid in runid) {
  
  region_gdp=data.table(c("OECD90","ASIA","LAM","MAF","EIT"),c(rtriangle(1,0.4,0.6,0.5),
                                                               rtriangle(1,1.2,1.6,1.5),
                                                               rtriangle(1,0.9,1.1,1),
                                                               rtriangle(1,1.8,2.8,2.3),
                                                               rtriangle(1,1.4,2.5,2)))
  
  setnames(region_gdp,c("V1","V2"),c("Region","ratio"))
  region_GDP=merge(region_gdp,region_allocate[,1:2],by=c("Region"))

  
  GDP_world=c(rtriangle(1,0.6,1.5,1.1),
              rtriangle(1,1.3,4.8,1.7),
              rtriangle(1,2.3,4.0,3.4),
              rtriangle(1,3.3,9.3,5.0))
  
  GDP=approx(c(2020,2030,2050,2100),GDP_world,fyears)
  GDP=setDT(GDP)
  setnames(GDP,c("x","y"),c("year","mitigation"))
  

    ssp_gr <- growthrate[SSP == ssp & year %in% fyears]
  
  if (clim == "ensemble") {
    ssp_temp <- ctemp[rcp == "rcp45" & year %in% fyears]
  } else {
    ssp_temp <- etemp[rcp == "rcp45" & year %in% fyears]
  }
  
  ssp_temp = merge(ssp_temp,basetemp,by = c("ISO3")) # add basetemp
  
  ssp_gdpr <- merge(ssp_gr,ssp_temp,by = c("ISO3","year")) # merge growth rate and temp
  ssp_gdpr = merge(ssp_gdpr, sspgdpcap[SSP == ssp & year == fyears[1]],
                   by = c("SSP","ISO3","year"),all.x = T) # add gdpcap0
  
  
  miss_val_iso3 <- unique(ssp_gdpr[year == impulse_year & is.na(gdpcap),ISO3])
  ssp_gdpr <- ssp_gdpr[!ISO3 %in% miss_val_iso3]
  
  ssp_gdpr <- ssp_gdpr[,.(model_id = nid,ISO3,year,temp,basetemp,gdpr,gdpcap)]
  
  setkey(ssp_gdpr,model_id,ISO3)
  
  ssp_gdpr[,temp:=temp_new[[1]]]
  ssp_gdpr[,rcp26_temp:=rcp26_new[[1]]]
  ssp_gdpr[,basetemp:=basetemp_new[[1]]]
  
  print(Sys.time() - t0)
  
  res_scc <- ssp_gdpr[,project_gdpcap(.SD),by = c("model_id","ISO3")]
  print(Sys.time() - t0)
  
  # yearly population 
  popyear <- pop[SSP == ssp,approx(year,pop,fyears),by = c("SSP","ISO3")]
  setnames(popyear,c("x","y"),c("year","pop"))
  res_scc <- merge(res_scc,popyear,by = c("ISO3","year"))
  res_scc<-merge(res_scc,region_GDP,by=c("ISO3"))
  res_scc<-merge(res_scc,GDP,by=c("year"))
  setorder(res_scc,"ISO3")
  
  print(Sys.time() - t0)
  
  # create main table for world
  res_wscc <- res_scc[,.(gdpcap_cc = weighted.mean(gdpcap_cc,pop)),
                      by = c("year",c("model_id"),"SSP")]
  
  # Compute average annual growth rate of per capita consumption between now and year t
  # for the computation of discount factor
  #countries
  gdprate_cc_impulse_year = res_scc[year == impulse_year,
                                    .(gdpcap_cc_impulse_year = gdpcap_cc),
                                    by = c("model_id","ISO3")]
  
  res_scc <- merge(res_scc,gdprate_cc_impulse_year,by = c("model_id","ISO3"))
  res_scc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                     gdprate_cc,
                                     (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
  
  #World
  gdprate_cc_impulse_year = res_wscc[year == impulse_year,
                                     .(gdpcap_cc_impulse_year = gdpcap_cc),
                                     by = c("model_id")]
  res_wscc <- merge(res_wscc,gdprate_cc_impulse_year,
                    by = c("model_id"))
  res_wscc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                      NA,
                                      (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
  res_wscc = merge(res_wscc,res_wscc[year == (impulse_year + 1),
                                     .(model_id,gdprate_cc_avg_impulse_year = gdprate_cc_avg)],
                   by = "model_id")
  res_wscc[year == impulse_year,gdprate_cc_avg := gdprate_cc_avg_impulse_year]
  res_wscc[,gdprate_cc_avg_impulse_year := NULL]
  
  print(Sys.time() - t0)
  
  # Compute social benefits of climate change mitigation
  res_scc[, scc := (-gdpcap_cc+gdpcap)  * 1.26*(1e6 / dollar_scale)] # $2005/tCO2  **I delete impulse here to calculate total GDP loss
  res_scc[, cgdp := (gdpcap_cc) *1.26 * (1e6 / dollar_scale)] #GDP under climate change
  res_scc[, mcc := cgdp*mitigation/100*ratio] #mitigation cost of climate change
  
  sum_res_scc = res_scc[, .(scc = sum(scc)), 
                        by = c("year",c("model_id"))]
  res_wscc = merge(res_wscc,sum_res_scc,
                   by = c("year",c("model_id")))
  
  
  
  prtps = c(1,2) # When using the fixed discount, set etas=0 
  etas = c(2) 
  
 
  cohort_scc=NULL
  cohort_gap=NULL
  for (.prtp in prtps) {
    for (.eta in etas) {
      dscc = res_scc[,list(ISO3,model_id,year,gdprate_cc_avg,scc,mcc)]
      dscc[,dfac := (1/(1 + .prtp/100 + .eta * gdprate_cc_avg)^(year - impulse_year))]
      dscc[,dscc := dfac * scc]
      dscc[,dmcc:=dfac*mcc]
      dscc[,dnet:=dscc-dmcc]
      cohort_scc0=merge(life_expectancy,dscc,by=c("ISO3"),allow.cartesian = TRUE)
      
    #  cohort_scc1=cohort_scc0[year>start-1 & year<end+1,
    #                        .(prtp=.prtp,eta=.eta,benefit=sum(dscc),cost=sum(dmcc),net=sum(dnet)),
    #                         by=c("model_id","ISO3","time","age")]
      cohort_scc0=cohort_scc0[year>start-1 & year<end+1,]
      cohort_scc0[,project_age:=year-time+age]
      cohort_scc0=merge(cohort_scc0,distribution,by=c("ISO3","project_age"))
      cohort_scc1=cohort_scc0[,.(prtp=.prtp,eta=.eta,benefit=sum(dscc*income_ratio),cost=sum(dmcc*income_ratio),net=sum(dnet*income_ratio)),
                              by=c("model_id","ISO3","time","age")]
      cohort_scc=rbind(cohort_scc,cohort_scc1[time==2020,])
      
      cohort_gap=rbind(cohort_gap,cohort_scc1[,.(prtp=.prtp,eta=.eta,gap=project_gap(.SD)),by=c("model_id","ISO3","time")])
     
      }
  }
  
  drs = c(3,5) #%
  
  cohort_scc_d=NULL
  cohort_gap_d=NULL
 
  for (.dr in drs) {
    dscc = res_scc[,list(ISO3,model_id,year,gdprate_cc_avg,scc,mcc)]
    dscc[,dfac := (1/(1 + .dr/100 * gdprate_cc_avg)^(year - impulse_year))]
    dscc[,dscc := dfac * scc]
    dscc[,dmcc:=dfac*mcc]
    dscc[,dnet:=dscc-dmcc]
    cohort_scc0=merge(life_expectancy,dscc,by=c("ISO3"),allow.cartesian = TRUE)
    
    cohort_scc0=cohort_scc0[year>start-1 & year<end+1,]
    cohort_scc0[,project_age:=year-time+age]
    cohort_scc0=merge(cohort_scc0,distribution,by=c("ISO3","project_age"))
    cohort_scc1=cohort_scc0[,.(dr=.dr,benefit=sum(dscc*income_ratio),cost=sum(dmcc*income_ratio),net=sum(dnet*income_ratio)),
                            by=c("model_id","ISO3","time","age")]
    
  #  cohort_scc1=cohort_scc0[year>start-1 & year<end+1,
  #                          .(dr=.dr,benefit=sum(dscc),cost=sum(dmcc),net=sum(dnet)),
  #                          by=c("model_id","ISO3","time","age")]
    
    cohort_scc_d=rbind(cohort_scc_d,cohort_scc1[time==2020,])
    
    cohort_gap_d=rbind(cohort_gap_d,cohort_scc1[,.(dr=.dr,gap=project_gap(.SD)),by=c("model_id","ISO3","time")])
    
  }
  
 cohort_scc = rbindlist(list(cohort_scc_d,cohort_scc),fill = T)
 cohort_gap = rbindlist(list(cohort_gap_d,cohort_gap),fill = T)
 
  print(Sys.time() - t0)
  
  lcohort = rbind(lcohort,cohort_scc)
  lcohort_gap = rbind(lcohort_gap,cohort_gap)
  
}

result_25=lcohort[,lapply(.SD,quantile, probs = c(0.25)),by=c("ISO3","prtp","eta","dr","age","time"),
                  .SDcols=c("benefit","cost","net")]
setnames(result_25,c("benefit","cost","net"),c("benefit_25","cost_25","net_25"))

result_75=lcohort[,lapply(.SD,quantile, probs = c(0.75)),by=c("ISO3","prtp","eta","dr","age","time"),
                  .SDcols=c("benefit","cost","net")]
setnames(result_75,c("benefit","cost","net"),c("benefit_75","cost_75","net_75"))

result_median=lcohort[,lapply(.SD,median),by=c("ISO3","prtp","eta","dr","age","time"),
                      .SDcols=c("benefit","cost","net")]

result=merge(result_median,result_25,by=c("ISO3","prtp","eta","dr","age","time"))

result=merge(result,result_75,by=c("ISO3","prtp","eta","dr","age","time"))


gap_25=lcohort_gap[,lapply(.SD,quantile, probs = c(0.25)),by=c("ISO3","prtp","eta","dr","time"),
                   .SDcols=c("gap")]
setnames(gap_25,c("gap"),c("gap_25"))

gap_75=lcohort_gap[,lapply(.SD,quantile, probs = c(0.75)),by=c("ISO3","prtp","eta","dr","time"),
                   .SDcols=c("gap")]
setnames(gap_75,c("gap"),c("gap_75"))

gap_median=lcohort_gap[,lapply(.SD,median),by=c("ISO3","prtp","eta","dr","time"),
                       .SDcols=c("gap")]
gap=merge(gap_median,gap_25,by=c("ISO3","prtp","eta","dr","time"))

gap=merge(gap,gap_75,by=c("ISO3","prtp","eta","dr","time"))



 write.csv(result,file=paste0("uncertainty","_",.rcp,"_",ssp,".csv"))

 write.csv(gap,file=paste0("gap","_",.rcp,"_",ssp,".csv")) 

  }
}

# dscc1=dscc
# setorder(dscc1,ISO3,year)
# 
# dscc1=dscc1[,.(ISO3,year,dcgdp)]
# 
# dscc_GDP=dcast(dscc1,
#             ISO3 ~ year,
#             value.var = c("dcgdp"))
# 
# dscc1=dscc1[,.(ISO3,year,dscc)]
# 
# dscc_SCC=dcast(dscc1,
#                ISO3 ~ year,
#                value.var = c("dscc"))
# 
# dscc1=dscc1[,.(ISO3,year,dscc)]
# 
# dscc_MCC=dcast(dscc1,
#                ISO3 ~ year,
#                value.var = c("dmcc"))
# 
# write.csv(cscc,file=paste0("GDP","_",.rcp,"_",ssp,".csv"))



