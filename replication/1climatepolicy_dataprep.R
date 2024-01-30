# This code loads, merges, and transform data for analyses
## set your working directory to this replication folder

# Preamble #### 
rm(list=ls())
library(lfe)
library(sandwich)
library(texreg)
library(orcutt)
library(sp)
library(grid)
library(rgeos)
library(maptools)
library(grid)
library(ggrepel)
library(dotwhisker)
library(xtable)
library(broom)
library(modelsummary)
library(estimatr)
library(haven)
library(tidyverse)

lag.new <- function(x, n = 1L, along_with){
  index <- match(along_with - n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}
statecodes<-read_csv("covariates/state_codes.csv")%>%select(STATE_CODE,NAME)

# load, transform, and clean data ####
## load climate policy index ####
results<-read_csv("policy_index_outputs/state_climate_policy_liberalism.csv")%>%
  select(-`...1`)%>%
  rename(climate=est)

## emissions data: EIA electricity sector emissions #### 
emissions<-read_csv("outcome_data/EIA_emission_annual_1990-2020.csv")

emissions<-filter(emissions, `Producer Type`=='Total Electric Power Industry')
emissions<-filter(emissions,`Energy Source`=='All Sources')
names(emissions)<-tolower(names(emissions))
names(emissions)
emissions%<>%
  select(-`so2\n(metric tons)`,-`nox\n(metric tons)`)%>%
  rename(abb=state,
         co2_current=`co2\n(metric tons)`)
analysis<-left_join(results, emissions, by=c("abb", "year"))

analysis2<-analysis

# population data #### 
pop0<-read_csv("population/state_population_estimates_9099.csv")
pop1<-read_csv("population/state_population_estimates_2000_2010.csv")
pop2<-read_csv("population/state_population_2010_2022.csv")

pop1<-pop1%>%
  filter(NAME!="United States",SEX==0,AGE==999)%>% ## 0 for sex is total 
  select(NAME,STATE,contains("POPESTIMATE"))%>%
  pivot_longer(-c(NAME,STATE),names_to="year",values_to="pop")%>%
  mutate(year=gsub("POPESTIMATE","",year))%>%
  group_by(year,NAME,STATE)%>%
  summarise(pop=sum(pop))%>%
  ungroup()
pop1<-pop1%>%left_join(statecodes,by="NAME")%>%
  select(-STATE)
unique(pop1$year)
pop2<-pop2%>%
  filter(state%in%c("United States","Northeast","Midwest","South","West")==FALSE)%>% 
  pivot_longer(-state,names_to="year",values_to="pop")%>%
  left_join(statecodes,by=c("state"="NAME"))
pop1<-pop1%>%filter(year!="2010")
pop0<-pop0%>%
  select(state,`1999`)%>%
  left_join(statecodes,by=c("state"="NAME"))%>%
  mutate(year="1999")%>%
  rename('pop'="1999")

pop<-full_join(pop1,pop2,by=c("NAME"="state","year","pop","STATE_CODE"))%>%
  full_join(pop0,by=c("NAME"="state","year","pop","STATE_CODE"))
unique(pop$year)
pop$year<-as.integer(pop$year)
analysis2<-left_join(analysis2,pop,by=c("abb"="STATE_CODE","year"))

analysis2<-group_by(analysis2, abb)%>%
  mutate(climate_lag1=lag.new(climate, n=1, along_with=year))

# eia total energy-related emissions data #### 
emissions2<-read_csv("outcome_data/EIA_energy.csv")%>%
  select(State,`1989`:`2020`)%>%
  pivot_longer(`1989`:`2020`,names_to="year",values_to="co2totaleia_current")
emissions2<-left_join(emissions2,statecodes,by=c("State"="NAME"))%>%
  rename(abb=STATE_CODE)
emissions2$year=as.numeric(emissions2$year)
analysis2<-left_join(analysis2,emissions2,by=c("abb","year"))

# mechanisms #### 
## energy consumption ####
## eia consumption data #### 
cons<-read_csv("outcome_data/EIA_annual_totalconsumption_state.csv") 
cons<-cons%>%
  select(State,MSN,`1990`:`2020`)%>%
  pivot_longer(`1990`:`2020`,names_to="year",values_to="consumption_current")
msn<-read_csv("outcome_data/MSN_codes.csv")%>%select(MSN,Description,Unit)
cons<-cons%>%left_join(msn,by='MSN')
cons<-cons%>%
  filter(MSN%in%c(#"CLTCB",
    "CLTXB",#"ESTCB",
    "ESTXB","FFTCB","NGTXB","RETCB","SOTXB","TETCB","TETXB","WYTXB")) ## code descriptions saved and highlighted in Outcomes/eia_consumption_codes.xlsx
cons<-cons%>%
  select(-Unit,-MSN)%>%
  pivot_wider(names_from="Description",values_from="consumption_current")
cons<-cons%>%rename(
  coalcons.enduse_current="Coal total end-use consumption",
  electricity.retailsales.enduse_current="Electricity total end-use consumption (i.e., retail sales)",
  ffcons_current="Fossil fuels total consumption",
  gascons.enduse_current="Natural gas total end-use consumption",                               
  renewcons_current="Renewable energy total consumption",                               
  solarcons.enduse_current="Solar energy total end-use consumption",
  totalcons_current="Total energy consumption",                      
  totalcons.enduse_current="Total end-use energy consumption",
  windcons.enduse_current="Wind energy total end-use consumption"  
)
cons<-cons%>%
  mutate(coalcons.enduse_current=coalcons.enduse_current*100/totalcons.enduse_current,
         gascons.enduse_current=gascons.enduse_current*100/totalcons.enduse_current,
         renewcons_current=renewcons_current*100/totalcons_current,
         ffcons_current=ffcons_current*100/totalcons_current,
         solarcons.enduse_current=solarcons.enduse_current*100/totalcons.enduse_current,
         windcons.enduse_current=windcons.enduse_current*100/totalcons.enduse_current)


## energy production (total primary) #### 
## codes from data: 
# BFPRP: biofuels production
# CLPRP: coal production (thousand short tons--different unit from others)
# CLPRK: conversion factor for converting thousand short tons to bbtu
# ENPRP: fuel ethanol production
# GETCB: geothermal energy consumption
# HYTCB: hydropower consumption
# NCPRB: noncombustible renewable energy production 
# NUETB: nuclear energy consumed for electricity generation, total 
# REPRB: renewable energy production
# NGMPB: Natural gas marketed production
# NGMPP: natural gas marketed production (this and the above are correlated at 0.9997)
# TEPRB: total primary energy production (Total primary energy production is the sum of the production values for fossil fuels, nuclear electric 
#                                         power, and renewable energy: https://www.eia.gov/totalenergy/data/monthly/pdf/sec1.pdf). 
prod<-readxl::read_excel("outcome_data/EIA_StateEnergyProduction.xlsx",sheet=1)
prod<-select(prod,"StateCode","MSN","1990":"2020")%>% ## UPDATE DATES HERE WHEN GET NEW DATA 
  filter(MSN%in%c("CLPRP","CLPRK","REPRB",#"NCPRB",
                  "TEPRB","NGMPB"))%>%
  pivot_longer(cols="1990":"2020",names_to="year",values_to="bil.btu")
prod<-prod%>%
  pivot_wider(names_from="MSN",values_from="bil.btu")%>%
  mutate(coalprod_current=CLPRP*CLPRK)%>%
  select(-c(CLPRP,CLPRK))
prod<-prod%>%rename(#noncombustible.renew_current=NCPRB,
  renewableprod_current=REPRB,energyprod_current=TEPRB,gasprod_current=NGMPB)

prod$year<-as.numeric(prod$year)
cons$year<-as.numeric(cons$year)
analysis2<-left_join(analysis2,prod,by=c("abb"="StateCode","year"))%>%
  left_join(cons,by=c("abb"="State","year"))

## create percent and lagged versions of these levels
analysis2<-analysis2%>%
  mutate(renewableprod_current=renewableprod_current*100/energyprod_current,
         coalprod_current=coalprod_current*100/energyprod_current,
         gasprod_current=gasprod_current*100/energyprod_current)

## electricity generation  #### 
gen<-read_csv("outcome_data/EIA_annual_generation_state.csv")
gen<-gen%>%
  filter(`TYPE OF PRODUCER`=="Total Electric Power Industry")%>%
  select(-`TYPE OF PRODUCER`)
gen<-gen%>%
  pivot_wider(names_from="ENERGY SOURCE",values_from="GENERATION (Megawatthours)")
names(gen)

gen<-gen%>%select(YEAR,STATE,Total,Coal,`Natural Gas`,Petroleum,Wind,`Solar Thermal and Photovoltaic`,
                  `Hydroelectric Conventional`,`Wood and Wood Derived Fuels`, `Geothermal`,
                  `Other Biomass`)
gen<-gen%>% ### NOTE THAT VARIABLES WITH 'GEN' DENOTE VARIABLES THAT ARE FROM THE ELECTRICITY GENERATION DATASET RATHER THAN PRIMARY ENERGY PRODUCTION 
  rename(totalgen_current=Total,
         coalgen_current=Coal,
         gasgen_current=`Natural Gas`,
         oilgen_current=Petroleum,
         windgen_current=Wind,
         solargen_current=`Solar Thermal and Photovoltaic`,
         hydrogen_current=`Hydroelectric Conventional`,
         woodgen_current=`Wood and Wood Derived Fuels`,
         biomassgen_current=`Other Biomass`,
         geothermalgen_current=`Geothermal`)
gen<-gen%>%
  rowwise()%>%
  mutate(renewgen_current=sum(windgen_current,solargen_current,hydrogen_current,woodgen_current,biomassgen_current,geothermalgen_current,na.rm=TRUE))
analysis2<-left_join(analysis2,gen,by=c("year"="YEAR","abb"="STATE"))

# create percent of disagg generation variables
analysis2<-analysis2%>%
  mutate(coalgen_current=coalgen_current*100/totalgen_current,
         gasgen_current=gasgen_current*100/totalgen_current,
         windgen_current=windgen_current*100/totalgen_current,
         solargen_current=solargen_current*100/totalgen_current,
         hydrogen_current=hydrogen_current*100/totalgen_current,
         renewgen_current=renewgen_current*100/totalgen_current)

analysis2<-analysis2%>%select(-c(`producer type`,`energy source`))
names(analysis2)

## RPS target data #### 
rps<-read_csv("policy_data/x_RPS_targets_bindingonly.csv")
rps<-rps%>%
  pivot_longer(-c(abb,state),values_to="rps_target",names_to="year")%>%
  mutate(year=as.integer(year))%>%
  arrange(abb,year)%>%
  group_by(abb)
rps<-rps%>%select(abb,year,rps_target)

analysis2<-analysis2%>%left_join(rps,by=c("abb","year"))

# census regions #### 
bea_regions<-read.csv("covariates/bea_regions.csv")
analysis2<-merge(analysis2, bea_regions, by="abb", all.x=T)
analysis2$bearegion_year=paste(analysis2$bea_region, analysis2$year, sep="_")
analysis2$census_region<-str_trim(analysis2$census_region)
analysis2$region_year=paste(analysis2$census_region,analysis2$year,sep="_")

# electricity price data #### 

electric<-read_csv("outcome_data/EIA_electricity_price.csv")
electric<-electric%>%
  mutate(abb=substr(gsub("ELEC.PRICE.","",`source key`),1,2))%>%
  select(`2001`:abb)%>%
  pivot_longer(cols=-c(abb),names_to="year",values_to="electricity_price")%>%
  filter(!is.na(electricity_price))
electric$year<-as.integer(electric$year)
analysis2<-left_join(analysis2,electric,by=c("abb","year"))

# jobs and wage data #### 
states<-unique(analysis2$abb)
jobs<-list()
wage<-list()
gdp<-list()
for(i in 1:length(states)){
  jobs[[i]]<-read_csv(paste0("outcome_data/StateEmployment/SAEMP25N_",states[i],"_1998_2020.csv"))%>%
    select(Description,`1999`:`2020`)%>%
    filter(Description=="Total employment (number of jobs)")%>%
    select(-Description)%>%
    mutate_all(as.numeric)%>%
    pivot_longer(everything(),names_to="year",values_to="jobs")%>%
    mutate(abb=states[i])
  wage[[i]]<-read_csv(paste0("outcome_data/StateEmployment/SAINC6N_",states[i],"_1998_2021.csv"))%>%
    select(Description,`1999`:`2021`)%>%
    filter(Description=="Wages and salaries")%>%
    select(-Description)%>%
    mutate_all(as.numeric)%>%
    pivot_longer(everything(),names_to="year",values_to="wages")%>%
    mutate(abb=states[i])
  gdp[[i]]<-read_csv(paste0("outcome_data/StateGDP/SAGDP2N_",states[i],"_1997_2021.csv"))%>%
    select(Description,`1999`:`2021`)%>%
    filter(Description=="All industry total")%>%
    select(-Description)%>%
    mutate_all(as.numeric)%>%
    pivot_longer(everything(),names_to="year",values_to="gdp")%>%
    mutate(abb=states[i])
}
jobs<-do.call(rbind,jobs)
wage<-do.call(rbind,wage)
gdp<-do.call(rbind,gdp)
jobs$year<-as.integer(jobs$year)
wage$year<-as.integer(wage$year)
gdp$year<-as.integer(gdp$year)
analysis2<-left_join(analysis2,jobs,by=c('abb','year'))%>%
  left_join(wage,by=c("abb","year"))%>%
  left_join(gdp,by=c("abb","year"))


## rename new variables for consistency with the other outcomes/ predictors
analysis2<-analysis2%>%
  rename(electricityprice_current=electricity_price,
         jobs_current=jobs,
         wages_current=wages,
         gdp_current=gdp)

## create percapita versions of economic variables #### 
analysis2<-analysis2%>%
  mutate(jobspercapita_current=jobs_current/pop,
         wagesperworker_current=wages_current/jobs_current,
         gdppercapita_current=gdp_current/pop,
         co2percapita_current=co2_current/pop,
         co2totaleiapercapita_current=co2totaleia_current/pop)
analysis2<-analysis2%>%
  group_by(abb)%>%
  arrange(year)%>%
  mutate(year=as.numeric(year))%>%
  mutate(jobspercapita_lag1=lag.new(jobspercapita_current,1,along_with=year),
         wagesperworker_lag1=lag.new(wagesperworker_current,1,along_with=year),
         gdppercapita_lag1=lag.new(gdppercapita_current,1,along_with=year))%>%
  ungroup

# union membership, for robustness checks ####
unions<-read_csv("covariates/union_membership.csv")%>%
  pivot_longer(`2021`:`1964`,values_to="unionizationrate",names_to="year")%>%
  filter(as.numeric(year)>=1990,
         state!="All States")%>%
  mutate(year=as.numeric(year))%>%
  group_by(abb)%>%
  arrange(year)%>%
  mutate(unionizationrate_lag1=lag.new(unionizationrate,1,along_with=year))
analysis2<-left_join(analysis2,unions,by=intersect(names(analysis2),names(unions)))

## remove variables used to create renewgen that we don't use in analyses 
analysis2<-analysis2%>%
  select(-woodgen_current,-geothermalgen_current,-biomassgen_current,-oilgen_current)
  

save(analysis2,file="cleaned_data_for_analysis.Rda")






