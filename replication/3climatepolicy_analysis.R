# this code runs regression analyses presented in the main paper 
# set your working directory to this replication folder 

# Preamble #### 
rm(list=ls())
# library(lfe)
library(sandwich)
library(texreg)
library(dotwhisker)
library(xtable)
library(broom)
library(modelsummary)
library(ggpubr)
library(foreign)
library(fixest)
library(tidyverse)
set.seed(1234)

## set up figure folder here
figures<-"figures/"

lag.new <- function(x, n = 1L, along_with){
 index <- match(along_with - n, along_with, incomparable = NA)
 out <- x[index]
 attributes(out) <- attributes(x)
 out
}

# load data #### 
load("cleaned_data_for_analysis.Rda")
analysis2<-analysis2%>%filter(year!="1999")
analysis2<-dplyr::select(analysis2,-bea_region,-bea_region_code)

# pull in full posterior of policy estimates
policy_posterior<-readRDS("policy_index_outputs/state_climate_policy_liberalism_posterior_sample.RDS")
policy_posterior<-policy_posterior%>%
  select(abb,year,value,.iteration)%>%
  rename(policy=value,
         iteration=.iteration)

## transform data for regressions: 
dvs<-analysis2%>%select(-abb,-year,-climate,-std_err,-NAME,-pop,-climate_lag1,-State,-rps_target,-fips,-state,-census_region,-bearegion_year,-region_year)
dvs<-names(dvs)


## find variables with 0 minima, and add a tiny amount to them, then re-run regressions
dvs2<-list()
for (i in 1:length(dvs)){
  print(dvs[i])
  if(min(analysis2[,dvs[i]],na.rm=TRUE)==0)
    dvs2[[i]]<-dvs[i]
}
dvs2<-unlist(dvs2)
dvs<-dvs[dvs%in%dvs2==FALSE]
## log all variables: 
analysis2<-analysis2%>%
  select(-contains("rps"),-State)%>%
  ## log all variables
  # mutate(across(all_of(c(currvarnames,lagvarnames)),
  #               ~log(.+1)))%>% ### NOTE THAT ALL DV'S ARE ALREADY LOGGED IN THIS DATASET. 
  mutate(across(all_of(dvs),~log(.)),
         across(all_of(dvs2),~log(.+0.000000001)))
analysis2<-analysis2%>%select(-c("jobs_current","wages_current","gdp_current"))

# Unadjusted regression results with state, regionXyear FEs #### 
sryfe<-feols(c(co2percapita_current,co2totaleiapercapita_current,co2_current,co2totaleia_current,
               coalgen_current,coalprod_current,energyprod_current,gasprod_current,renewableprod_current,
               solargen_current,gasgen_current,hydrogen_current,renewgen_current,totalgen_current,windgen_current,
               coalcons.enduse_current,electricity.retailsales.enduse_current,ffcons_current,
               gascons.enduse_current,renewcons_current,solarcons.enduse_current,totalcons_current,
               totalcons.enduse_current,windcons.enduse_current,electricityprice_current,
               gdppercapita_current,wagesperworker_current,jobspercapita_current)~scale(climate_lag1)|abb+region_year,
             vcov=~abb+region_year,data=analysis2)
## rename DVs 
sryfe<-coeftable(sryfe)%>%
  mutate(lhs=case_when(lhs=="coalgen_current"~"Coal elec. gen.",
                       lhs=="coalprod_current"~"Coal energy prod.",
                       lhs=="energyprod_current"~"Total energy prod.",
                       lhs=="gasprod_current"~"Gas energy prod.",
                       lhs=="renewableprod_current"~"Ren. energy prod.",
                       lhs=="solargen_current"~"Solar elec. gen.",
                       lhs=="gasgen_current"~"Gas elec. gen.",
                       lhs=="hydrogen_current"~"Hydro elec. gen.",
                       lhs=="renewgen_current"~"Ren. elec. gen.",
                       lhs=="totalgen_current"~"Total elec. prod.",
                       lhs=="windgen_current"~"Wind elec. gen.",
                       lhs=="coalcons.enduse_current"~"End-use coal cons.",
                       lhs=="electricity.retailsales.enduse_current"~"End-use elec. cons. (retail)",
                       lhs=="ffcons_current"~"Fossil fuel cons.",
                       lhs=="gascons.enduse_current"~"End-use gas cons.",
                       lhs=="renewcons_current"~"Ren. energy cons.",
                       lhs=="solarcons.enduse_current"~"End-use solar energy cons.",
                       lhs=="totalcons_current"~"Total elec. cons.",
                       lhs=="totalcons.enduse_current"~"Total end-use elec. cons.",
                       lhs=="windcons.enduse_current"~"End-use wind energy cons.",
                       lhs=="co2_current"~"co2 (electricity)",
                       lhs=="co2totaleia_current"~"co2 (total)",
                       lhs=="electricityprice_current"~"Electricity price",
                       lhs=="jobs_current"~"Jobs",
                       lhs=="wages_current"~"Wages",
                       lhs=="gdp_current"~"GDP",
                       lhs=="gdppercapita_current"~"GDP per capita",
                       lhs=="wagesperworker_current"~"Wages per worker",
                       lhs=="jobspercapita_current"~"Jobs per capita",
                       lhs=="co2percapita_current"~"co2 (electricity) per capita",
                       lhs=="co2totaleiapercapita_current"~"co2 (total) per capita",
                       TRUE~lhs))
results<-sryfe%>%
  select(lhs,Estimate,`Std. Error`,`t value`)%>%
  rename("Estimate (unadjusted)"="Estimate",
                 "Std. error (unadjusted)"="Std. Error",
                 "T statistic (unadjusted)"="t value",
                 "DV"="lhs")

# state-year FE results #### 
syfe<-feols(c(co2percapita_current,co2totaleiapercapita_current,co2_current,co2totaleia_current,
               coalgen_current,coalprod_current,energyprod_current,gasprod_current,renewableprod_current,
               solargen_current,gasgen_current,hydrogen_current,renewgen_current,totalgen_current,windgen_current,
               coalcons.enduse_current,electricity.retailsales.enduse_current,ffcons_current,
               gascons.enduse_current,renewcons_current,solarcons.enduse_current,totalcons_current,
               totalcons.enduse_current,windcons.enduse_current,electricityprice_current,
               gdppercapita_current,wagesperworker_current,jobspercapita_current)~scale(climate_lag1)|abb+year,
             vcov=~abb+year,data=analysis2)
## rename DVs 
syfe<-coeftable(syfe)%>%
  mutate(lhs=case_when(lhs=="coalgen_current"~"Coal elec. gen.",
                       lhs=="coalprod_current"~"Coal energy prod.",
                       lhs=="energyprod_current"~"Total energy prod.",
                       lhs=="gasprod_current"~"Gas energy prod.",
                       lhs=="renewableprod_current"~"Ren. energy prod.",
                       lhs=="solargen_current"~"Solar elec. gen.",
                       lhs=="gasgen_current"~"Gas elec. gen.",
                       lhs=="hydrogen_current"~"Hydro elec. gen.",
                       lhs=="renewgen_current"~"Ren. elec. gen.",
                       lhs=="totalgen_current"~"Total elec. prod.",
                       lhs=="windgen_current"~"Wind elec. gen.",
                       lhs=="coalcons.enduse_current"~"End-use coal cons.",
                       lhs=="electricity.retailsales.enduse_current"~"End-use elec. cons. (retail)",
                       lhs=="ffcons_current"~"Fossil fuel cons.",
                       lhs=="gascons.enduse_current"~"End-use gas cons.",
                       lhs=="renewcons_current"~"Ren. energy cons.",
                       lhs=="solarcons.enduse_current"~"End-use solar energy cons.",
                       lhs=="totalcons_current"~"Total elec. cons.",
                       lhs=="totalcons.enduse_current"~"Total end-use elec. cons.",
                       lhs=="windcons.enduse_current"~"End-use wind energy cons.",
                       lhs=="co2_current"~"co2 (electricity)",
                       lhs=="co2totaleia_current"~"co2 (total)",
                       lhs=="electricityprice_current"~"Electricity price",
                       lhs=="jobs_current"~"Jobs",
                       lhs=="wages_current"~"Wages",
                       lhs=="gdp_current"~"GDP",
                       lhs=="gdppercapita_current"~"GDP per capita",
                       lhs=="wagesperworker_current"~"Wages per worker",
                       lhs=="jobspercapita_current"~"Jobs per capita",
                       lhs=="co2percapita_current"~"co2 (electricity) per capita",
                       lhs=="co2totaleiapercapita_current"~"co2 (total) per capita",
                       TRUE~lhs))
results.syfe<-syfe%>%
  select(lhs,Estimate,`Std. Error`,`t value`)%>%
  rename("Estimate (unadjusted)"="Estimate",
         "Std. error (unadjusted)"="Std. Error",
         "T statistic (unadjusted)"="t value",
         "DV"="lhs")
# regression results with measurement error correction ####
sryfe.moc.all<-NA
for (p in 1:100){
	print(p)
	iter.sample<-sample(unique(policy_posterior$iteration),1) 
	policy_posterior2<-filter(policy_posterior, iteration==iter.sample)%>% ## sample from the posterior distribution
	  group_by(abb)%>%
	  mutate(policy_lag1=lag.new(policy,n=1,along_with=year))%>%
	  ungroup()
	analysis_boot<-analysis2%>%
	  mutate(year=as.numeric(year))%>%
	  left_join(policy_posterior2,by=c("abb","year"))
	sryfe.moc<-feols(c(co2percapita_current,
	                   co2totaleiapercapita_current,co2_current,co2totaleia_current,coalgen_current,coalprod_current,energyprod_current,gasprod_current,renewableprod_current,
	                   solargen_current,gasgen_current,hydrogen_current,renewgen_current,totalgen_current,windgen_current,
	                   coalcons.enduse_current,electricity.retailsales.enduse_current,ffcons_current,
	                   gascons.enduse_current,renewcons_current,solarcons.enduse_current,totalcons_current,
	                   totalcons.enduse_current,windcons.enduse_current,electricityprice_current,
	                   gdppercapita_current,wagesperworker_current,jobspercapita_current)~scale(policy_lag1)|abb+region_year,
	                 vcov=~abb+region_year,data=analysis_boot)
	sryfe.moc<-coeftable(sryfe.moc)
	sryfe.moc<-sryfe.moc%>%
	  mutate(lhs=case_when(lhs=="co2percapita_current"~"co2 (electricity) per capita",
	                       lhs=="co2totaleiapercapita_current"~"co2 (total) per capita",
	                       lhs=="co2_current"~"co2 (electricity)",
	                       lhs=="co2totaleia_current"~"co2 (total)",
	                       lhs=="coalgen_current"~"Coal elec. gen.",
	                      lhs=="coalprod_current"~"Coal energy prod.",
	                      lhs=="energyprod_current"~"Total energy prod.",
	                      lhs=="gasprod_current"~"Gas energy prod.",
	                      lhs=="renewableprod_current"~"Ren. energy prod.",
	                      lhs=="solargen_current"~"Solar elec. gen.",
	                      lhs=="gasgen_current"~"Gas elec. gen.",
	                      lhs=="hydrogen_current"~"Hydro elec. gen.",
	                      lhs=="renewgen_current"~"Ren. elec. gen.",
	                      lhs=="totalgen_current"~"Total elec. prod.",
	                      lhs=="windgen_current"~"Wind elec. gen.",
	                      lhs=="coalcons.enduse_current"~"End-use coal cons.",
	                      lhs=="electricity.retailsales.enduse_current"~"End-use elec. cons. (retail)",
	                      lhs=="ffcons_current"~"Fossil fuel cons.",
	                      lhs=="gascons.enduse_current"~"End-use gas cons.",
	                      lhs=="renewcons_current"~"Ren. energy cons.",
	                      lhs=="solarcons.enduse_current"~"End-use solar energy cons.",
	                      lhs=="totalcons_current"~"Total elec. cons.",
	                      lhs=="totalcons.enduse_current"~"Total end-use elec. cons.",
	                      lhs=="windcons.enduse_current"~"End-use wind energy cons.",
	                      lhs=="electricityprice_current"~"Electricity price",
	                      lhs=="jobs_current"~"Jobs",
	                      lhs=="wages_current"~"Wages",
	                      lhs=="gdp_current"~"GDP",
	                      lhs=="gdppercapita_current"~"GDP per capita",
	                      lhs=="wagesperworker_current"~"Wages per worker",
	                      lhs=="jobspercapita_current"~"Jobs per capita",
	                      TRUE~lhs))
	sryfe.moc<-sryfe.moc%>%
	  mutate(boot=p,
	         estimate2=rnorm(n=dim(sryfe.moc)[1],mean=Estimate,sd=`Std. Error`))## draw estimate from standard normal with mean=beta, sd=std.error of beta
	sryfe.moc.all<-rbind(sryfe.moc.all, sryfe.moc)
}
sryfe.moc.all<-sryfe.moc.all%>%
  filter(!is.na(lhs))%>%
  group_by(lhs)%>%
  summarise(estimate.moc=mean(estimate2),
            std.error.moc=sd(estimate2),
            statistic.moc=estimate.moc/std.error.moc)%>%
  ungroup()

results.moc<-sryfe.moc.all%>%
  select(estimate.moc,std.error.moc,statistic.moc,lhs)%>%
  mutate(ci.low=estimate.moc-1.96*std.error.moc,
         ci.high=estimate.moc+1.96*std.error.moc,
         ci.low90=estimate.moc-1.64*std.error.moc,
         ci.high90=estimate.moc+1.64*std.error.moc)

## merge together with unadjusted results and save table
# table S1: all results with and without measurement error corrections ####
moctab<-sryfe.moc.all%>%
  select(lhs,estimate.moc,std.error.moc,statistic.moc)%>%
  rename("Estimate (corrected)"="estimate.moc",
         "Std. error (corrected)"="std.error.moc",
         "T statistic (corrected)"="statistic.moc",
         "DV"="lhs")
moctab<-left_join(results,moctab,by="DV")%>%
  rename("Dependent variable"="DV")
print.xtable(xtable(moctab,digits=3),include.rownames=FALSE)


## contextualize effects (described in text, no table) #### 
## what is change across time period in each state? 
deltas<-analysis2%>%select(abb,year,climate)%>%
  filter(year=="2000"|year=="2020")%>%
  distinct()%>%
  pivot_wider(names_from="year",values_from="climate")%>%
  mutate(diff0020=`2000`-`2020`)
mean(deltas$diff0020) ## average state increased stringency by 1.76 standard deviations between 2000 and 2020
sryfe.moc.all[sryfe.moc.all$lhs=="co2 (electricity) per capita",] ## effect on per-capita electricity: 0.0395
mean(deltas$diff0020)*(sryfe.moc.all[sryfe.moc.all$lhs=="co2 (electricity) per capita",2] *100)## 6.97 5/5/23

## what's this within state overtime shift equivalent to, between states, in 2020? 
View(analysis2%>%filter(year=="2020")%>%select(abb,climate)%>%distinct()%>%arrange(desc(climate)))

## categorize variables for splitting into panels for figures
results.moc<-results.moc%>%
  mutate(vartype=case_when(grepl("prod.",lhs)==TRUE~"production",
                           grepl("cons.",lhs)==TRUE~"consumption",
                           grepl("gen.",lhs)==TRUE~"generation",
                           lhs%in%c("co2 (total)","so2","nox","co2 (electricity)")~"emissions",
                           lhs%in%c("Jobs","Wages","GDP")~"economy",
                           lhs%in%c("Electricity price","Jobs per capita","Wages per worker","GDP per capita")~"economy.percap",
                           lhs%in%c("co2 (total) per capita","co2 (electricity) per capita")~"emissions.percap"))
results.moc<-rename(results.moc,DV=lhs)
## do this with unadjusted results too, to put in SI
results<-results%>%
  mutate(vartype=case_when(grepl("prod.",DV)==TRUE~"production",
                           grepl("cons.",DV)==TRUE~"consumption",
                           grepl("gen.",DV)==TRUE~"generation",
                           DV%in%c("co2 (total)","so2","nox","co2 (electricity)")~"emissions",
                           DV%in%c("Jobs","Wages","GDP")~"economy",
                           DV%in%c("Electricity price","Jobs per capita","Wages per worker","GDP per capita")~"economy.percap",
                           DV%in%c("co2 (total) per capita","co2 (electricity) per capita")~"emissions.percap"))

# Figure 5: regression results #### 
## emissions results #### 
mainplot<-
  results.moc%>%
  filter(DV=="co2 (total) per capita"|DV=="co2 (electricity) per capita")%>%
  mutate(DV=ifelse(DV=="co2 (total) per capita","co2 (total)\nper capita","co2 (electricity)\nper capita"),
         sig=case_when(abs(statistic.moc)>1.64&abs(statistic.moc)<1.96~'sig90',
                              abs(statistic.moc)>1.96~'sig95',
                              abs(statistic.moc)<=1.64~'null'),         
         estimate.moc=estimate.moc*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>% ## multiply by 100 so that effect is shown in percent units
  ggplot(aes(x=estimate.moc,y=DV))+
  geom_pointrange(aes(x=estimate.moc,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate.moc,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="Effect of climate policy on CO2 emissions (%)",y="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")
mainplot

## economic indicators per capita #### 
econplot2<-results.moc%>%
  filter(vartype=="economy.percap")%>% 
  mutate(sig=case_when(abs(statistic.moc)>1.64&abs(statistic.moc)<1.96~'sig90',
                       abs(statistic.moc)>1.96~'sig95',
                       abs(statistic.moc)<=1.64~'null'),
         estimate=estimate.moc*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>% ## b/c data aren't logged.
  ggplot(aes(x=estimate,y=DV))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate.moc,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="",y="",shape="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")
econplot2

## energy production and consumption ####
eplot<-results.moc%>%
  filter(DV%in%c("Total elec. cons.","Total elec. prod.",
                 "Total end-use elec. cons.","Total energy prod.",
                 "End-use elec. cons. (retail)"))%>%
  mutate(sig=case_when(abs(statistic.moc)>1.64&abs(statistic.moc)<1.96~'sig90',
                       abs(statistic.moc)>1.96~'sig95',
                       abs(statistic.moc)<=1.64~'null'),
         estimate=estimate.moc*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>%
  ggplot(aes(x=estimate,y=DV))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="",y="",shape="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(text=element_text(size=8))
eplot

mechplot<-results.moc%>%
  filter(vartype=="production"|vartype=="consumption"|vartype=="generation",
         DV%in%c("Total elec. cons.","Total elec. prod.",
                 "Total end-use elec. cons.","Total energy prod.",
                 "End-use elec. cons. (retail)")==FALSE)%>%
  mutate(sig=case_when(abs(statistic.moc)>1.64&abs(statistic.moc)<1.96~'sig90',
                       abs(statistic.moc)>1.96~'sig95',
                       abs(statistic.moc)<=1.64~'null'))%>%
  mutate(estimate=estimate.moc*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>%
  ggplot(aes(x=estimate,y=DV))+#,color=model,shape=model))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="",y="",shape="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(text=element_text(size=8))
mechplot
mechplot2<-ggarrange(eplot,mechplot,common.legend=TRUE,legend="right")


## Figure 5: combined figure with all regression results ####
ggsave(file=paste0(figures,"fig5_results_all.pdf"),width=6.5,height=4,
                   ggarrange(mainplot+labs(x="")+ggtitle("Carbon dioxide emissions per capita")+
                               theme(axis.text.y=element_text(size=6),
                                     axis.text.x=element_text(size=6),
                                     plot.title=element_text(size=6,hjust=.5)),
                             eplot+ggtitle("Energy production and consumption")+
                               theme(legend.position="none",axis.text.y=element_text(size=6),
                                     axis.text.x=element_text(size=6),
                                     plot.title=element_text(size=6,hjust=.5)),
                             econplot2+ggtitle("Economic indicators")+
                               theme(axis.text.y=element_text(size=6),
                                     axis.text.x=element_text(size=6),
                                     plot.title=element_text(size=6,hjust=.5)),
                             mechplot+ggtitle("Energy production and consumption by source")+
                               theme(legend.position="none",axis.text.y=element_text(size=6),
                                     axis.text.x=element_text(size=4),
                                     plot.title=element_text(size=6,hjust=.5)),
                             nrow=2,ncol=2,labels="auto",font.label=list(size=6)))


# robustness checks: #### 
#regressions with leave-one-out model results, with error correction #### 
# loop to pull in full posterior distributions of policy estimates
mod.dir<-"robustness_checks/"
models<-list.files(mod.dir)
posteriors<-list()
for (i in 1:length(models)){
  p<-readRDS(paste0(mod.dir,models[i]))
  p<-p%>%
    select(abb,year,value,`.iteration`)
  posteriors[[i]]<-p
}
robustmods.moc<-list()
for(i in 1:length(models)){
  print(i)
  modname=models[i]
  modname=gsub("state_climate_policy_liberalism_posterior_sample_drop_","",modname)
  modname=gsub(".RDS","",modname)
  print(modname)
  robustmods.moc[[i]]<-NA
  for (j in 1:length(unique(posteriors[[i]]$.iteration))){
    print(j)
    iter.sample<-sample(unique(posteriors[[i]]$.iteration),1) 
    policy_posterior<-filter(posteriors[[i]], .iteration==iter.sample)%>% ## sample from the posterior distribution
      group_by(abb)%>%
      mutate(policy_lag1=lag.new(value,n=1,along_with=year))%>%
      ungroup()
    analysis_boot<-left_join(analysis2,policy_posterior,by=c("abb","year"))
    robustmods<-feols(co2percapita_current~policy_lag1
                      |abb+region_year,
                      vcov=~abb+region_year,data=analysis_boot)
    
    # extract coefficients and standard errors
    robustmods<-data.frame(coeftable(robustmods))
    robustmods$boot<-j ## record the index for the bootstrap run
    robustmods<-robustmods%>%mutate(rhs=modname) ## record name of rhs variable
    robustmods$estimate2<-rnorm(n=dim(robustmods)[1],mean=robustmods$Estimate,sd=robustmods$`Std..Error`)
    
    
    robustmods.moc[[i]]<-rbind(robustmods.moc[[i]],robustmods)
  }
  robustmods.moc[[i]]$model<-modname
}
robustmods.moc<-do.call(rbind,robustmods.moc)
sort(unique(robustmods.moc$model))
robustmods.moc<-robustmods.moc%>%
  filter(estimate2!="NA")%>%
  group_by(model)%>%
  summarise(estimate.moc=mean(estimate2),
            std.error.moc=sd(estimate2),
            statistic.moc=estimate.moc/std.error.moc)%>%
  ungroup()%>%
  mutate(ci.low=estimate.moc-1.96*std.error.moc,
         ci.high=estimate.moc+1.96*std.error.moc,
         ci.low90=estimate.moc-1.64*std.error.moc,
         ci.high90=estimate.moc+1.64*std.error.moc,
         model=tolower(model),
         model=case_when(model=='z_gasoline_tax'~"Gas tax",
                       model=='x_rps_targets_bindingonly'~"Binding RPS target",
                       model=='climate_action_plan_21'~"Climate action plan",
                       model=='community_solar'~"Community solar",
                       model=='environment_ca_car_emissions_standards_21'~"CA car emissions standards",
                       model=='environment_ghg_cap_21'~"Greenhouse gas cap",
                       model=='environment_preemption_naturalgasbans'~"State preemption of local gas bans",
                       model=='environment_publicbenefit_funds_21'~"Public benefit fund",
                       # model=='environment_utility_deregulation_21'~"Utility deregulation",
                       model=='fgd_21'~"Fuel generation mix disclosure",
                       model=='ghg_standards_21'~"Emissions performance standards",
                       model=='netmeter_yearadopted_21'~"On-site renewable generation",
                       model=='pace_21'~"PACE authorization",
                       model=='public_building_standards'~"Environmental building standards",
                       model=='w_complete_streets_21'~"Complete streets policies",
                       model=='w_ee_21'~"Energy efficiency target",
                       model=='w_environment_solar_taxcredit_21'~"Solar tax credit",
                       model=='w_environment_state_nepas_21'~"State NEPA",
                       model=='w_gg_rr_21'~"GHG registry/ reporting",
                       model=='w_ghg_targets_21'~"GHG target",
                       model=='w_low_income_ee_21'~"Low-income energy efficiency program",
                       model=='w_mgpo_21'~"Mandatory green power option",
                       model=='w4_electric_decoupling_21'~"Electric decoupling",
                       model=='w4_environment_state_rps_21'~"RPS",
                       model=='w4_gas_decoupling_21'~"Gas decoupling",
                       model=='x_eers'~"Energy efficiency resource standard"))

## Figure S6: regressions with leave-one-out model results #### 
robustplot.moc<-
  robustmods.moc%>%
  mutate(ci.high=ci.high*100,
         ci.low=ci.low*100,
         estimate.moc=estimate.moc*100)%>%
  ggplot(aes(x=estimate.moc,y=model))+
  geom_pointrange(aes(x=estimate.moc,xmin=ci.low,xmax=ci.high),position=position_dodge(.75),size=.5)+
  labs(x="Effect of climate policy on\nelectricity-sector CO2 emissions (%)",y="Policy left out of index")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")
robustplot.moc
ggsave(file=paste0(figures,"figs6_robustness_leaveoneout_moc.pdf"),width=6.5,height=3.5,robustplot.moc)

# state-regionxyear FE models with additional controls for lagged economic variables #### 
## read in posterior again since it got rewritten in previous loop 
# pull in full posterior of policy estimates
policy_posterior<-readRDS("policy_index_outputs/state_climate_policy_liberalism_posterior_sample.RDS")
policy_posterior<-policy_posterior%>%
  select(abb,year,value,.iteration)%>%
  rename(policy=value,
         iteration=.iteration)


sryfe.robust.all<-NA
sryfe.robust.econ<-NA
for (p in 1:100){
  print(p)
  iter.sample<-sample(unique(policy_posterior$iteration),1)
  policy_posterior2<-filter(policy_posterior, iteration==iter.sample)%>% ## sample from the posterior distribution
    group_by(abb)%>%
    mutate(policy_lag1=lag.new(policy,n=1,along_with=year))%>%
    ungroup()
  analysis_boot<-analysis2%>%
    mutate(year=as.numeric(year))%>%
    left_join(policy_posterior2,by=c("abb","year"))
  sryfe.robust<-feols(c(co2percapita_current,co2totaleiapercapita_current)~
                        scale(policy_lag1)+gdppercapita_lag1+jobspercapita_lag1+
                        wagesperworker_lag1|abb+region_year,
                   vcov=~abb+region_year,data=analysis_boot)
  sryfe.robust<-coeftable(sryfe.robust)%>%
    filter(coefficient=="scale(policy_lag1)")
  sryfe.robust<-sryfe.robust%>%
    mutate(lhs=case_when(lhs=="co2percapita_current"~"co2 (electricity) per capita",
                         lhs=="co2totaleiapercapita_current"~"co2 (total) per capita",
                         TRUE~lhs))
  sryfe.robust<-sryfe.robust%>%
    mutate(boot=p,
           estimate2=rnorm(n=dim(sryfe.robust)[1],mean=Estimate,sd=`Std. Error`))## draw estimate from standard normal with mean=beta, sd=std.error of beta
  sryfe.robust.all<-rbind(sryfe.robust.all, sryfe.robust)
  
  sryfe.econ<-feols(c(jobspercapita_current,wagesperworker_current,gdppercapita_current,electricityprice_current)~
                      scale(policy_lag1)+unionizationrate_lag1+gdppercapita_lag1+jobspercapita_lag1+wagesperworker_lag1|
                      abb+region_year,vcov=~abb+region_year,data=analysis_boot)
  sryfe.econ<-coeftable(sryfe.econ)%>%
    filter(coefficient=="scale(policy_lag1)")
  sryfe.econ<-sryfe.econ%>%
    mutate(lhs=case_when(lhs=="gdppercapita_current"~"GDP per capita",
                         lhs=="wagesperworker_current"~"Wages per worker",
                         lhs=="jobspercapita_current"~"Jobs per capita",
                         lhs=="electricityprice_current"~"Electricity price"))
  sryfe.econ<-sryfe.econ%>%
    mutate(boot=p,
           estimate2=rnorm(n=dim(sryfe.econ)[1],mean=Estimate,sd=`Std. Error`))
  sryfe.robust.econ<-rbind(sryfe.robust.econ,sryfe.econ)
  sryfe.robust.all<-rbind(sryfe.robust.all,sryfe.robust.econ)
}
sryfe.robust.all<-sryfe.robust.all%>%
  filter(!is.na(lhs),coefficient=="scale(policy_lag1)")%>%
  group_by(lhs)%>%
  summarise(estimate=mean(estimate2),
            std.error=sd(estimate2),
            statistic=estimate/std.error)%>%
  ungroup()

results.robust<-sryfe.robust.all%>%
  select(estimate,std.error,statistic,lhs)%>%
  mutate(ci.low=estimate-1.96*std.error,
         ci.high=estimate+1.96*std.error,
         ci.low90=estimate-1.64*std.error,
         ci.high90=estimate+1.64*std.error)

mainplot.withcontrols<-
  results.robust%>%
  rename(DV=lhs)%>%
  filter(DV%in%c("co2 (total) per capita","co2 (electricity) per capita"))%>%
  mutate(DV=ifelse(DV=="co2 (total) per capita","co2 (total)\nper capita","co2 (electricity)\nper capita"),
         sig=case_when(abs(statistic)>1.64&abs(statistic)<1.96~'sig90',
                       abs(statistic)>1.96~'sig95',
                       abs(statistic)<=1.64~'null'),         
         estimate=estimate*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>% ## multiply by 100 so that effect is shown in percent units
  ggplot(aes(x=estimate,y=DV))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="Effect of climate policy on CO2 emissions (%)",y="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")
mainplot.withcontrols

## plot with main results with controls #### 
results.robust<-results.robust%>%mutate(mod="With additional controls")%>%
  rename(DV=lhs)%>%
  mutate(vartype=case_when(grepl("prod.",DV)==TRUE~"production",
                           grepl("cons.",DV)==TRUE~"consumption",
                           grepl("gen.",DV)==TRUE~"generation",
                           DV%in%c("co2 (total)","so2","nox","co2 (electricity)")~"emissions",
                           DV%in%c("Jobs","Wages","GDP")~"economy",
                           DV%in%c("Electricity price","Jobs per capita","Wages per worker","GDP per capita")~"economy.percap",
                           DV%in%c("co2 (total) per capita","co2 (electricity) per capita")~"emissions.percap"))
results.moc<-results.moc%>%
  mutate(mod="State, region x year FE")%>%
  rename(estimate=estimate.moc,
         std.error=std.error.moc,
         statistic=statistic.moc)
results.robust<-rbind(results.robust,results.moc)
results.robust.plot<-results.robust%>%
  filter(DV%in%c("co2 (total) per capita","co2 (electricity) per capita"))%>%
  mutate(DV=ifelse(DV=="co2 (total) per capita","co2 (total)\nper capita","co2 (electricity)\nper capita"),
         sig=case_when(abs(statistic)>1.64&abs(statistic)<1.96~'sig90',
                       abs(statistic)>1.96~'sig95',
                       abs(statistic)<=1.64~'null'),         
         estimate=estimate*100,ci.low=ci.low*100,ci.high=ci.high*100,
         ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>% ## multiply by 100 so that effect is shown in percent units
  ggplot(aes(x=estimate,y=DV))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="Effect of climate policy on CO2 emissions (%)",y="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")+
  facet_wrap(~mod)+
  theme(strip.background=element_blank())
results.robust.plot
ggsave(paste0(figures,"figs7_robustness_mainplot_withcontrols.pdf"),width=6.5,height=3.5,results.robust.plot)

econplot.withcontrols<-
  results.robust%>%
  filter(DV%in%c("GDP per capita","Wages per worker","Jobs per capita","Electricity price"))%>%
  mutate(sig=case_when(abs(statistic)>1.64&abs(statistic)<1.96~'sig90',
                  abs(statistic)>1.96~'sig95',
                  abs(statistic)<=1.64~'null'),         
    estimate=estimate*100,ci.low=ci.low*100,ci.high=ci.high*100,
    ci.low90=ci.low90*100,ci.high90=ci.high90*100)%>% ## multiply by 100 so that effect is shown in percent units
  ggplot(aes(x=estimate,y=DV))+
  geom_pointrange(aes(x=estimate,xmin=ci.low90,xmax=ci.high90,shape=sig),position=position_dodge(.75),size=.5)+
  geom_linerange(aes(x=estimate,xmin=ci.low,xmax=ci.high),lwd=.1)+
  scale_shape_manual(values=c("sig90"=1,"sig95"=8,"null"=16))+
  labs(x="Effect of climate policy on economic indicators",y="")+
  geom_vline(xintercept=0,lty="dotted")+
  theme_classic()+
  theme(legend.position = "none")+
  facet_wrap(~mod)+
  theme(strip.background=element_blank())
econplot.withcontrols
ggsave(file=paste0(figures,"figs8_robustness_econresults_withcontrols.pdf"),width=6.5,height=3.5,econplot.withcontrols)

