## this code creates the descriptive figures presented in the paper. 
# set your working directory to this replication folder 

# Preamble #### 
rm(list=ls())
library(foreign)
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
library(ggforce)
library(ggpubr)
library(usmap)
library(colorRamps)
library(RColorBrewer)
library(ggmap)
library(sf)
library(tigris)
library(tidycensus)
library(tidyverse)

# set name of figure folder, for saving 
figures<-"figures/"

lag.new <- function(x, n = 1L, along_with){
  index <- match(along_with - n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}

# load data #### 
load("cleaned_data_for_analysis.Rda")
analysis2<-analysis2%>%filter(as.numeric(year)>1999)
analysis2<-dplyr::select(analysis2,-bea_region, -bea_region_code)
policies<-read.csv("policies_updated.csv")

# Figure 1: time series plot ####
## Find change across time period in average state
deltas<-analysis2%>%select(abb,year,climate)%>%
  filter(year=="2000"|year=="2020")%>%
  distinct()%>%
  pivot_wider(names_from="year",values_from="climate")%>%
  mutate(diff0020=`2020`-`2000`)
View(deltas%>%arrange(diff0020))
mean(deltas$diff0020) ## average state increased stringency by 1.76 standard deviations between 1999 and 2019

## what's this difference equivalent to in 2020? 
View(analysis2%>%filter(year=="2020")%>%select(abb,climate)%>%distinct()%>%arrange(desc(climate)))

## summarize national average
resultsum<-analysis2%>%
  group_by(year)%>%
  summarise(climate=mean(climate))%>%
  mutate(abb="US")%>%
  ungroup()

## create a value to position text labels for 2020 and 2000
analysis2<-analysis2%>%
  mutate(lab.yr=case_when(year==2000~year-.5,
                          year==2020~year+.5))

lineplot.context<-
  analysis2%>%
  ggplot(aes(x=year,y=climate,group=abb,color=abb))+
  geom_line(data=resultsum,inherit.aes=TRUE,color="black",lwd=2)+
  geom_line(data=subset(analysis2,abb%in%c("CA","AZ","VA","WV")),
                        inherit.aes=TRUE)+
  labs(x="Year",y="Climate policy stringency")+
  geom_text(data=subset(analysis2,(year==2020)&
                          abb%in%c("CA","WV")),
            aes(x=lab.yr,y=climate,label=abb),size=3,color="black"#,
            # position=position_jitter(width=.5)
            )+
  geom_text(data=subset(analysis2,year==2020&abb=="VA"),
            aes(x=lab.yr,y=climate+.3,label=abb),size=3,color="black")+
  geom_text(data=subset(analysis2,year==2020&abb=="AZ"),
            aes(x=lab.yr,y=climate+.15,label=abb),size=3,color="black")+
  geom_text(data=subset(resultsum,year==2020),aes(x=2020+0.5,y=climate,label=abb),color="black",size=3)+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=8))
lineplot.context 
ggsave(file=paste0(figures,"fig1_timeseries_context.pdf"),width=6.5,height=3.5,units=c("in"),lineplot.context)

# Fig. S1 supplementary info time series plot #### 
lineplot.supp<-
  analysis2%>%
  ggplot(aes(x=year,y=climate,group=abb))+
  geom_line(data=resultsum,inherit.aes=TRUE,color="black",lwd=2)+
  geom_line(aes(color=abb),alpha=.8)+
  labs(x="Year",y="Climate policy stringency")+
  geom_text(data=subset(analysis2,(year==2000|year==2020)&
                          abb%in%c("NY","CA","VA","DE","AZ","IA","ND","WV")),
            aes(x=lab.yr,y=climate,label=abb),size=2,color="black",
            position=position_jitter(width=.25,height=0.1))+
  scale_color_manual(values=primary.colors(n=51))+
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=8))
lineplot.supp 
ggsave(file=paste0(figures,"figs1_timeseries_supp.pdf"),width=6.5,height=3.5,units=c("in"),lineplot.supp)


# Figure 2: map #### 
mdata<-analysis2%>%select(abb,year,climate)
states <- tigris::states(cb = T,resolution = "20m")
states <- tigris::shift_geometry(states,geoid_column="STATEFP")%>% # this shifts Hawaii and Alaska over
  left_join(mdata,by=c("STUSPS"="abb"))%>%
  filter(STUSPS!="PR",(year%in%c("2000","2007","2014","2020")))

fig2map<-
  states%>%filter(NAME%in%c("Alaska","Hawaii")==FALSE)%>%
  ggplot() + 
  geom_sf(col="white",size=.25,aes(fill=climate))+
  scale_fill_distiller(palette="Greens",direction=1)+
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  coord_sf(expand=FALSE)+
  scale_y_continuous()+
    facet_wrap(~year)
fig2map
ggsave(file=paste0(figures,"fig2_map_4panels.pdf"),height=3.5,width=6.5,fig2map)


## map including AK and HI #### 
## the paper only includes the map above, but code below will create a 4-panel map showing AK and HI, without lat/long labels
# fig2mapb<-ggplot(states) + 
#   geom_sf(col="white",size=.25,aes(fill=climate))+
#   scale_fill_distiller(palette="Greens",direction=1)+
#   theme(legend.position="none",
#         axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major = element_line(colour = 'transparent'),
#         panel.grid.minor=element_blank(),plot.background=element_blank())+
#   facet_wrap(~year)
# fig2mapb
# 
# ggsave(file=paste0(figures,"map_4panels_supp.pdf"),height=3.5,width=6.5,fig2mapb)



# Figure 3: descriptive scatterplots of policy inputs and climate policy index ####
## Figure 3 left panel: cross-sectional scatterplot: climate policy and rps #### 
## find correlation between rps targets and policy index: 
r.rps<-cor(analysis2[analysis2$year=="2020","rps_target"],analysis2[analysis2$year=="2020","climate"])

scatter1<-analysis2%>%
  filter(year=="2020")%>%
  ggplot(aes(y=rps_target,x=climate,label=abb))+
  geom_smooth(method="loess",se=FALSE,lty="dashed",lwd=.5)+
  geom_text(size=1.5)+
  annotate("text",x=2,y=-2.5,label=paste0("r = ",round(r.rps,2)),color="blue")+
  theme_classic()+
  labs(y="RPS Target in 2020",x="Climate policy stringency in 2020")
scatter1

## Figure 3 right panel: Number of policies in 2020 and climate policy in 2020 ####
### create df with the number of climate policies per state-year #### 
rps.0<-policies%>%
  filter(year=="2020")%>%
  select(-z_gasoline_tax,-environment_preemption_naturalgasbans,-year)%>%
  pivot_longer(cols=climate_action_plan_21:x_rps_targets_bindingonly,names_to="policy",values_to="value")%>%
  mutate(value=ifelse(value!=0&!is.na(value),1,0))%>%## binarize policies to count
  filter(value==1)%>%
  group_by(abb)%>%
  dplyr::summarise(n.policies=sum(value),
                   policies=paste(unique(policy),collapse=","))%>%
  arrange(desc(n.policies))%>%
  left_join(data.frame(analysis2%>%
                         filter(year=="2020")%>%
                         select(abb,year,climate)),by=c("abb"))
## find correlation between # of policies and climate policy index
r.polcount<-cor(rps.0$n.policies,rps.0$climate)
### create plot ####
scatter2<-ggplot(rps.0,aes(y=n.policies,x=climate,label=abb))+
  geom_text(size=1.5)+
  geom_smooth(method="loess",se=FALSE,lty="dashed",lwd=.5)+
  annotate("text",x=2,y=3.2,label=paste0("r = ",round(r.polcount,2)),color="blue")+
  labs(y="Number of policies enacted by 2020",x="Climate policy stringency in 2020")+
  theme_classic()
scatter2
ggsave(file=paste0(figures,"fig3_scatter-rps-npolicies-climatepolicy.pdf"),width=6.5,height=3.5,
       ggarrange(scatter1,scatter2,labels="auto",font.label=list(size=10))
)

# Figure 4: descriptive scatterplots of emissions and climate policy ####
## Figure 4 left panel: cross-sectional scatterplot from 2020 #### 
r.xc<-cor(log(analysis2[analysis2$year=="2020","co2percapita_current"]),analysis2[analysis2$year=="2020","climate_lag1"])

xcplot<-analysis2%>%
  filter(year=="2020")%>%
  ggplot(aes(x=climate_lag1,y=log(co2percapita_current),label=abb))+
  geom_text(size=2,position=position_jitter())+
  geom_smooth(method=lm,se=FALSE,lwd=.5,lty="dashed")+
  theme_classic()+
  annotate("text",x=0,y=-3.5,label=paste0("r = ",round(r.xc,2)),color="blue")+
  theme(text=element_text(size=8))+
  labs(x="Climate policy stringency\n(2019)",y="Electricity sector carbon dioxide emissions per capita\n(logged, 2020)")
xcplot

## Figure 4 right panel: change in emissions, 2000-2020 and change in climate policy, 2000-2019 #### 
deltas<-analysis2%>%
  mutate(year=as.numeric(year))%>%
  arrange(abb,year)%>%
  group_by(abb)%>%
  mutate(co2percapita_lag1=lag.new(co2percapita_current,along_with=year))%>%
  ungroup()%>%
  filter(year=="2020"|year=="2019"|year=="2000")%>%
  select(year,abb,co2percapita_current,co2percapita_lag1,climate)%>%
  pivot_wider(names_from=year,values_from=c(co2percapita_current,co2percapita_lag1,climate))%>%
  mutate(co2.delta=100*(co2percapita_lag1_2020-co2percapita_current_2000)/co2percapita_current_2000,
         policy.delta=climate_2019-climate_2000)


## regression with long-term differences ####
# regression of deltas
r.deltas<-cor(deltas$co2.delta,deltas$policy.delta)

deltaplot<-deltas%>%
  ggplot(aes(x=policy.delta,y=co2.delta,label=abb))+
  geom_text(size=2,position=position_jitter())+
  geom_smooth(method="lm",lty="dashed",se=FALSE,lwd=.5)+
  labs(x="Change in climate policy stringency\n(2000-2019)",y="Percent change in electricity sector carbon dioxide\nemissions per capita (2000-2020)")+
  theme_classic()+
  annotate("text",x=1,y=-80,label=paste0("r = ",round(r.deltas,2)),color="blue")+
  theme(text=element_text(size=8))
deltaplot

## save both scatterplots together for figure 4 #### 
ggsave(paste0(figures,"fig4_scatter_2panel.pdf"),width=6.5,height=3.5,ggarrange(xcplot,deltaplot,
                                                                           labels="auto",font.label=list(size=10)))


# Table 1: policy data ####
p<-read_csv("policies_updated.csv")
p<-p%>%
  select(-`...1`)%>%
  pivot_longer(-c(abb,year),values_to="value",names_to="variable")
## binary variable for enactment in a state-year
p<-p%>%
  mutate(adopted=ifelse(!is.na(value)&value!=0,1,0))
## first year in which each policy appears 
years<-p%>%
  filter(adopted!=0,
         year>=2000)%>%
  group_by(variable)%>%
  summarise(year1=min(year),
            year2=max(year))%>%
  mutate(`First year in our data`=year1)%>%
  select(-year2)
## states that have adopted each policy  in first year
state1<-p%>%
  filter(adopted!=0,variable!="z_gasoline_tax")%>%
  right_join(years,by=c("variable","year"="year1"))%>%
  arrange(variable,year,`First year in our data`)%>%
  group_by(variable,year,`First year in our data`)%>%
  summarise(`State(s) adopting by year 1`=paste(unique(abb),collapse=", "))
state1$`State(s) adopting by year 1`[state1$variable=="z_gasoline_tax"]<-NA

## number of states that have adopted each policy at some point
psum<-p%>%group_by(variable,abb)%>%
  summarise(enacted=ifelse(sum(adopted,na.rm=TRUE)>0,1,0))%>%
  group_by(variable)%>%
  summarise(n.enacted=sum(enacted))%>%
  arrange(n.enacted)
names(psum)<-c("Policy","No. states enacting")

## load descriptions and merge in 
codebook<-read_csv("codebooks/codebook_policies_withlinks.csv")
codebook<-codebook%>%
  select(`Code in policies dataset`,`Policy - short description`)%>%
  mutate(`Code in policies dataset`=tolower(`Code in policies dataset`))
psum<-left_join(psum,codebook,by=c("Policy"="Code in policies dataset"))%>%
  left_join(state1,by=c("Policy"="variable"))
psum<-psum%>%select(`Policy - short description`,`State(s) adopting by year 1`,`No. states enacting`)%>%
  rename("Policy"="Policy - short description")
psum<-psum%>%arrange(`Policy`)

psum2<-select(psum,Policy,`No. states enacting`,`State(s) adopting by year 1`)
print.xtable(xtable(psum2,digits=0,label="tab:policy",caption="Policies included in the dataset of state climate policy stringency: The table shows the name of each policy, the number of states that have adopted it, and the number of states that had adopted it by the first year for which we have a record of the policy's adoption. Table \\ref{tab:codebook} provides a longer description of each policy."),include.rownames=F)

# Table S1: Cross-sectional policy stringency #### 
tab1<-analysis2%>%
  filter(year%in%c(2000,2010,2020))%>%
  select(abb,year,climate)%>%
  rename(State=abb,
         "Policy stringency"=climate)%>%
  group_by(year)%>%
  mutate(Rank=dense_rank(desc(`Policy stringency`)))%>%
  ungroup()%>%
  pivot_wider(id_cols=State,names_from=year,values_from=c("Policy stringency",Rank),names_sep=": ")

print.xtable(xtable(tab1,label="tab:estimates",caption="Cross-sectional estimates of climate policy stringency and state rankings (high to low)"),include.rownames=FALSE)

# Fig. S1: Discrimination parameters #### 
codebook<-codebook%>%
  rename("Policy" = "Policy - short description")
disc<-readRDS("policy_index_outputs/state_climate_policy_liberalism_discrimination.RDS")
disc<-disc%>%
  mutate(ci.low=est-1.96*se,
         ci.high=est+1.96*se,
         ITEM=tolower(ITEM))%>%
  rename(estimate=est,
         variable=ITEM)

disc<-left_join(disc,codebook,by=c("variable"="Code in policies dataset"))%>%
  mutate_at(c("ci.low","estimate","ci.high"),as.numeric)
disc<-disc%>%
  left_join(psum,by=c("Policy"))

disc$Policy[disc$variable=="x_eers_zi"]<-gsub("continuous","dichotomous",disc$Policy[disc$variable=="x_eers"])
disc$`No. states enacting`[disc$variable=="x_eers_zi"]<-disc$`No. states enacting`[disc$variable=="x_eers"]

disc<-disc%>%
  mutate(Policy = paste0(Policy, " (",`No. states enacting`,")"))%>%
  mutate(Policy=factor(Policy))
disc$Policy<-fct_reorder(disc$Policy,disc$estimate)

discplot<-disc%>%
  ggplot(aes(x=estimate,y=Policy))+
  geom_pointrange(aes(x=estimate,xmin=ci.low,xmax=ci.high))+
  labs(x="Discrimination parameter estimate",y="")+
  theme_bw()
discplot

ggsave(file=paste0(figures,"figs2_discrimination.pdf"),
       discplot+theme(text=element_text(size=10)),width=6.5,height=3.5)

# Figure S3: ACEEE validation ####
validation<-read_csv("validation_data/aceee_scores.csv")

validation<-select(validation,abb,starts_with('aceee'))
names(validation)[2:ncol(validation)]<-gsub("_","_20",names(validation)[2:ncol(validation)])
names(validation)[2:ncol(validation)]<-parse_number(names(validation)[2:ncol(validation)])
validation<-validation%>%
  pivot_longer(cols=starts_with("20"),names_to="year",values_to="aceee")
validation$year<-as.numeric(validation$year)

results<-analysis2%>%select(abb,year,climate)%>%mutate(year=as.numeric(year))
validationsum<-validation%>%
  left_join(results,by=c("abb","year"))%>%
  group_by(year)%>%
  summarise(cor=cor(climate,aceee,use="complete.obs"))
validation<-left_join(validation,validationsum,by="year")

validation<-left_join(analysis2,validation,by=c("abb","year"))%>%
  select(abb,year,climate,aceee,cor)%>%
  filter(!is.na(cor))
validation<-validation%>%
  filter(year%in%c('2006','2010','2015','2020')==TRUE)

clabels<-data.frame(year=c('2006','2010','2015','2020'),label=c(paste0("r = ",round(unique(validationsum$cor[validationsum$year=='2006']),2)),
                                                        paste0("r = ",round(unique(validationsum$cor[validationsum$year=='2010']),2)),
                                                        paste0("r = ",round(unique(validationsum$cor[validationsum$year=='2015']),2)),
                                                        paste0("r = ",round(unique(validationsum$cor[validationsum$year=='2020']),2))))

ggsave(file=paste0(figures,"figs3_ACEEE_multi.pdf"),width=6.5,height=3.5,
       ggplot(validation,aes(x=climate,y=aceee,label=abb))+
         geom_text(size=2)+
         geom_smooth()+
         geom_text(x=2,y=10,aes(label=label),data=clabels,color="blue")+
         labs(x="Climate policy stringency",y="ACEEE score")+
         theme_bw()+
         facet_wrap(~year)
)

# Figure S4: validation with policy liberalism ####
cw<-read.dta("validation_data/CaugheyWarshaw_DynamicDemocracyAmericanStates_Export_2021.dta")%>%
  filter(year=="2020")%>%
  select(abb,year,policy_updated,policy_updated_se)
# cw$year<-as.character(cw$year)
analysis2<-left_join(analysis2,cw,by=c("abb","year"))
ggplot(analysis2,aes(x=policy_updated,y=climate,label=abb))+
  geom_text()+
  geom_smooth(method="lm")
analysis2sub<-analysis2%>%
  filter(year==2020)
analysis2sub$policy.scaled<-scale(analysis2sub$policy_updated)
analysis2sub$climate.scaled<-scale(analysis2sub$climate)

cw.cor<-cor(analysis2sub$climate.scaled,analysis2sub$policy.scaled)
ggsave(file=paste0(figures,"figs4_liberalism-climate.pdf"),width=6.5,height=3.5,
       ggplot(analysis2sub,aes(x=policy.scaled,y=climate.scaled,label=abb))+
         geom_text()+
         geom_smooth(method="lm")+
         theme_bw()+
         annotate(geom="text",x=1.5,y=0,label=paste0("r = ",round(cw.cor,1)),color="blue")+
         labs(x="Policy liberalism (2020)",y="Climate policy stringency (2020)")+
         theme(text=element_text(size=12))
)

# Figure S5: validation with public ideology #### 
tw_states<-read_csv("validation_data/tw_states_ideology.csv")
tw_states<-left_join(analysis2,tw_states,by=c("abb"))%>%
  select(abb,year,mrp_estimate,climate)%>%
  group_by(abb)%>%
  filter(abb!="DC")%>%
  summarise(climate=mean(climate),mrp_estimate=mean(mrp_estimate))
cors<-tw_states %>%
  summarise(
    cor=cor(mrp_estimate, climate,use="complete.obs"))

ggsave(paste0(figures,"figs5_validation_tw_ideology.pdf"),width=6.5,height=3.5,
ggplot(tw_states, aes(x=climate,y= mrp_estimate, label=abb)) +
  geom_text() + theme_bw()+
  geom_smooth()+
  annotate("text", x = 1, y = .4, label = paste("r = ",round(cors$cor,2),sep=""),color="blue")+
  xlab("Average Climate Policy Stringency")+ylab("State Public Ideology")
)

# table S4: outcomes and covariates data sources #### 
codebook.outcomes<-read_csv("codebooks/codebook_outcomes_forSI_nolinks.csv")
print.xtable(xtable(codebook.outcomes,digits=0,label="tab:outcomescodebook",caption="Sources and descriptions of outcome and covariate data"),include.rownames=F)
