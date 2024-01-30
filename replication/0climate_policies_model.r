####
## Estimate climate policy liberalism model
####

## Requires you to already have cmdstanr installed on your machine
## See: https://mc-stan.org/cmdstanr/index.html. Install with lines below.
## install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# library(cmdstanr)
# install_cmdstan()

## Load tidyverse
library(tidyverse)

## Install the Dynamic Bayesian Measurement Models package
devtools::install_github("devincaughey/dbmm")
## Load the Dynamic Bayesian Measurement Models package
library(dbmm)

options(mc.cores = parallel::detectCores())

## Read the data
policies_data <- read.csv("policies_long.csv")
policies_data<-filter(policies_data, variable!="environment_utility_deregulation_21" & !is.na(abb)& !is.na(value_real))
glimpse(policies_data)
unique(policies_data$variable)

## Shape the data for the model
shaped_data <- shape_data(
    long_data = policies_data,
    unit_var = "abb",
    time_var = "year",
    item_var = "variable",
    value_var = "value_real",
    standardize = TRUE,
    periods_to_estimate = 1998:2020
)

## Run the latent factor model 
fitted_model <- fit(
    data = shaped_data,
    n_dim = 1,
    chains = 4,
    parallelize_within_chains = TRUE,
    threads_per_chain = 2,
    constant_alpha = TRUE,
    separate_eta = TRUE,
    init_kappa = FALSE,
    force_recompile = FALSE,
    iter_warmup = 500,
    iter_sampling = 500,
    adapt_delta = .9,
    refresh = 10,
    seed = 123
)

## Extract draws
raw_draws <- extract_draws(fitted_model)

## Rotate draws for identification
id_draws <- identify_draws(raw_draws)
id_summ <- summary(id_draws$id_draws)
summary(id_summ)

lab_draws <- label_draws(id_draws)

### Make some graphs to visualize the estimates
lab_draws$eta %>%
    group_by(UNIT) %>%
    summarise(est = mean(value), err = sd(value)) %>%
     ungroup()%>%
   mutate(UNIT = reorder(UNIT, -est)) %>%
    ggplot(aes(x = est, y = UNIT)) +
    geom_pointrange(aes(xmin = est - 1.96*err, xmax = est + 1.96*err)) +
    labs(
        title = "State policy scores",
        x = "Policy score (95% CI)",
        y = NULL
    )
    
lab_draws$eta %>%
    group_by(UNIT,TIME) %>%
    summarise(est = mean(value), err = sd(value)) %>%filter(TIME==2020) %>%
    ungroup()%>%
    mutate(UNIT = reorder(UNIT, -est)) %>%
    ggplot(aes(x = est, y = UNIT)) +
    geom_pointrange(aes(xmin = est - 1.96*err, xmax = est + 1.96*err)) +
    labs(
        title = "State policy scores",
        x = "Policy score (95% CI)",
        y = NULL
    )
    
    
## Output a summary of the estimates
estimates_all=lab_draws$eta %>%
    group_by(UNIT,TIME) %>%
    summarise(est = mean(value), std_err = sd(value)) %>%
     rename(abb=UNIT, year=TIME)%>%
     mutate(year=as.numeric(as.vector(year)))%>%
   filter(year>1998)
write.csv(estimates_all, file="policy_index_outputs/state_climate_policy_liberalism.csv")
    

## Output the full posterior
posterior=lab_draws$eta %>%
    select(-name, -time, -unit)%>%
    rename(abb=UNIT, year=TIME)%>%
     mutate(year=as.numeric(as.vector(year)))%>%
   filter(year>1998)
#saveRDS(posterior, file="policy_index_outputs/state_climate_policy_liberalism_posterior.RDS")

## Output a sample of the full posterior
posterior=lab_draws$eta %>%
    select(-name, -time, -unit)%>%
    rename(abb=UNIT, year=TIME)%>%
     mutate(year=as.numeric(as.vector(year)))%>%
    filter(year>1998)%>% 
    group_by(.draw)%>%nest()%>%ungroup()%>%slice_sample(n=200)%>%unnest(cols = c(data)) #https://stackoverflow.com/questions/37149649/randomly-sample-groups
saveRDS(posterior, file="policy_index_outputs/state_climate_policy_liberalism_posterior_sample.RDS")


# discrimination parameters
discrimination_ordinal<-lab_draws$lambda_ordinal %>%
    group_by(ITEM, dim) %>%
    summarise(est = mean(value), se=sd(value))# %>%
   # ungroup() %>%
   # pivot_wider(names_from = dim, values_from = est, names_prefix = "est_") %>%
   # ggplot(aes(x = est_1, y = ITEM)) +
   # geom_point()
discrimination_binary<-lab_draws$lambda_binary %>%
    group_by(ITEM, dim) %>%
    summarise(est = mean(value), se=sd(value))
 discrimination_metric<-lab_draws$lambda_metric %>%
    group_by(ITEM, dim) %>%
    summarise(est = mean(value), se=sd(value))
 discrimination<-rbind(discrimination_binary,discrimination_ordinal,discrimination_metric )  
 discrimination<-arrange(discrimination, est)
 saveRDS(discrimination, file="policy_index_outputs/state_climate_policy_liberalism_discrimination.RDS")
 
 
 
 ### Robustness checks, leaving one policy at a time out

policies_data <- read.csv("policies_long.csv")
policies_data<-filter(policies_data, variable!="environment_utility_deregulation_21" & !is.na(abb)& !is.na(value_real))
glimpse(policies_data)

policies<-unique(policies_data$variable)
for(i in 1:length(policies)){

policy<-policies[i]

## Shape the data for the model, leaving one policy out of each model run
shaped_data <- shape_data(
    long_data = filter(policies_data, variable!=policy),
    unit_var = "abb",
    time_var = "year",
    item_var = "variable",
    value_var = "value_real",
    standardize = TRUE,
    periods_to_estimate = 1998:2020
)

## Run the latent factor model 
fitted_model <- fit(
    data = shaped_data,
    n_dim = 1,
    chains = 4,
    parallelize_within_chains = TRUE,
    threads_per_chain = 2,
    constant_alpha = TRUE,
    separate_eta = TRUE,
    init_kappa = FALSE,
    force_recompile = FALSE,
    iter_warmup = 500,
    iter_sampling = 500,
    adapt_delta = .9,
    refresh = 10,
    seed = 123
)
## Extract draws
raw_draws <- extract_draws(fitted_model)

## Rotate draws for identification
id_draws <- identify_draws(raw_draws)

id_summ <- summary(id_draws$id_draws)
summary(id_summ)

lab_draws <- label_draws(id_draws)
    
estimates_all=lab_draws$eta %>%
    group_by(UNIT,TIME) %>%
    summarise(est = mean(value), std_err = sd(value)) %>%
     rename(abb=UNIT, year=TIME)%>%
     mutate(year=as.numeric(as.vector(year)))%>%
   filter(year>1998)
#write.csv(estimates_all, file=paste0("robustness_checks/state_climate_policy_liberalism_",policy,".csv"))
    
posterior=lab_draws$eta %>%
    select(-name, -time, -unit)%>%
    rename(abb=UNIT, year=TIME)%>%
     mutate(year=as.numeric(as.vector(year)))%>%
    filter(year>1998)%>% 
    group_by(.draw)%>%nest()%>%ungroup()%>%slice_sample(n=200)%>%unnest(cols = c(data)) #https://stackoverflow.com/questions/37149649/randomly-sample-groups
saveRDS(posterior, file=paste0("robustness_checks/state_climate_policy_liberalism_posterior_sample_drop_",policy,".RDS"))


}