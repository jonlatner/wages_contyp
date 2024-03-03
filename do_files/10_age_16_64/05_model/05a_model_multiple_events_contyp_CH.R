# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package,character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/age_16_64/"
results = "results/age_16_64/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(broom)
library(broom.helpers)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

# clean data -----

df_original <- df_original %>%
  filter(country == "CH")

df_original <- df_original %>%
        select(country,pid,pidseq,year,age,ln_hourly_wage,unmp,temp,perm,event_t_p_yes_final,event_t_p_time_pos,unemployment_rate) %>%
        filter(age >= 20) %>%
        filter(unmp == 0)  %>%
        group_by(country, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        ungroup() %>%
        filter(max>2) 


df_problem <- df_original %>%
  select(pidseq,age,year,ln_hourly_wage,temp,perm,event_t_p_yes_final,event_t_p_time_pos) %>%
  filter(age < 25) %>%
  filter(event_t_p_yes_final==1) %>%
  filter(event_t_p_time_pos == 2 | event_t_p_time_pos == 3) %>%
  group_by(pidseq) %>%
  mutate(diff = ln_hourly_wage-dplyr::lag(ln_hourly_wage),
         diff = last(diff)) %>%
  ungroup() %>%
  filter(diff>1) %>%
  mutate(problem = 1) %>%
  select(pidseq,problem)
df_problem <- unique(df_problem)

df_merge <- merge(df_original,df_problem,all.x = TRUE) %>%
  filter(is.na(problem))

# Prepare for models ----

df_yhat <- data.frame()

# Countries
country_ann <- c("CH")

# Annual countries ----
for(c in country_ann) {
        df_country <- df_original %>%
                filter(country == c)
        
        # FE model
        model_fe <- feis(ln_hourly_wage ~ age + unemployment_rate + temp | 1,
                         data = data.frame(df_country), 
                         robust = TRUE,
                         id = "pid")
        
        df_output <- tidy_parameters(model_fe)
        df_output$model <- "FE"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)

        # FEIS model
        model_feis <- feis(ln_hourly_wage ~ unemployment_rate + temp | age,
                           data = data.frame(df_country), 
                           robust = TRUE,
                           id = "pid")
        
        df_output <- tidy_parameters(model_feis)
        df_output$model <- "FEIS"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Save output ----

df_yhat %>% filter(term == "temp") 
