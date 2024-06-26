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

data_files = "data_files/"
results = "results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(broom)
library(broomExtra)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

# clean data -----

# Create new temp variable that equals the addition of the 2 step function variables
# This ensures that models only evaluate events that fit the criteria of an event

df_original <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,event_t_p_yes_final,event_p_t_yes_final,event_t_p_time_pos,event_p_t_time_pos,unemployment_rate) %>%
        filter(unmp == 0)  %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        ungroup() %>%
        filter(max>2) 

# Prepare for models ----

df_yhat <- data.frame()

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")

# Annual countries ----
for(c in country_ann) {
        print(paste0("country = ", c))
        
        df_country <- df_original %>%
                filter(country == c)
        

        # Dummy IF
        df_country$event_t_p_time_pos <- relevel(factor(df_country$event_t_p_time_pos), ref = "2")
        df_country$event_p_t_time_pos <- relevel(factor(df_country$event_p_t_time_pos), ref = "2")
        
        # Temp to perm
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_t_p_time_pos | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_t_p_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "t_p"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)

        # Perm to temp
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_p_t_time_pos | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_p_t_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "p_t"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Biannual countries ----
for(c in country_bi) {
        print(paste0("country = ", c))
        
        df_country <- df_original %>%
                filter(country == c)
        
        # Dummy IF
        df_country$event_t_p_time_pos <- relevel(factor(df_country$event_t_p_time_pos), ref = "1")
        df_country$event_p_t_time_pos <- relevel(factor(df_country$event_p_t_time_pos), ref = "1")
        
        # Temp to perm
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_t_p_time_pos | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_t_p_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "t_p"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
        
        # Perm to temp
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_p_t_time_pos | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_p_t_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "p_t"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Save output ----

saveRDS(df_yhat, file = paste0(results, "df_yhat_first_event_contyp.rds"))

beep()

