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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

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

df_original <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

# clean data -----

df_original <- df_original %>%
        filter(male == 1)

df_event_t_p <- df_original %>%
        select(country,pid,pidseq,year,age,ln_wages,unmp,temp,perm,matches("event_t_p"),unemployment_rate) %>%
        rename(event_time_pos = event_t_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_p <- df_original %>%
        select(country,pid,pidseq,year,age,ln_wages,unmp,temp,perm,matches("event_u_p"),unemployment_rate) %>%
        rename(event_time_pos = event_u_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_t <- df_original %>%
        select(country,pid,pidseq,year,age,ln_wages,unmp,temp,perm,matches("event_u_t"),unemployment_rate) %>%
        rename(event_time_pos = event_u_t_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_p_t <- df_original %>%
        select(country,pid,pidseq,year,age,ln_wages,unmp,temp,perm,matches("event_p_t"),unemployment_rate) %>%
        rename(event_time_pos = event_p_t_time_pos) %>%
        filter(!is.na(event_time_pos))

# examine some data ----

# df_test <- df_event_u_p %>%
#         filter(event_u_p_yes==1 & country == "IT") %>%
#         select(country,pid,pidseq,year,unmp,temp,perm,matches("event"))
# with(df_test,table(event_u_p_time,event_time_pos))

# df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,pidseq,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)
# 

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("t_p","p_t","u_p","u_t")

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
country_all <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")

# Models ---- 

# Annual countries
for(c in country_ann) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "2")
                df_country <- data.frame(df_country)
                model <- feis(ln_wages ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                              data = df_country, 
                              robust = TRUE,
                              id = "pidseq")
                
                # assign(paste0("model_annual_wages_log_male_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_annual_wages_log_male_event_",e,"_country_",c,".Rdata"))
                
                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}


# Biannual countries
for(c in country_bi) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model - baseline is 1 period before event.  in biannual data this is event_time_pos==1
                # To estimate a conventional fixed effects model without individual slopes, please use y ~ x1 + x2 | 1 to indicate that the slopes should only contain an individual-specific intercept.
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "1")
                df_country <- data.frame(df_country)
                model <- feis(ln_wages ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                              data = df_country, 
                              robust = TRUE,
                              id = "pidseq")
                
                # assign(paste0("model_annual_wages_log_male_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_annual_wages_log_male_event_",e,"_country_",c,".Rdata"))

                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}

# Save output ----

saveRDS(df_yhat, file = paste0(results, "df_yhat_annual_wages_log_male.rds"))

beep()

