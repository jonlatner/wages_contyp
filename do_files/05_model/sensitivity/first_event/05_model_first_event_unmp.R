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

# Adapt this pathway!
setwd("~/GitHub/wages_contyp/")

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

# if treated, then must be employed after treatment
# if not treated, then must be employed

df_original <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,event_u_p_yes_final,event_u_t_yes_final,event_u_p_time_pos,event_u_t_time_pos,unemployment_rate)

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
        df_country$event_u_p_time_pos <- relevel(factor(df_country$event_u_p_time_pos), ref = "2")
        df_country$event_u_t_time_pos <- relevel(factor(df_country$event_u_t_time_pos), ref = "2")
        
        # Unmp to perm
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_u_p_time_pos + as.factor(year) | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "u_p"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
        
        # Unmp to temp
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_u_t_time_pos + as.factor(year) | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "u_t"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Biannual countries ----
for(c in country_bi) {
        print(paste0("country = ", c))
        
        df_country <- df_original %>%
                filter(country == c)
        
        # Dummy IF
        df_country$event_u_p_time_pos <- relevel(factor(df_country$event_u_p_time_pos), ref = "1")
        df_country$event_u_t_time_pos <- relevel(factor(df_country$event_u_t_time_pos), ref = "1")
        
        # Unmp to perm
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_u_p_time_pos + as.factor(year) | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "u_p"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
        
        # Unmp to temp
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_u_t_time_pos + as.factor(year) | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "u_t"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Save output ----

saveRDS(df_yhat, file = paste0(results, "df_yhat_first_event_unmp.rds"))

beep()

