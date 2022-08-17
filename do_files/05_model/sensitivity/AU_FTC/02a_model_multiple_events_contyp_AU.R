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

df_original <- readRDS(paste0(data_files,"df_AU_sensitivity.rds"))

df_original <- df_original %>%
        select(country,pid,pidseq,year,age,ln_hourly_wage,unmp,temp,perm,event_t_p_yes_final,event_p_t_yes_final,event_t_p_time_pos,event_p_t_time_pos,unemployment_rate) %>%
        filter(unmp == 0)  %>%
        group_by(country, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        ungroup() %>%
        filter(max>2) 

# Prepare for models ----

df_yhat <- data.frame()

# Countries
country_ann <- c("AU")

# Annual countries ----
for(c in country_ann) {
        print(paste0("country = ", c))
        
        df_country <- df_original %>%
                filter(country == c)
        
        # Dummy IF
        df_country$event_t_p_time_pos <- relevel(factor(df_country$event_t_p_time_pos), ref = "2")
        df_country$event_p_t_time_pos <- relevel(factor(df_country$event_p_t_time_pos), ref = "2")
        
        model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_t_p_time_pos + event_p_t_time_pos | 1,
                      data = data.frame(df_country), 
                      robust = TRUE,
                      id = "pid")
        
        # save(model, file=paste0(results,"model_first_contyp_t_p_country_",c,".Rdata"))
        
        df_output <- tidy_parameters(model)
        df_output$event <- "FE + IF"
        df_output$country <- c
        df_yhat <- rbind(df_yhat,df_output)
}

# Save output ----

saveRDS(df_yhat, file = paste0(results, "df_yhat_multiple_events_contyp_AU.rds"))

beep()

