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

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

# clean data -----

# if treated, then must be employed after treatment
# if not treated, then must be employed

# Unmp to perm
df_event_u_p <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_u_p"),unemployment_rate) %>%
        mutate(event_u_p_time = ifelse(event_u_p_time < -2, yes = -2,
                                       ifelse(event_u_p_time > 4, yes = 4, no = event_u_p_time)),
               event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 3, no = 0),
        ) %>%
        rename(event_time_pos = event_u_p_time_pos)

df_event_u_p_treat_y <- df_event_u_p %>%
        filter(event_u_p_yes == 1 & event_u_p_drop == 0) %>%
        mutate(event_time_pos = ifelse(event_time_pos<1, yes = 0, no = event_time_pos))

df_event_u_p_treat_n <- df_event_u_p %>%
        filter(event_u_p_yes == 0 & unmp == 0)

df_event_u_p <- bind_rows(df_event_u_p_treat_y,df_event_u_p_treat_n)
rm(df_event_u_p_treat_y,df_event_u_p_treat_n)

df_event_u_p <- df_event_u_p %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2)

# Unmp to temp
df_event_u_t <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_u_t"),unemployment_rate) %>%
        mutate(event_u_t_time = ifelse(event_u_t_time < -2, yes = -2,
                                       ifelse(event_u_t_time > 4, yes = 4, no = event_u_t_time)),
               event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0),
        ) %>%
        rename(event_time_pos = event_u_t_time_pos)

df_event_u_t_treat_y <- df_event_u_t %>%
        filter(event_u_t_yes == 1 & event_u_t_drop == 0) %>%
        mutate(event_time_pos = ifelse(event_time_pos<1, yes = 0, no = event_time_pos))

df_event_u_t_treat_n <- df_event_u_t %>%
        filter(event_u_t_yes == 0 & unmp == 0) 

df_event_u_t <- bind_rows(df_event_u_t_treat_y,df_event_u_t_treat_n)
rm(df_event_u_t_treat_y,df_event_u_t_treat_n)

df_event_u_t <- df_event_u_t %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2)

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("u_p","u_t")

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")

# Dummy IF Models ---- 

# Annual countries
for(c in country_ann) {
        print(paste0("country = ", c))
        for (e in event) {

                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)

                # Dummy IF
                # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "2")
                model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                              data = data.frame(df_country), 
                              robust = TRUE,
                              id = "pid")
                
                assign(paste0("model_first_hourly_wages_log_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_hourly_wages_log_event_",e,"_country_",c,".Rdata"))
                
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
                
                # Dummy IF
                # model - baseline is 1 period before event.  in biannual data this is event_time_pos==1
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "1")
                model <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                              data = data.frame(df_country), 
                              robust = TRUE,
                              id = "pid")
                
                assign(paste0("model_first_hourly_wages_log_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_hourly_wages_log_event_",e,"_country_",c,".Rdata"))

                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}

# Save output ----

saveRDS(df_yhat, file = paste0(results, "df_yhat_first_hourly_wages_log_unmp.rds"))

beep()

