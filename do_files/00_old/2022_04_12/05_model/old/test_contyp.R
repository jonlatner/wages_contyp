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

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}


# load data -----

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_original <- df_original %>%
        filter(country == "DE")

# create sample ----

set.seed(1234)

df_sample <- df_original %>%
        select(country,pid) %>%
        group_by(country,pid) %>%
        slice(1) %>%
        ungroup() %>%
        sample_frac(.1,replace = TRUE) %>%
        mutate(sample = 1)
df_original <- merge(df_original,df_sample,by = c("country", "pid")) %>%
        filter(sample == 1) %>%
        arrange(country,pid,year)

# clean data -----



# if treated, then must be employed after treatment
# if not treated, then must be employed

# Temp to perm
df_event_t_p_01 <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_t_p"),unemployment_rate) %>%
        filter(unmp==0) %>%
        rename(event_time_pos = event_t_p_time_pos) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        ungroup() %>%
        filter(max>2) 

# Perm to temp
df_event_t_p_02 <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_t_p"),unemployment_rate) %>%
        rename(event_time_pos = event_t_p_time_pos) 

# Prepare for models ----

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

# Dummy IF Models ---- 

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
df_event_t_p_01$event_time_pos <- relevel(factor(df_event_t_p_01$event_time_pos), ref = "2")
model_event_t_p_01 <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                        data = data.frame(df_event_t_p_01), 
                        robust = TRUE,
                        id = "pid")

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
df_event_t_p_02$event_time_pos <- relevel(factor(df_event_t_p_02$event_time_pos), ref = "2")
model_event_t_p_02 <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                        data = data.frame(df_event_t_p_02), 
                        robust = TRUE,
                        id = "pid")


# Table ----

screenreg(
        list(model_event_t_p_01, model_event_t_p_02),
        table = FALSE,
        custom.coef.map = list(
                "event_time_pos0"="Pre event (<-2)",
                "event_time_pos1"="Pre event (-2)",
                "event_time_pos2"="Pre event (-1)",
                "event_time_pos3"="event",
                "event_time_pos4"="Post event (+1)",
                "event_time_pos5"="Post event (+2)",
                "event_time_pos6"="Post event (+3)",
                "event_time_pos7"="Post event (+4)"
        ),
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept|position"),
)

