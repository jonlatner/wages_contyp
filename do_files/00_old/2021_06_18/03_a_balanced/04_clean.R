# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample.rds")) 
# describe(df_sample_0$ln_hourly_wage)

# test <- select(df_sample_0, country, pid, year, unmp, hours, wages, ln_hourly_wage) %>%
#                 filter(ln_hourly_wage>0 & ln_hourly_wage < 1) %>%
#                 mutate(hourly_wage = exp(ln_hourly_wage))
# describe(test$ln_hourly_wage)

# clean ----

# employment status (0=unemployed; 1=temp contract; 2=perm contract)
df_sample_0 <- df_sample_0 %>%
        mutate(temp = ifelse(unmp==0 & temp==1, yes = 1, no = 0),
               perm = ifelse(unmp==0 & temp==0, yes = 1, no = 0),
               emp_status = ifelse(unmp==1, yes = 0, 
                                   ifelse(temp==1, yes = 1, 
                                          ifelse(perm==1, yes = 2, no = 0))))

df_sample_0 <- df_sample_0 %>%
        filter(country=="AU" & study_period==2001) %>%
        select(country, study_period, pid, year, age_cat, edu_cat, male, unmp, temp, perm, emp_status, ln_hourly_wage) %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(emp_status_lag = lag(emp_status,1),
               period=row_number(),
               edu_cat = last(edu_cat),
               age_cat = first(age_cat),
               male = first(male)) %>%
        ungroup()


with(df_sample_0,table(emp_status, unmp, useNA = "ifany"))
with(df_sample_0,table(emp_status, temp, useNA = "ifany"))
with(df_sample_0,table(emp_status, perm, useNA = "ifany"))

# t <- with(df_sample_0,table(emp_status,emp_status_lag))
# prop.table(t,1)

# Event 1 - movement into temporary ----

df_sample_1 <- df_sample_0 %>%
        select(country, study_period, pid, year,emp_status,unmp,temp,perm,ln_hourly_wage) %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(event_1 = ifelse(temp == 1 & lag(temp,1) != 1 & row_number()>1, yes = 1, no = 0),
               spell_1 = cumsum(event_1),
               post_event_1 = ifelse(event_1 == 1 & spell_1 == 1, yes = 1, no = NA),
               post_event_1 = na.locf(post_event_1, na.rm = FALSE),
               post_event_1 = ifelse(is.na(post_event_1), yes = 0, no = post_event_1),
               post_event_1 = cumsum(post_event_1),
               pre_event_1 = ifelse(lead(post_event_1,1)==1, yes = 1, no = 0),
               pre_event_1 = ifelse(is.na(pre_event_1), yes = 0, no = pre_event_1),
               event_1 = ifelse(post_event_1 == 1 & event_1 == 1 & spell_1 == 1, yes = 1, no = 0),
               post_event_1 = ifelse(post_event_1>0, yes = post_event_1 - 1, no = post_event_1),
               event_1a = ifelse(temp == 1 & lag(temp,1) != 1 & row_number()>1, yes = 1, no = NA),
               event_1a = na.locf(event_1a, na.rm = FALSE),
               event_1a = ifelse(is.na(event_1a), yes = 0, no = event_1a),
        ) %>%
        ungroup() %>%
        select(-spell_1)

# filter(df_sample_1) %>% print(n=40)
filter(df_sample_1,pid==100270)
filter(df_sample_1,pid==100052)
filter(df_sample_1,pid==100357)
filter(df_sample_1,pid==100290)

# Event 2 - movement into permanent ----

df_sample_2 <- df_sample_1 %>%
        # filter(country=="AU" & study_period==2001) %>%
        # select(country, study_period, pid, year,emp_status,unmp,temp,perm) %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(event_2 = ifelse(perm == 1 & lag(perm,1) != 1 & row_number()>1, yes = 1, no = 0),
               spell_2 = cumsum(event_2),
               post_event_2 = ifelse(event_2 == 1 & spell_2 == 1, yes = 1, no = NA),
               post_event_2 = na.locf(post_event_2, na.rm = FALSE),
               post_event_2 = ifelse(is.na(post_event_2), yes = 0, no = post_event_2),
               post_event_2 = cumsum(post_event_2),
               pre_event_2 = ifelse(lead(post_event_2,1)==1, yes = 1, no = 0),
               pre_event_2 = ifelse(is.na(pre_event_2), yes = 0, no = pre_event_2),
               event_2 = ifelse(post_event_2 == 1 & event_2 == 1 & spell_2 == 1, yes = 1, no = 0),
               post_event_2 = ifelse(post_event_2>0, yes = post_event_2 - 1, no = post_event_2),
               event_2a = ifelse(perm == 1 & lag(perm,1) != 1 & row_number()>1, yes = 1, no = NA),
               event_2a = na.locf(event_2a, na.rm = FALSE),
               event_2a = ifelse(is.na(event_2a), yes = 0, no = event_2a),
        ) %>%
        ungroup() %>%
        select(-spell_2)

# filter(df_sample_2,event_2==1) %>% print(n=40)
# filter(df_sample_2,pre_event_2==1 & unmp == 1)
filter(df_sample_2,pid==100079)
filter(df_sample_2,pid==100030)



# Save data sets ----

saveRDS(df_sample_2, file = paste0(data_files, "df_sample_clean.rds"))
