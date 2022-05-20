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
with(df_sample_0,table(study_period,country))

# clean ----

# if unemployed and missing wages, then wages = 0
df_sample_0 <- df_sample_0 %>%
        mutate(ln_hourly_wage = ifelse(unmp==1 & is.na(ln_hourly_wage), yes = 0, no = ln_hourly_wage))

# employment status (0=unemployed; 1=temp contract; 2=perm contract)
df_sample_0 <- df_sample_0 %>%
        mutate(emp_status = ifelse(temp==1, yes = 1, 
                                   ifelse(temp==0, yes = 2, no = 0)),
               emp_status = ifelse(unmp==1, yes = 0, no = emp_status))

df_sample_0 <- df_sample_0 %>%
        mutate(temp = ifelse(unmp==1, yes = 0, no = temp))

with(df_sample_0,table(emp_status, temp, useNA = "ifany"))
with(df_sample_0,table(emp_status, unmp, useNA = "ifany"))
with(df_sample_0,table(temp, unmp, useNA = "ifany"))

df_sample_0 <- df_sample_0 %>%
        select(-temp)

df_sample_0 <- df_sample_0 %>%
        mutate(age_cat = as.factor(age_cat),
               edu_cat = as.factor(edu_cat))
df_sample_0$edu_cat <- relevel(df_sample_0$edu_cat,ref = 2)
df_sample_0$age_cat <- relevel(df_sample_0$age_cat,ref = 2)

# Spells ----

# df_sample_0 <- df_sample_0 %>%
#         filter(country == "DE" & study_period == 2004) %>%
#         select(country,study_period,pid,year,unmp,emp_status)

# Temporary employment spell
df_sample_1 <- df_sample_0 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(period=row_number()) %>%
        mutate(start_temp = ifelse(emp_status==1 & lag(emp_status,1)!=1, yes = year, no = 0),
               start_temp = ifelse(emp_status==1 & row_number()==1, yes = year, no = start_temp),
               end_temp = ifelse(emp_status!=1 & lag(emp_status,1)==1, yes = year, no = NA)) %>%
        ungroup() %>%
        group_by(country, study_period, pid) %>%
        mutate(spell_temp = cumsum(ifelse(start_temp>0, yes = 1, no = 0))) %>%
        ungroup() %>%
        mutate(start_temp = ifelse(start_temp == 0, yes = NA, no = start_temp),
               end_temp = ifelse(end_temp == 0, yes = NA, no = end_temp))

filter(df_sample_1,pid==8603)
filter(df_sample_1,pid==18804)

# Post temporary employment spell
df_sample_2 <- df_sample_1 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE),
               post_temp = year - end_temp + 1,
               post_temp = ifelse(is.na(post_temp), yes = 0, no = post_temp),
               post_temp = ifelse(emp_status==1, yes = 0, no = post_temp)
        ) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE)) %>%
        ungroup() %>%
        arrange(pid,year)

filter(df_sample_2,pid==8603)
filter(df_sample_2,pid==18804)
filter(df_sample_2,pid==8201202)

# Post first temporary employment spell
df_sample_3 <- df_sample_2 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(end_temp_first = ifelse(spell_temp==1, yes = end_temp, no = NA),
               end_temp_first = na.locf(end_temp_first, na.rm = FALSE),
               post_temp_first = year - end_temp_first + 1,
               post_temp_first = ifelse(is.na(post_temp_first), yes = 0, no = post_temp_first)
        ) %>%
        ungroup()

df_sample_4 <- df_sample_3 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(edu_cat = last(edu_cat),
               age_cat = first(age_cat),
               male = first(male)) %>%
        ungroup() %>%
        select(-start_temp,-end_temp,-end_temp_first)

filter(df_sample_4,pid==8603)
filter(df_sample_4,pid==18804)
filter(df_sample_4,pid==8201202)

t <- with(subset(df_sample_4, post_temp>0), prop.table(table(post_temp)))
t <- data.frame(t)
t$cumsum <- cumsum(t$Freq)
t

df_sample_5 <- df_sample_4 %>%
        mutate(unmp = ifelse(emp_status == 0, yes = 1, no = 0),
               temp = ifelse(emp_status == 1, yes = 1, no = 0),
               perm = ifelse(emp_status == 2, yes = 1, no = 0)) %>%
        select(-emp_status)
        

# Save data sets ----

saveRDS(df_sample_5, file = paste0(data_files, "df_sample_clean.rds"))
