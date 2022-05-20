# Top commands ----
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
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "SECCOPA/projects/mobility/data_files/KO/"
raw_data = "SECCOPA/data/KO_KLIPS/support_files/"
support_files = "SECCOPA/projects/mobility/support_files/"
prestige_files = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ko <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, KO) %>%
        rename(cpi=KO)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "oecd_exchange_rate_2010.csv"), sep = ";") %>%
        filter(country_name == "KO")

# Merge data ----

df_ko <- merge(df_ko,df_inflation)
rm(df_inflation)

df_ko$exchange <- df_exchange$exchange
rm(df_exchange)

# Occupation ------------------------

df_ko$occ <- recode(df_ko$occ, "lo:0=NA")

# Employment status ----

# labor force participation
df_ko$lfp <- recode(df_ko$empstat, "1:3=1; 4:hi=0; lo:0=NA")

# employment status
df_ko$unmp <- recode(df_ko$empstat, "1:2=0; 3=1; else=NA")

# contract type
df_ko$temp  <- recode(df_ko$contyp, "2=0; 1=1; else=NA")

# Self-employed
df_ko$slf <- recode(df_ko$emptype, "2=1; else=0")

# Hours (weekly)
df_ko$hours <- recode(df_ko$hours, "lo:-.001=NA")
summary(df_ko$hours)

# Monthly wages  (unit: krw 10,000)
df_ko$wages <- recode(df_ko$wages, "lo:-.001=NA")

df_ko <- df_ko %>%
        mutate(wages = (wages*10000)/(cpi/100),
               wages = wages/exchange,
               hourly_wage = wages/(hours*4),
               ln_hourly_wage = log(hourly_wage))

# demographics -------------------------
# education (ISCED)
table(df_ko$edu)

df_ko$edu_cat <- recode(df_ko$edu, "1:4=1; 5=2; 6:9=3; else=NA")

# sex
df_ko$male <- recode(df_ko$gender, "2 = 0")

# Age
df_ko <- df_ko %>%
        mutate(age=year-birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_select <- df_ko %>%
        select(pid, year, age, age_cat, edu_cat, male, slf, lfp, unmp, temp, hours, wages, ln_hourly_wage)

df_select$country <- "KO"

saveRDS(df_select, file = paste0(data_files, "ko_sample.rds"))


df_sample_1 <- df_select %>%
        filter(age >= 25 & age <= 54) %>%
        filter(year>=2000 & year<=2018) %>%
        select(country,pid,year,age_cat,edu_cat,male,lfp,unmp,temp) %>%
        filter(lfp == 1) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(unmp == 1 | (temp == 0 | temp == 1)) %>%
        arrange(country,pid,year)

with(df_sample_1, table(unmp,temp,useNA = "ifany"))
# remove individuals who are both unemployed and have a work contract (temp or permanent)
df_sample_1 <- df_sample_1 %>%
        mutate(test = ifelse(unmp == 1 & !is.na(temp), yes = 1, no = 0)) %>%
        filter(test == 0) %>%
        select(-test)
with(df_sample_1, table(unmp,temp,useNA = "ifany"))

# employment status (0=unemployed; 1=temp contract; 2=perm contract)
df_sample_1 <- df_sample_1 %>%
        mutate(temp = ifelse(unmp==1, yes = 0, no = temp),
               perm = ifelse(unmp==0 & temp==0, yes = 1, no = 0),
               emp_status = ifelse(unmp==1, yes = 0, 
                                   ifelse(temp==1, yes = 1, 
                                          ifelse(perm==1, yes = 2, no = 0))))


with(df_sample_1,table(emp_status, unmp, useNA = "ifany"))
with(df_sample_1,table(emp_status, temp, useNA = "ifany"))
with(df_sample_1,table(emp_status, perm, useNA = "ifany"))

df_sample_1 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_1 = ifelse(temp == 1 & lag(temp,1) != 1 & row_number()>1, yes = 1, no = 0),
               emp_status_lag = lag(emp_status,1),
               period=row_number(),
               edu_cat = last(edu_cat),
               age_cat = first(age_cat),
               male = first(male)) %>%
        ungroup()

with(subset(df_sample_1,event_1==1),table(emp_status,emp_status_lag))
