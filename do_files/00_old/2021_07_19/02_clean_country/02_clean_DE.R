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

data_files = "SECCOPA/projects/mobility/data_files/DE/"
support_files = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(readxl)

options(scipen = 999) # disable scientific notation

# Load data ----

df_de <- readRDS(paste0(data_files, "covars.rds"))

df_de <- df_de %>%
        rename(year=syear)

# inflation data - world bank
df_inflation <- read_xlsx(paste0(support_files, "world_bank_cpi.xlsx")) %>%
        select(year, DE) %>%
        rename(cpi=DE)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "oecd_exchange_rate_2010.csv"), sep = ";") %>%
        filter(country_name == "DE")

# Merge data ----

df_de <- merge(df_de,df_inflation)
rm(df_inflation)

df_de$exchange <- df_exchange$exchange
rm(df_exchange)

# Employment status ----

# labor force participation
df_de$lfp <- recode(df_de$lfs, "lo:0 = NA; seq(1,5,1)=0; seq(6,12,1)=1")

# employment status
df_de$unmp <- recode(df_de$empst, "lo:0 = NA; 1:2=0; 5=1; else = NA")

# contract type
with(df_de, table(year,contyp))

df_de$temp <- recode(df_de$contyp, "lo:0 = NA; 1=0; 2=1; 3:hi=NA")

t <- with(df_de, table(year,temp))
prop.table(t,1)

# Self-employed
df_de$slf <- recode(df_de$contyp, "3:hi=1; else=0")

# Occupation (ISCO 88)
df_de$occ <- recode(df_de$occ, "lo:0 = NA")

# Prestige (Treiman)
df_de$prestige <- recode(df_de$prestige, "lo:0 = NA")

# Hours (annual)
df_de$hours <- recode(df_de$hours, "lo:-.001 = NA")
library(Hmisc)
describe(df_de$hours)

# Wages (annual)
df_de$wages <- recode(df_de$wages, "lo:-.001 = NA")

df_de <- df_de %>%
        mutate(wages = wages/(cpi/100),
               wages = wages/exchange,
               hourly_wage = wages/hours,
               wages=wages/12,
               hours=hours/52,
               ln_hourly_wage = log(hourly_wage))

describe(df_de$hours)

# demographic characteristics ----

# education (CASMIN)
# https://paneldata.org/soep-long/df_covars/pgen/pgcasmin
df_de$edu_cat <- recode(df_de$edu, "lo:0=NA; c(1,2,3)=1; c(4,5,6,7)=2; c(8,9)=3")
with(df_de, table(edu,edu_cat))

# sex
df_de <- df_de %>%
        mutate(sex = ifelse(sex == 2, yes = 0, no = sex)) %>%
        rename(male = sex) 

# Age
df_de <- df_de %>%
        mutate(age = ifelse(birth_year>0, yes = year - birth_year, no = NA),
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_select <- df_de %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, hours, wages, ln_hourly_wage)

df_select$country <- "DE"

saveRDS(df_select, file = paste0(data_files, "de_sample.rds"))


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
