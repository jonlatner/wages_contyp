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
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "SECCOPA/projects/mobility/data_files/UK/"
prestige_files = "SECCOPA/projects/mobility/support_files/"
support_files = "SECCOPA/projects/mobility/support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data ----

df_uk <- readRDS(paste0(data_files, "covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, UK) %>%
        rename(cpi=UK,
               wave_yr = year)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "oecd_exchange_rate_2010.csv"), sep = ";") %>%
        filter(country_name == "UK")

# Merge data ----

df_uk <- merge(df_uk,df_inflation)
rm(df_inflation)

df_uk$exchange <- df_exchange$exchange
rm(df_exchange)

# clean data ----

df_uk <- df_uk %>%
        arrange(pidp, wave_yr) %>%
        rename(year=wave_yr,
               pid =pidp) %>%
        select(-wave_no)


# employment status ----
df_uk <- df_uk %>%
        mutate(unmp = ifelse(jbstat==3, yes=1,
                             ifelse(jbstat == 2, yes = 0, no = NA)),
               lfp = ifelse(jbstat==2 | jbstat==3, yes = 1,
                            ifelse(jbstat < 0, yes = NA, no = 0)))

# Contract type (0=permanent, 1=fixed term contract, 2=other temporary)
# Contract type if year < 1999
df_uk <- df_uk %>%
        mutate(temp_1 = ifelse(jbterm == 1, yes = 0,
                             ifelse(jbterm == 2 | jbterm == 3, yes = 1, no = NA))) %>%
        select(-jbterm)

# Contract type if year >= 1999
df_uk <- df_uk %>%
        mutate(temp_2 = ifelse(jbterm1 == 1, yes = 0,
                               ifelse(jbterm1 == 2, yes = 1, no = NA))) %>%
        select(-jbterm1, jbterm2)

df_uk <- df_uk %>%
        mutate(temp = ifelse(year < 1999, yes = temp_1, no = temp_2)) %>%
        select(-temp_1, -temp_2)

# Self-employed
df_uk$slf <- recode(df_uk$jbsemp, "2=1; else=0")

# Occupation (ISCO 88)
df_uk$occ <- recode(df_uk$jbisco88_cc, "lo:0 = NA")
df_uk <- df_uk %>%
        mutate(occ = ifelse(occ>100 | occ == 10, yes = occ*10, no=occ*100))
table(df_uk$occ, useNA = "ifany")

# Hours (weekly)
df_uk$hours <- recode(df_uk$jbhrs, "lo:-.001 = NA")
describe(df_uk$hours)

# Wages (monthly)
df_uk$wages <- recode(df_uk$fimnlabgrs_dv, "lo:-.001 = NA")
df_uk <- df_uk %>%
        mutate(wages = wages/(cpi/100),
               wages = wages/exchange,
               hourly_wage = wages/(hours*4),
               ln_hourly_wage = log(hourly_wage))

# Demographic characteristics ----

# Education
df_uk <- df_uk %>%
        rename(edu=hiqual_dv)

df_uk$edu_cat <- recode(df_uk$edu, "lo:0 = NA; c(5,9)=1; c(2,3,4)=2; c(1)=3")

# sex
df_uk$male <- recode(df_uk$sex, "lo:0 = NA; 1=1; 2=0") 

df_uk <- df_uk %>%
        mutate(age = ifelse(doby>0, yes = year - doby, no = NA)) %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1, 
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_select <- df_uk %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, hours, wages, ln_hourly_wage)

df_select$country <- "UK"

saveRDS(df_select, file = paste0(data_files, "uk_sample.rds"))

