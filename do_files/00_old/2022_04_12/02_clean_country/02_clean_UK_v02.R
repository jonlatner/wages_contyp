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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/UK/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data ----
# This data declares that temporary jobs are jobs with a fixed-term contract (i.e. not seasonal, casual, or temp agency)

df_uk <- readRDS(paste0(data_files, "covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, UK) %>%
        rename(cpi=UK,
               wave_yr = year)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "OECD/oecd_exchange_rate_2010.csv"), sep = ";")

# Unemployment rate - world bank
df_unmp_rate <- read.csv(paste0(support_files, "world_bank/world_bank_unmp.csv"), sep = ",")

# Merge data ----

df_uk <- merge(df_uk,df_inflation)
rm(df_inflation)

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

with(df_uk,table(year,jbterm, useNA = "ifany"))

# jbterm 1 Permanent job
# jbterm 2 Seasonal/tmp job
# jbterm 3 Contract/fixed time
# Contract type (1=permanent, 2=temporary, 3=employed, but not temp or perm)
# Contract type if year < 1999
df_uk <- df_uk %>%
        mutate(emp_status_1 = ifelse(jbterm == 1, yes = 1,
                               ifelse(jbterm == 3, yes = 2, # temporary and ftc
                                      ifelse(jbterm == 2, yes = 3, no = NA)))) # temporary and not ftc

with(df_uk,table(year,jbterm1, useNA = "ifany"))
with(df_uk,table(year,jbterm2, useNA = "ifany"))
# jbterm1 1 Permanent job
# jbterm1 2 Or is there some way that it is not permanent?
# Contract type if year >= 1999

df_uk <- df_uk %>%
        mutate(emp_status_2 = ifelse(jbterm1 == 1, yes = 1,
                               ifelse(jbterm1 == 2 & jbterm2 == 2, yes = 2, # temporary and ftc
                                      ifelse(jbterm1 == 2 & (jbterm2 == 1|jbterm2==3|jbterm2==4|jbterm2==97), yes = 3, no = NA)))) # temporary and not ftc

df_uk <- df_uk %>%
        mutate(emp_status = ifelse(year < 1999, yes = emp_status_1, no = emp_status_2)) %>%
        select(-emp_status_1, -emp_status_2)

# Employment status (1=permanent, 2=temporary, 3=employed, but not temp or perm)
with(df_uk,table(unmp,emp_status, useNA = "ifany"))
t <- with(df_uk,table(year,emp_status, useNA = "ifany"))
prop.table(t,1)

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
               hourly_wage = wages/(hours*4))

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

df_sample_0 <- df_uk %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "UK"

saveRDS(df_sample_0, file = paste0(data_files, "uk_sample_v02.rds"))
