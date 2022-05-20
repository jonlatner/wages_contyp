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

data_files = "SECCOPA/projects/mobility/data_files/CH/"
raw_data = "SECCOPA/panel_data/CH_FORS/support_files/"
support_files = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ch <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, CH) %>%
        rename(cpi=CH)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "oecd_exchange_rate_2010.csv"), sep = ";") %>%
        filter(country_name == "CH")

# Merge data ----

df_ch <- merge(df_ch,df_inflation)
rm(df_inflation)


df_ch$exchange <- df_exchange$exchange
rm(df_exchange)


# Clean ------------------------

df_ch <- df_ch %>%
        rename(occ = is4maj,
               prestige = tr1maj,
               hours= pw74,
               edu = isced,
               pid=idpers) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_ch$lfp <- recode(df_ch$wstat, "lo:0 = NA; 3=0; 1:2=1")

# employment status
df_ch$unmp <- recode(df_ch$wstat, "lo:0 = NA; 1=0; 2=1; else=NA")

# contract type
df_ch$temp  <- recode(df_ch$pw36, "lo:0 = NA; 1=1; 2=0")
# with(df_ch,table(pw37,pw36,useNA = "ifany"))

# Self-employed
df_ch$slf <- recode(df_ch$pw29, "3=1; else=0")

# Occupation (ISCO 88)
df_ch$occ <- recode(df_ch$occ, "lo:0 = NA")

# Prestige (Treiman)
df_ch$prestige <- recode(df_ch$prestige, "lo:0 = NA")

filter(df_ch,pid==10776101)

# Hours (weekly)
df_ch$hours <- recode(df_ch$hours, "lo:-.001 = NA")

# Wages (monthly)
df_ch$wages <- recode(df_ch$wages, "lo:-.001 = NA")
df_ch <- df_ch %>%
        mutate(wages = wages/(cpi/100),
               wages = wages/exchange,
               hourly_wage = wages/(hours*4),
               ln_hourly_wage = log(hourly_wage))

# demographics -------------------------
# education (ISCED)
table(df_ch$edu)
df_ch$edu_cat <- recode(df_ch$edu, "lo:-1 = NA; c(0,10,20)=1; 30:33=2; 40:hi=3")
with(df_ch,table(edu,edu_cat))

# sex
df_ch$male <- recode(df_ch$sex, "2 = 0")

# Age
df_ch <- df_ch %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_select <- df_ch %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, hours, wages, ln_hourly_wage)

df_select$country <- "CH"

saveRDS(df_select, file = paste0(data_files, "ch_sample.rds"))

