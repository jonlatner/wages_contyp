# https://www.online.fbe.unimelb.edu.au/HILDAodd/srchVarnameUsingCategoriesCrossWave.aspx

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

data_files = "SECCOPA/projects/mobility/data_files/IT/"
world_bank = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_it <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(world_bank, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, IT) %>%
        rename(cpi=IT)

df_it <- merge(df_it,df_inflation)
rm(df_inflation)

# Create uniform pid ----

df_it <- df_it %>%
        arrange(hid,pid,year) %>%
        unite(pid, c(hid, pid), sep = "", remove = TRUE) %>%
        mutate(pid = as.numeric(as.character(pid))) %>%
        arrange(pid,year)

# Keep one observation per individual per year ----

# View(filter(df_it, pid==241943 & year == 2012))

df_it <- df_it %>%
        group_by(pid,year) %>%
        arrange(pid,year,-months_worked) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        arrange(pid,year)

# View(filter(df_it_2, pid==241943 & year == 2012))
        
# Employment status ----


# labor force participation
df_it$lfp <- recode(df_it$lfs, "lo:-1=NA; c(0,5)=1; else=0")

# employment status
df_it$unmp <- recode(df_it$lfs, "lo:-1=NA; 0=0; 5=1; else=NA")

# contract type
df_it$temp  <- recode(df_it$contyp, "lo:0=NA; 1=0; 2:3=1; else=NA")

# Self-employed
df_it$slf <- recode(df_it$empstat, "2=1; else=0")

# Hours (weekly)
df_it$hours <- df_it$hours_total - is.na(df_it$hours_overtime)
df_it$hours <- recode(df_it$hours, "lo:-.001 = NA")

# Wages (annual)
df_it$wages <- recode(df_it$wages, "lo:-.001 = NA")
df_it <- df_it %>%
        mutate(wages = wages/cpi*100,
               hourly_wage = wages/(hours*52),
               wages=wages/12,
               ln_hourly_wage = log(hourly_wage))

# demographics -------------------------
# education (ISCED)
df_it$edu_cat <- recode(df_it$edu, "1:3=1; 4=2; 5:6=3; else=NA")

# sex
df_it$male <- recode(df_it$gender, "lo:0 = NA; 2 = 0")

# Age
df_it <- df_it %>%
        mutate(age = year - birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_select <- df_it %>%
        mutate(occ=NA,
               prestige=NA) %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, occ, prestige, hours, wages, ln_hourly_wage)

df_select$country <- "IT"
saveRDS(df_select, file = paste0(data_files, "it_sample.rds"))

summary(df_select)

