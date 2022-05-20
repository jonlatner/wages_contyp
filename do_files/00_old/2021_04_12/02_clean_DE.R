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
world_bank = "SECCOPA/projects/mobility/support_files/"

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
df_inflation <- read_xlsx(paste0(world_bank, "world_bank_cpi.xlsx")) %>%
        select(year, DE) %>%
        rename(cpi=DE)

df_de <- merge(df_de,df_inflation)
rm(df_inflation)
summary(df_de)

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

# Hours (weekly)
df_de$hours <- recode(df_de$hours, "lo:-.001 = NA")

# Wages (annual)
df_de$wages <- recode(df_de$wages, "lo:-.001 = NA")
df_de <- df_de %>%
        mutate(wages = wages/cpi*100,
               hourly_wage = wages/(hours),
               wages=wages/12,
               ln_hourly_wage = log(hourly_wage))

df_de$hours <- df_de$hours/52

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
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, occ, prestige, hours, wages, ln_hourly_wage)

df_select$country <- "DE"

saveRDS(df_select, file = paste0(data_files, "de_sample.rds"))
