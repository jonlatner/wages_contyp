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

data_files = "SECCOPA/projects/mobility/data_files/NE/LSP/"
world_bank = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ne <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(world_bank, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, CH) %>%
        rename(cpi=CH)

df_ne <- merge(df_ne,df_inflation)
rm(df_inflation)

# Employment status ----

with(df_ne,table(year,empstat))
with(df_ne,table(year,contyp))
with(df_ne,table(year,edu))

# labor force participation
df_ne$lfp <- recode(df_ne$empstat, "0:2=1; 3=0; 4=1; 5=0; 7=0")

# employment status
df_ne$unmp <- recode(df_ne$empstat, "0:1=0; 4=1; else=NA")

# contract type
df_ne$temp <- recode(df_ne$contyp, "1=0; 2:3=1; else=NA")

# Self-employed
df_ne$slf <- recode(df_ne$empstat, "2=1; else=0")

# Hours (weekly)
df_ne$hours <- recode(df_ne$hours, "lo:-.001 = NA")

# Wages (monthly)
df_ne$wages <- recode(df_ne$wages, "lo:-.001 = NA")

df_ne <- df_ne %>%
        mutate(wages = wages/cpi*100,
               hourly_wage = wages/(hours*4),
               ln_hourly_wage = log(hourly_wage))

# demographics -------------------------
# education (ISCED)
with(df_ne,table(year, edu))
df_ne$edu_cat <- recode(df_ne$edu, "lo:0 = NA; 2:3=1; 4=2; 5:6=3")

# sex
df_ne$male <- recode(df_ne$gender, "2 = 0")

# Age
df_ne <- df_ne %>%
        mutate(age = year - birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# save ----

df_select <- df_ne %>%
        mutate(prestige=NA) %>%
        mutate(occ=NA) %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, temp, occ, prestige, hours, wages, ln_hourly_wage)

summary(df_select)

df_select$country <- "NE-LSP"

saveRDS(df_select, file = paste0(data_files, "ne_sample.rds"))

