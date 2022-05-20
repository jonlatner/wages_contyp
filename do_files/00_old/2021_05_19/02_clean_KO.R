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
raw_data = "SECCOPA/panel_data/KO_KLIPS/support_files/"
world_bank = "SECCOPA/projects/mobility/support_files/"
prestige_files = "SECCOPA/projects/mobility/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)
library(readxl)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ko <- readRDS(paste0(data_files,"covars.rds"))

df_crosswalk <- read_excel(path = paste0(raw_data, "ksco_isco_crosswalk.xlsx"), sheet = "ksco_5_isco88") 
df_crosswalk <- df_crosswalk %>%
        select(ksco_5_code, isco88) %>%
        filter(!is.na(ksco_5_code)) %>%
        mutate(isco88 = as.numeric(as.character(isco88)))

df_prestige <- read.csv(paste0(prestige_files, "isco88_siops.csv"))

df_inflation <- read.csv(paste0(world_bank, "world_bank_cpi.csv"), sep = ";") %>%
        select(year, KO) %>%
        rename(cpi=KO)

df_ko <- merge(df_ko,df_inflation)
rm(df_inflation)
summary(df_ko)

# Occupation ------------------------

# prestige

df_ko$occ <- recode(df_ko$occ, "lo:0=NA")
df_ko <- merge(df_ko, df_crosswalk, by.x = c("occ"), by.y = c("ksco_5_code"), all.x = TRUE)

df_ko <- df_ko %>%
        mutate(isco88 = ifelse(isco88 < 10, yes = isco88*1000,
                               ifelse(isco88 >= 10 & isco88 < 100, yes = isco88*100,
                                      ifelse(isco88 >= 100 & isco88 < 1000, yes = isco88*10,
                                             ifelse(isco88 >= 1000, yes = isco88, no = NA)))))

df_ko <- merge(df_ko, df_prestige, by = c("isco88"), all.x = TRUE)
rm(df_crosswalk,df_prestige)
summary(df_ko)
df_ko <- df_ko %>%
        select(-occ) %>%
        rename(occ=isco88)

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
        mutate(wages = (wages*10000)/cpi*100,
               hourly_wage = wages/(hours*4),
               ln_hourly_wage = log(hourly_wage))

summary(df_ko)

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
        select(pid, year, age, age_cat, edu_cat, male, slf, lfp, unmp, temp, occ, prestige, hours, wages, ln_hourly_wage)

df_select$country <- "KO"

saveRDS(df_select, file = paste0(data_files, "ko_sample.rds"))

summary(df_select)
