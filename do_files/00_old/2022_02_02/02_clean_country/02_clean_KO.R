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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/KO/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ko <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ",") %>%
        select(year, KO) %>%
        rename(cpi=KO)

# Merge data ----

df_ko <- merge(df_ko,df_inflation)
rm(df_inflation)

# Occupation ----

df_ko$occ <- recode(df_ko$occ, "lo:0=NA")

# Employment status ----

# labor force participation
df_ko$lfp <- recode(df_ko$empstat, "1:3=1; 4:hi=0; lo:0=NA")

# unemployment status
df_ko$unmp <- recode(df_ko$empstat, "1:2=0; 3=1; else=NA")

# Employment status (1=permanent, 2=temporary)
# (main job) work period stated in contract p__0501
# (1) yes - temporary
# (2) no - permanent
# (3) don't know - NA

df_ko$emp_status  <- recode(df_ko$contyp, "2=1; 1=2; else=NA")

with(df_ko,table(emp_status, contyp))

# Self-employed
df_ko$slf <- recode(df_ko$emptype, "2=1; else=0")

# Hours (weekly)
df_ko$hours <- recode(df_ko$hours, "lo:-.001=NA")
summary(df_ko$hours)

# Monthly wages  (unit: krw 10,000)
df_ko$wages <- recode(df_ko$wages, "lo:-.001=NA")

df_ko <- df_ko %>%
        mutate(wages = (wages*10000)/(cpi/100),
               hourly_wage = wages/(hours*4))

# demographics -----
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

df_sample_0 <- df_ko %>%
        select(pid, year, age, age_cat, edu_cat, male, slf, lfp, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "KO"

saveRDS(df_sample_0, file = paste0(data_files, "ko_sample.rds"))

