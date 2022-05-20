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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wages_contyp/")

data_files = "data_files/NE/LISS/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ne <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, CH) %>%
        rename(cpi=CH)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "OECD/oecd_exchange_rate_2010.csv"), sep = ";")

# Unemployment rate - world bank
df_unmp_rate <- read.csv(paste0(support_files, "world_bank/world_bank_unmp.csv"), sep = ",")


# Merge data ----

df_ne <- merge(df_ne,df_inflation)
rm(df_inflation)

# Employment status ----

# labor force participation
df_ne <- df_ne %>%
        mutate(lfp = ifelse(work_088_emp == 1 | work_089_unemp == 1, yes = 1, 0),
               lfp = ifelse(work_095_nw_student == 1 |
                                    work_096_nw_homemaker == 1 |
                                           work_097_nw_private == 1 |
                                           work_098_nw_retired_early == 1 |
                                           work_099_nw_retired == 1 |
                                           work_100_nw_disabled == 1, yes = 0, no = lfp))

# with(df_ne,table(lfs,work_088_emp,useNA = "ifany"))
# with(df_ne,table(lfs,work_089_unemp,useNA = "ifany"))
# with(df_ne,table(lfs,lfp,useNA = "ifany"))

# with(subset(df_ne,lfp==1),table(work_089_unemp,work_088_emp,useNA = "ifany"))
# View(filter(df_ne, work_088_emp==1 & work_089_unemp==1 & lfp==1))

# Unemployment status
df_ne <- df_ne %>%
        mutate(unmp = ifelse(work_088_emp == 1 & lfp == 1, yes = 0,
                             ifelse(work_088_emp == 0 & lfp == 1, yes = 1, no = NA)))

with(df_ne,table(lfp,unmp,useNA = "ifany"))

# cw17j088=1 or cw17j089=1 or cw17j091=1 or cw17j092=1 or cw17j102=1 or cw17j103=1

# Employment status (1=permanent, 2=temporary)
with(subset(df_ne,lfp==1),table(empstat,work_088_emp, useNA = "ifany"))

df_ne <- df_ne %>%
        mutate(emp_status = ifelse(work_088_emp == 1 & lfp == 1 & empstat == 1, yes = 1,
                             ifelse(work_088_emp == 1 & lfp == 1 & empstat == 2, yes = 2, no = NA)))

# Self-employed
df_ne <- df_ne %>%
        mutate(slf = ifelse(work_088_emp == 1 & lfp == 1 & empstat == 5, yes = 1, no = 0))

# Hours (weekly)
df_ne$hours <- recode(df_ne$hours, "lo:-.001 = NA")

# Wages (annual)
df_ne$wages_orig <- df_ne$wages
df_ne$wages <- recode(df_ne$wages, "lo:-.001 = NA; 9999999998:hi=NA")

df_ne <- df_ne %>%
        mutate(wages = wages/(cpi/100),
               wages=wages/12, # monthly wages
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours, 
        )

# demographics -----
# education (ISCED)
df_ne$edu_cat <- recode(df_ne$edu, "lo:0 = NA; 1:11=1; 12:15=2; 16:28=3; else=NA")

# sex
df_ne$male <- recode(df_ne$gender, "2 = 0")

# Age
df_ne <- df_ne %>%
        mutate(age = year - birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# save ----

df_sample_0 <- df_ne %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)


df_sample_0$country <- "NE-LISS"

saveRDS(df_sample_0, file = paste0(data_files, "ne_sample.rds"))
