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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/AU/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----
# This data declares that all job contracts that are not permanent are temporary

df_au <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ",") %>%
        select(year, AU) %>%
        rename(cpi=AU)

# Merge data ----

df_au <- merge(df_au,df_inflation)
rm(df_inflation)


# Clean ----

df_au <- df_au %>%
        rename(occ = jbm682,
               prestige = jbmo6s,
               lfs = esdtl,
               hours=jbmhruc,
               wages=wsfei, # DV: Financial year gross wages & salary ($) [imputed] [weighted topcode]
               edu = edhigh1,
               contyp = jbmcnt,
               sex = hgsex,
               empstat = esempdt,
               age=hgage,
               pid = xwaveid) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_au$lfp <- recode(df_au$lfs, "lo:0=NA; 1:4=1; 5:7=0")

# unemployment status
df_au$unmp <- recode(df_au$lfs, "lo:0=NA; 1:2=0; 3:4=1; else=NA")

# jbmcnt 1 Contract/fixed time
# jbmcnt 2 Casual
# jbmcnt 3 Permanent
# Employment status (1=permanent, 2=temporary)
df_au$emp_status  <- recode(df_au$contyp, "lo:0=NA; 1:2=2; 3=1; 8=NA")

# Self-employed
df_au$slf <- recode(df_au$empstat, "4:5=1; else=0")

# Occupation (ISCO 88)
df_au$occ <- recode(df_au$occ, "lo:0=NA")

# Prestige (Treiman)
df_au$prestige <- recode(df_au$prestige, "lo:0 = NA")

# Hours (weekly)
df_au$hours <- recode(df_au$hours, "lo:-.001 = NA")

# Wages (annual)
df_au$wages <- recode(df_au$wages, "lo:-.001 = NA")
summary(df_au$wages)
df_au <- df_au %>%
        mutate(wages = wages/(cpi/100),
               wages=wages/12, # monthly wages
               hourly_wage = wages/(hours*4), #monthly wages/monthly hours
               )

# demographics -----
# education (ISCED)
df_au$edu_cat <- recode(df_au$edu, "lo:-1 = NA; 6:9=1; 4:5=2; 1:3=3; 10=NA")

# sex
df_au$male <- recode(df_au$sex, "lo:0 = NA; 2 = 0")

# Age
df_au <- df_au %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# save ----

df_sample_0 <- df_au %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "AU"

saveRDS(df_sample_0, file = paste0(data_files, "au_sample_v01.rds"))

