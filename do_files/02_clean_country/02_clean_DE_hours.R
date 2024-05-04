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

# FOLDERS (ADAPT THIS PATHWAY!)
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/DE/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_de <- readRDS(paste0(data_files, "covars.rds"))

df_de <- df_de %>%
        rename(year=syear) %>%
  filter(year>=2000)

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, DE) %>%
        rename(cpi=DE)

# Merge data ----

df_de <- merge(df_de,df_inflation,by = c("year"))
rm(df_inflation)

# Employment status ----

# labor force participation
df_de$lfp <- recode(df_de$lfs, "lo:0 = NA; seq(1,5,1)=0; seq(6,12,1)=1")

# unemployment status
df_de$unmp <- recode(df_de$empst, "lo:0 = NA; 1:2=0; 5=1; else = NA")

# Employment status (1=permanent, 2=temporary)
with(df_de, table(year,contyp))

df_de$emp_status <- recode(df_de$contyp, "lo:0 = NA; 3:hi=NA")

# Hours (annual)
df_de$hours <- recode(df_de$hours, "lo:-.001 = NA")

# Hours (Weekly)
df_de$hours_weekly <- recode(df_de$hours_weekly, "lo:-.001 = NA")

# Wages (annual)
df_de$wages <- recode(df_de$wages, "lo:-.001 = NA")

df_de <- df_de %>%
        mutate(wages = wages/(cpi/100),
               hourly_wage = wages/hours,
               hourly_wage_2 = (wages/52)/hours_weekly,
               wages=wages/12, # monthly wages
               hours=hours/12, # monthly hours
               )

describe(df_de$hours)


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
  mutate(age = ifelse(birth_year>0, yes = year - birth_year, no = NA))

# Sample creation ----

df_sample_0 <- df_de %>%
        select(pid, year, age, lfp, unmp, emp_status, hours, hours_weekly, wages, hourly_wage, hourly_wage_2) %>%
  filter(!is.na(emp_status),
         lfp == 1,
         age>=25&age<=54)

