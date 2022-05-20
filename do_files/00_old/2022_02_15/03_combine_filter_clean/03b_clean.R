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

data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)
library(data.table)

options(scipen = 999) # disable scientific notation

# Load data -----

df_sample_0 <- readRDS(file = paste0(data_files, "03a_df_sample.rds"))

df_sample_1 <- df_sample_0



# Clean wages ----

# If unemployed, then wages == 0
df_sample_1 <- df_sample_1 %>%
        mutate(
                wages = ifelse(unmp==1, yes = 0, no = wages),
                ln_wages = log(wages),
                ln_wages = ifelse(wages == 0 & unmp==1, yes = 0, no = ln_wages),
                hourly_wage = ifelse(unmp==1, yes = 0, no = hourly_wage),
                ln_hourly_wage = log(hourly_wage),
                ln_hourly_wage = ifelse(hourly_wage == 0 & unmp==1, yes = 0, no = ln_hourly_wage),
                hourly_wage_dollars = ifelse(unmp==1, yes = 0, no = hourly_wage_dollars),
                ln_hourly_wage_dollars = log(hourly_wage_dollars),
                ln_hourly_wage_dollars = ifelse(hourly_wage_dollars == 0 & unmp==1, yes = 0, no = ln_hourly_wage_dollars),
        )

# Clean employment status ----

# If unemployed, then no work contract (temp or permanent)
# recode employment status (0=unemployed; 1=temp contract; 2=perm contract; 3=temp, not not FTC)
df_sample_1 <- df_sample_1 %>%
        mutate(emp_status = ifelse(unmp == 1, yes = 0, no = emp_status),
               perm = ifelse(unmp==0 & emp_status==1, yes = 1, no = 0),
               temp = ifelse(unmp==0 & emp_status==2, yes = 1, no = 0))

# determine minimum difference between sample periods (i.e. annual or biannual)
df_sample_1 <- df_sample_1 %>%
        group_by(country, pid) %>%
        mutate(year_lag = year - lag(year,1,default = NA)) %>%
        group_by(country) %>%
        mutate(year_lag = min(year_lag,na.rm = TRUE)) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_1, file = paste0(data_files, "03b_df_sample_cleaned.rds"))
