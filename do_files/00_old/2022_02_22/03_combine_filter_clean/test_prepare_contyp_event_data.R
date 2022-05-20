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
library(beepr)
library(haven)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files, "03b_df_sample_cleaned.rds"))
df_original <- df_original %>%
        select(country, pid, year, year_lag, wages, hourly_wage, perm, temp, age, unemployment_rate)

# Transition indicator ----

df_sample_1 <- df_original %>%
        filter(perm==1 | temp == 1) %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
               event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
        ) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_2, file = paste0(data_files, "test_prepare_contyp_event_data.rds"))

beep()
