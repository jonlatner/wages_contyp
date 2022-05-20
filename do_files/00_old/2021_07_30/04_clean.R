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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample.rds")) 

# clean ----

# employment status (0=unemployed; 1=temp contract; 2=perm contract)
df_sample_clean <- df_sample_0 %>%
        # filter(country=="DE" | country == "AU" | country == "IT") %>%
        select(country, pid, year, unmp, temp, ln_hourly_wage,age) %>%
        mutate(temp = ifelse(unmp==0 & temp==1, yes = 1, no = 0),
               perm = ifelse(unmp==0 & temp==0, yes = 1, no = 0),
               emp_status = ifelse(unmp==1, yes = 0, 
                                   ifelse(temp==1, yes = 1, 
                                          ifelse(perm==1, yes = 2, no = 0))))

with(df_sample_clean,table(emp_status, unmp, useNA = "ifany"))
with(df_sample_clean,table(emp_status, temp, useNA = "ifany"))
with(df_sample_clean,table(emp_status, perm, useNA = "ifany"))

# determine consecutive time periods
df_sample_clean_1 <- df_sample_clean %>%
        # filter(country == "DE") %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(diff = year - lag(year,1)) %>%
        group_by(country) %>%
        mutate(diff = min(diff, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(country, pid) %>%
        mutate(begin = ifelse(year - lag(year,1)>diff, yes = 1, no = 0),
               begin = ifelse(row_number()==1, yes = 1, no = begin),
               spell = cumsum(begin),
        ) %>%
        group_by(country, pid, spell) %>%
        mutate(count = row_number(),
               max = max(count),
               year_lag = lag(year,1),
        ) %>%
        ungroup() %>%
        mutate(check = year - year_lag) %>%
        filter(max>2)

# with(df_sample_1, table(country,check))
# head(df_sample_1, n=20)
# filter(df_sample_1, pid==4652 & country=="IT")


# must be in three consecutive time periods

# Event 1 - temp into perm
# Event 2 - unmp into perm
# Event 3 - unmp into temp
# Event 4 - perm into temp

# Event 1 - temp into perm ----

df_sample_1 <- df_sample_clean_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_1 = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1, yes = 1, no = 0),
               event_1_yes = max(event_1),
               event_1_year = ifelse(event_1 == 1, yes = year, no = 0),
               event_1_year = max(event_1_year),
               event_1_time = ifelse(event_1_yes == 1, yes = year - event_1_year, no = 0),
               event_1_time = ifelse(event_1_time < -3, yes = -3,
                                     ifelse(event_1_time > 5, yes = 5, no = event_1_time)),
               event_1_time_pos = ifelse(event_1_yes == 1, yes = event_1_time + 3, no = 0),
        ) %>%
        ungroup()

# Event 2 - unmp into perm ----

df_sample_2 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_2 = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1, yes = 1, no = 0),
               event_2_yes = max(event_2),
               event_2_year = ifelse(event_2 == 1, yes = year, no = 0),
               event_2_year = max(event_2_year),
               event_2_time = ifelse(event_2_yes == 1, yes = year - event_2_year, no = 0),
               event_2_time = ifelse(event_2_time < -3, yes = -3,
                                     ifelse(event_2_time > 5, yes = 5, no = event_2_time)),
               event_2_time_pos = ifelse(event_2_yes == 1, yes = event_2_time + 3, no = 0),
        ) %>%
        ungroup()

# Event 3 - unmp into temp ----

df_sample_3 <- df_sample_2 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_3 = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1, yes = 1, no = 0),
               event_3_yes = max(event_3),
               event_3_year = ifelse(event_3 == 1, yes = year, no = 0),
               event_3_year = max(event_3_year),
               event_3_time = ifelse(event_3_yes == 1, yes = year - event_3_year, no = 0),
               event_3_time = ifelse(event_3_time < -3, yes = -3,
                                     ifelse(event_3_time > 5, yes = 5, no = event_3_time)),
               event_3_time_pos = ifelse(event_3_yes == 1, yes = event_3_time + 3, no = 0),
        ) %>%
        ungroup() %>%
        select(-spell)

# Event 4 - perm into temp ----

df_sample_4 <- df_sample_3 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_4 = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1, yes = 1, no = 0),
               event_4_yes = max(event_4),
               event_4_year = ifelse(event_4 == 1, yes = year, no = 0),
               event_4_year = max(event_4_year),
               event_4_time = ifelse(event_4_yes == 1, yes = year - event_4_year, no = 0),
               event_4_time = ifelse(event_4_time < -3, yes = -3,
                                     ifelse(event_4_time > 5, yes = 5, no = event_4_time)),
               event_4_time_pos = ifelse(event_4_yes == 1, yes = event_4_time + 3, no = 0),
        ) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_4, file = paste0(data_files, "df_sample_clean.rds"))
