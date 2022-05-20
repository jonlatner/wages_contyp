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
        select(country, pid, year, unmp, temp, ln_hourly_wage) %>%
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
               spell = cumsum(event_1),
               post_event_1 = ifelse(event_1 == 1 & spell == 1, yes = 1, no = NA),
               post_event_1 = na.locf(post_event_1, na.rm = FALSE),
               post_event_1 = ifelse(is.na(post_event_1), yes = 0, no = post_event_1),
               post_event_1 = cumsum(post_event_1),
               pre_event_1 = ifelse(lead(post_event_1,1)==1, yes = 1, no = 0),
               pre_event_1 = ifelse(is.na(pre_event_1), yes = 0, no = pre_event_1),
               event_1 = ifelse(post_event_1 == 1 & event_1 == 1 & spell == 1, yes = 1, no = 0),
               post_event_1 = ifelse(post_event_1>0, yes = post_event_1 - 1, no = post_event_1),
               event_1_yes = ifelse(row_number()==n() & post_event_1 > 0, yes = 1, no = 0),
               event_1_yes = ifelse(last(event_1_yes) == 1, yes = 1, no = 0)
        ) %>%
        ungroup() %>%
        select(-spell)

# Event 2 - unmp into perm ----

df_sample_2 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_2 = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1, yes = 1, no = 0),
               spell = cumsum(event_2),
               post_event_2 = ifelse(event_2 == 1 & spell == 1, yes = 1, no = NA),
               post_event_2 = na.locf(post_event_2, na.rm = FALSE),
               post_event_2 = ifelse(is.na(post_event_2), yes = 0, no = post_event_2),
               post_event_2 = cumsum(post_event_2),
               pre_event_2 = ifelse(lead(post_event_2,1)==1, yes = 1, no = 0),
               pre_event_2 = ifelse(is.na(pre_event_2), yes = 0, no = pre_event_2),
               event_2 = ifelse(post_event_2 == 1 & event_2 == 1 & spell == 1, yes = 1, no = 0),
               post_event_2 = ifelse(post_event_2>0, yes = post_event_2 - 1, no = post_event_2),
               event_2_yes = ifelse(row_number()==n() & post_event_2 > 0, yes = 1, no = 0),
               event_2_yes = ifelse(last(event_2_yes) == 1, yes = 1, no = 0)
        ) %>%
        ungroup() %>%
        select(-spell)

# Event 3 - unmp into temp ----

df_sample_3 <- df_sample_2 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_3 = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1, yes = 1, no = 0),
               spell = cumsum(event_3),
               post_event_3 = ifelse(event_3 == 1 & spell == 1, yes = 1, no = NA),
               post_event_3 = na.locf(post_event_3, na.rm = FALSE),
               post_event_3 = ifelse(is.na(post_event_3), yes = 0, no = post_event_3),
               post_event_3 = cumsum(post_event_3),
               pre_event_3 = ifelse(lead(post_event_3,1)==1, yes = 1, no = 0),
               pre_event_3 = ifelse(is.na(pre_event_3), yes = 0, no = pre_event_3),
               event_3 = ifelse(post_event_3 == 1 & event_3 == 1 & spell == 1, yes = 1, no = 0),
               post_event_3 = ifelse(post_event_3>0, yes = post_event_3 - 1, no = post_event_3),
               event_3_yes = ifelse(row_number()==n() & post_event_3 > 0, yes = 1, no = 0),
               event_3_yes = ifelse(last(event_3_yes) == 1, yes = 1, no = 0)
        ) %>%
        ungroup() %>%
        select(-spell)

# Event 4 - perm into temp ----

df_sample_4 <- df_sample_3 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_4 = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1, yes = 1, no = 0),
               spell = cumsum(event_4),
               post_event_4 = ifelse(event_4 == 1 & spell == 1, yes = 1, no = NA),
               post_event_4 = na.locf(post_event_4, na.rm = FALSE),
               post_event_4 = ifelse(is.na(post_event_4), yes = 0, no = post_event_4),
               post_event_4 = cumsum(post_event_4),
               pre_event_4 = ifelse(lead(post_event_4,1)==1, yes = 1, no = 0),
               pre_event_4 = ifelse(is.na(pre_event_4), yes = 0, no = pre_event_4),
               event_4 = ifelse(post_event_4 == 1 & event_4 == 1 & spell == 1, yes = 1, no = 0),
               post_event_4 = ifelse(post_event_4>0, yes = post_event_4 - 1, no = post_event_4),
               event_4_yes = ifelse(row_number()==n() & post_event_4 > 0, yes = 1, no = 0),
               event_4_yes = ifelse(last(event_4_yes) == 1, yes = 1, no = 0)
        ) %>%
        ungroup() %>%
        select(-spell)

# Save data sets ----

saveRDS(df_sample_4, file = paste0(data_files, "df_sample_clean.rds"))
