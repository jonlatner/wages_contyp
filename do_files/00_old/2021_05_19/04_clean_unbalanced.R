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

df_sample_0 <- readRDS(paste0(data_files, "df_sample_unbalanced.rds")) 
with(df_sample_0,table(study_period,country))

# clean ----

df_sample_0 <- df_sample_0 %>%
        mutate(drop = ifelse(country == "IT" & study_period>2010, yes = 1,
                             ifelse(country == "NE-LSP" & study_period>2008, yes = 1, no = 0))) %>%
        filter(drop == 0) %>%
        select(-drop)

df_sample_0 <- df_sample_0 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(edu_cat = last(edu_cat),
               age_cat = first(age_cat),
               male = first(male)) %>%
        ungroup()

# Spells ----

df_sample_1 <- df_sample_0 %>%
        arrange(country, study_period, pid, year) %>%
        group_by(country, study_period, pid) %>%
        mutate(period=row_number()) %>%
        mutate(start = ifelse(temp == 1 & lag(temp,1)!= 1, yes = year, no = NA),
               start = na.locf(start, na.rm = FALSE),
               post_temp = year-start,
               post_temp = ifelse(is.na(post_temp), yes = 0, no = post_temp)
        ) %>%
        ungroup()

# Temporary employment spell
# df_sample_1 <- df_sample_0 %>%
#         arrange(country, study_period, pid, year) %>%
#         group_by(country, study_period, pid) %>%
#         mutate(period=row_number()) %>%
#         mutate(start_temp = ifelse(temp==1 & lag(temp,1)!=1, yes = year, no = 0),
#                start_temp = ifelse(temp==1 & row_number()==1, yes = year, no = start_temp),
#                end_temp = ifelse(temp!=1 & lag(temp,1)==1, yes = year, no = NA)) %>%
#         ungroup() %>%
#         group_by(country, study_period, pid) %>%
#         mutate(spell_temp = cumsum(ifelse(start_temp>0, yes = 1, no = 0))) %>%
#         ungroup() %>%
#         mutate(start_temp = ifelse(start_temp == 0, yes = NA, no = start_temp),
#                end_temp = ifelse(end_temp == 0, yes = NA, no = end_temp))

# Post temporary employment spell
# df_sample_2 <- df_sample_1 %>%
#         arrange(country, study_period, pid, year) %>%
#         group_by(country, study_period, pid) %>%
#         mutate(end_temp = na.locf(end_temp,na.rm = FALSE),
#                post_temp = year - end_temp + 1,
#                post_temp = ifelse(is.na(post_temp), yes = 0, no = post_temp),
#                post_temp = ifelse(temp==1, yes = 0, no = post_temp)
#         ) %>%
#         mutate(end_temp = na.locf(end_temp,na.rm = FALSE)) %>%
#         ungroup() %>%
#         mutate(post_temp = ifelse((country == "IT" | country == "NE-LSP") & post_temp > 0, yes = post_temp + 1, no = post_temp)) %>%
#         mutate(post_temp_sq = post_temp*post_temp) %>%
#         arrange(country, study_period, pid, year)

t <- with(subset(df_sample_1, post_temp>0), prop.table(table(post_temp)))
t <- data.frame(t)
t$cumsum <- cumsum(t$Freq)
t

# Save data sets ----

saveRDS(df_sample_1, file = paste0(data_files, "df_sample_clean_unbalanced.rds"))
