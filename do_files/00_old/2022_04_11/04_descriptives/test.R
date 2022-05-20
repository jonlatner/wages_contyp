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
results = "results/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter_01 <- readRDS(file = paste0(data_files,"3a_df_filter_steps.rds"))
df_filter_01 <- df_filter_01 %>%
        filter(country!="NE-LISS")
table(df_filter_01$country)
df_filter_01$country <- recode(df_filter_01$country, "'NE-LSP'='NE'")

df_event_data_01 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))
df_event_data_01 <- df_event_data_01 %>%
        filter(country!="NE-LISS")
table(df_event_data_01$country)
df_event_data_01$country <- recode(df_event_data_01$country, "'NE-LSP'='NE'")

# clean data -----

# FE/FEIS - keep if employed
df_employed <- df_event_data_01 %>%
        filter(unmp == 0) %>%
        select(country,pid,year,temp,event_t_p_yes) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() 

# if treated, then must be employed after treatment
# if not treated, then must be employed
# must be observable at least 3 times

# Temp to perm
df_event_t_p <- df_event_data_01 %>%
        filter((event_t_p_yes == 0 & unmp == 0) | (event_t_p_yes == 1 & event_p_t_drop_01 == 0 & event_p_t_drop_02 == 0)) %>%
        select(country,pid,year,event_t_p_yes) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup()
