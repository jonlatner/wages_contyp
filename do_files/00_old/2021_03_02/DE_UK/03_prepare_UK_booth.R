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

df_uk <- readRDS(paste0(data_files, "UK/uk_sample.rds")) 

df_sample_0 <- df_uk

# filter ----
df_sample_0 <- df_sample_0 %>%
        filter(year>=1991 & year<=1997)
        
df_sample_1 <- df_sample_0 %>%
        filter(lfp == 1) %>%
        filter(slf!=1) %>%
        filter(!is.na(age)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(hours>0) %>%
        filter(!is.na(temp)) %>%
        mutate(ln_hourly_wage = ifelse(is.infinite(ln_hourly_wage), yes = NA, no = ln_hourly_wage)) %>%
        filter(!is.na(ln_hourly_wage)) %>%
        # filter(unmp == 1 | (unmp==0 & !is.na(temp))) %>% # unemployed or employed with a work contract (temp or perm)
        # filter(unmp == 1 | (unmp==0 & ln_hourly_wage >= 1)) %>% # unemployed or employed with hourly wages > 1
        filter(age <= 60) %>%
        select(-lfp,-slf,-occ,-wages,-hours) %>%
        arrange(country,pid,year)

df_booth <- df_sample_1 %>%
        group_by(pid) %>%
        slice(1) %>%
        ungroup()

with(df_booth,table(male))
