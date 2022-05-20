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
library(texreg)
library(feisr)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        mutate(perm = ifelse(emp_status==2, yes = 1, no = 0)) %>%
        filter(country == "DE") %>%
        filter(study_period == 2010) %>%
        # filter(emp_status>0) %>%
        rename(wage=ln_hourly_wage) %>%
        select(pid,year,emp_status,unmp,temp,perm,wage,period,post_temp,post_unmp,age)

df_id <- df_sample_1 %>%
        select(pid) %>%
        group_by(pid) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(id = row_number())

df_sample_2 <- merge(df_sample_1,df_id) %>%
        arrange(pid,year)

# df_sample_2 <- df_sample_2 %>%
#         filter(id < 5000)

df_sample_2 %>%
        group_by(post_temp) %>%
        summarise(mean=mean(wage)) %>%
        ungroup()
df_sample_2 %>%
        group_by(emp_status) %>%
        summarise(mean=mean(wage)) %>%
        ungroup()

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

df_sample_2 <- data.frame(df_sample_2)
fe_model <- feis(wage ~ unmp + temp + factor(year) | 1, 
                   data = df_sample_2, 
                   id = "pid")

feis_model <- feis(wage ~unmp + temp + factor(post_temp) | year, 
                   data = df_sample_2, 
                   id = "pid")

screenreg(list(fe_model,feis_model))
# screenreg(list(fe_model,feis_model), omit.coef = c("year|post"))
