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
library(feisr)
library(texreg)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean_unbalanced.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(unmp==0) %>%
        select(country,study_period,pid,year,temp,ln_hourly_wage,period,post_temp) %>%
        group_by(country,study_period,pid) %>%
        mutate(count = row_number(),
               max = max(count)) %>%
        ungroup() %>%
        filter(max>2) %>%
        mutate(post_temp_sq = post_temp * post_temp)

# prepare for output ----
df_output = data.frame() # output
df_yhat <- data.frame()

# post model ----

country <- c("DE")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        year <- 2002
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model_cat <- feis(ln_hourly_wage ~ temp + factor(post_temp) | year, 
                                       data = df_period, 
                                       id = "pid")
                feis_model_lin <- feis(ln_hourly_wage ~ temp + post_temp | year, 
                                   data = df_period, 
                                   id = "pid")
                feis_model_sq <- feis(ln_hourly_wage ~ temp + post_temp + post_temp_sq | year, 
                                   data = df_period, 
                                   id = "pid")


        }
}

screenreg(list(feis_model_cat,feis_model_lin,feis_model_sq))

