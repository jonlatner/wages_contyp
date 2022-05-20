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
        filter(country == "DE") %>%
        filter(study_period == 2012) %>%
        rename(wage=ln_hourly_wage) %>%
        select(pid,year,emp_status,unmp,temp,wage,period,post_temp)

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

df_sample_2 <- data.frame(df_sample_1)
fe_model_1 <- feis(wage ~ unmp + temp + factor(year) | 1, 
                   data = df_sample_2, 
                   id = "pid")

feis_model_1 <- feis(wage ~unmp + temp | year, 
                   data = df_sample_2, 
                   id = "pid")

screenreg(list(fe_model_1,feis_model_1), omit.coef = c("year"))

fe_model_2 <- feis(wage ~ unmp + temp + factor(post_temp) + factor(year) | 1, 
                   data = df_sample_2, 
                   id = "pid")

feis_model_2a <- feis(wage ~unmp + temp | year, 
                      data = df_sample_2, 
                      id = "pid")

feis_model_2b <- feis(wage ~unmp + temp + factor(post_temp) | year, 
                     data = df_sample_2, 
                     id = "pid")

# screenreg(list(fe_model_2,feis_model_2a,feis_model_2b), omit.coef = c("year"))
# screenreg(list(fe_model,feis_model_1,fe_model_2,feis_model_2))

df_sample_3 <- filter(df_sample_2, unmp==0)
fe_model_3b <- feis(wage ~ temp + factor(post_temp) + factor(year) | 1, 
                   data = df_sample_3, 
                   id = "pid")

feis_model_3a <- feis(wage ~ temp | year, 
                      data = df_sample_3, 
                      id = "pid")

feis_model_3b <- feis(wage ~ temp + factor(post_temp) | year, 
                     data = df_sample_3, 
                     id = "pid")

screenreg(list(fe_model_2,feis_model_2a,feis_model_2b), omit.coef = c("year"))
screenreg(list(fe_model_3b,feis_model_3a,feis_model_3b), omit.coef = c("year"))
# screenreg(list(fe_model,feis_model_1,fe_model_2,feis_model_2))
