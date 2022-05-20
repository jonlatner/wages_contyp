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
results = "projects/mobility/results/balanced/"

# LIBRARY
library(tidyverse)
library(feisr)
library(texreg)
library(dummies)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(country=="DE" & study_period==2001) %>%
        select(country,study_period,pid,year,period,ln_hourly_wage,unmp,temp,perm,event_1a,event_2a)

df_dummy <- dummy(x = df_sample_1$period, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)

# prepare for output ----

df_output = data.frame() # output
df_model_yhat <- data.frame()

# fe model ----

# model
df_period <- data.frame(df_sample_1)
model_fe <- feis(ln_hourly_wage ~ unmp + temp + period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1, 
              data = df_period, 
              id = "pid")

model_fe_event_1a <- feis(ln_hourly_wage ~ unmp + temp + event_1a + period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1, 
                 data = df_period, 
                 id = "pid")

model_fe_event_2a <- feis(ln_hourly_wage ~ unmp + temp + event_2a + period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1, 
                         data = df_period, 
                         id = "pid")

screenreg(list(model_fe,model_fe_event_1a,model_fe_event_2a))
