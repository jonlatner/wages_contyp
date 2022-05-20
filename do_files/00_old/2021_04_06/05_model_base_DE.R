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
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)
library(broom)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        select(country,study_period,pid,year,unmp,temp,perm,ln_hourly_wage,post_temp,post_temp_sq,period)

# prepare for output ----
df_table_output = data.frame() # output
df_model_yhat <- data.frame()

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

country <- c("AU","CH","DE","JP","KO","NE-LISS")
country <- c("DE")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(2000,2012)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", 
                        "post_temp", "post_temp_sq",
                        "period1","period2","period3","period4","period5", "period6", "period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + 
                # post_temp_sq +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period)

                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
        }
}

# Save data sets ----

