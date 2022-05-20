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

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# problem with CH, study period == 2004 | study period == 2005 & age_cat_3 == 1

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(sample_perm == 1 | sample_temp == 1) %>%
        mutate(post_temp_2 = post_temp*post_temp) %>%
        filter(study_period>=2000 & study_period < 2013)

with(df_sample_1,table(study_period,country))

with(df_sample_1, table(country,edu_cat))

df_country <- df_sample_1 %>%
        filter(country == "KO")

with(df_country, table(study_period,edu_cat))

