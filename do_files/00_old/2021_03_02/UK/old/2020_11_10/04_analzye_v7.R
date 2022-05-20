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

data_files = "projects/booth_etal_2002/data_files/update/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 

# clean data -----

df_uk <- df_uk %>%
        mutate(wages = ifelse(wages<1, yes = 1, no = wages),
               ln_wages = log(wages)) %>%
        mutate(ftc = ifelse(is.na(ftc) & unmp == 1, yes = 0, no = ftc),
               tmp = ifelse(is.na(tmp) & unmp == 1, yes = 0, no = tmp),
               contyp = ifelse(is.na(contyp) & unmp == 1, yes = 0, no = contyp),
               prestige = ifelse(is.na(prestige) & unmp == 1, yes = 0, no = prestige))

df_vars <- df_uk %>%
        select(age_cat,edu_cat,post_ftc) %>%
        mutate_all(.funs = as.factor)

df_uk <- df_uk %>%
        select(-age_cat,-edu_cat,-post_ftc)

df_uk <- cbind(df_uk, df_vars)

df_uk$age_cat <- relevel(df_uk$age_cat, "2")
df_uk$edu_cat <- relevel(df_uk$edu_cat, "2")

df_unique <- df_uk %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

df_uk <- df_uk %>%
        group_by(pid) %>%
        arrange(pid,year) %>%
        mutate(male = first(male),
               edu_cat = last(edu_cat),
               age_cat = first(age_cat)) %>%
        ungroup()

with(df_uk,table(spell,max))

# independent variables -----

iv <-   "unmp + ftc*male + ftc*edu_cat + ftc*age_cat + post_ftc*male + post_ftc*edu_cat + post_ftc*age_cat + tmp + factor(year)"

# plm model -----

plm_model <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                       data = df_uk,
                       index = c("pid","year"))
summary(plm_model)


plot_model(plm_model, 
           type = "int")


