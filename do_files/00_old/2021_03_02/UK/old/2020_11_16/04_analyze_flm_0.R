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
library(dummies)
library(robumeta)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 

# clean data -----

df_uk <- df_uk %>%
        mutate(wages = ifelse(wages<1, yes = 1, no = wages),
               ln_wages = log(wages)) %>%
        mutate(ftc = ifelse(is.na(ftc) & unmp == 1, yes = 0, no = ftc),
               tmp = ifelse(is.na(tmp) & unmp == 1, yes = 0, no = tmp),
               prestige = ifelse(is.na(prestige) & unmp == 1, yes = 0, no = prestige))

df_uk <- df_uk %>%
        group_by(pid) %>%
        arrange(pid,year) %>%
        mutate(male = first(male),
               edu_cat = last(edu_cat),
               age_cat = first(age_cat)) %>%
        ungroup()

with(df_uk,table(spell,max))

# Prepare data for individual fixed effects ----

df_uk_cntr <- df_uk

# create categorical dummy variables
df_dummy <- dummy(x = df_uk_cntr$year)
df_uk_cntr <- cbind(df_uk_cntr, df_dummy)

df_dummy <- dummy(x = df_uk_cntr$post_ftc)
df_uk_cntr <- cbind(df_uk_cntr, df_dummy)
rm(df_dummy)

# center variables
vars = c(
        "unmp", "tmp", "ftc", 
        "post_ftc1", "post_ftc2", "post_ftc3", "post_ftc4", "post_ftc5",
        "ln_wages"
)

for (v in vars) {
        df_uk_cntr$test <- group.center(df_uk_cntr[[v]], df_uk_cntr$pid) # create new variable (group centered)
        df_uk_cntr$test <- as.numeric(df_uk_cntr$test) # make it numeric (probably not necessary)
        df_uk_cntr[[v]] <- NULL # drop old variable
        names(df_uk_cntr)[names(df_uk_cntr) == "test"] <- paste0(v) # rename new variable with old variable
}

# independent variables -----

iv_plm <-   "unmp + tmp + ftc + as.factor(post_ftc) + factor(year)"

iv_ols <-   "unmp + tmp + ftc +
post_ftc1 + post_ftc2 + post_ftc3 + post_ftc4 + post_ftc5 + 
year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016"

# plm model -----

plm_model <- plm(as.formula(paste0("ln_wages ~ ",iv_plm)),
                 data = df_uk,
                 index = c("pid","year"))

ols_model <- lm(as.formula(paste0("ln_wages ~ ",iv_ols)),
                data = df_uk_cntr)

stargazer(plm_model, ols_model, type = "text", keep = c("ftc", "post"))
