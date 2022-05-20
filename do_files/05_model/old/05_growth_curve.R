# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package,character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FoldS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"
results = "results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(plm)
library(dummies)
library(robumeta)
options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

# clean data -----

# Create new temp variable that equals the addition of the 2 step function variables
# This ensures that models only evaluate events that fit the criteria of an event

df_country_1 <- df_original %>%
        filter(country == "DE") %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm) %>%
        mutate(age_2=age*age,
               year = as.factor(year),
               age_cat = ifelse(age < 35, yes = 1,
                                ifelse(age>=35 & age < 45, yes = 2,
                                       ifelse(age>=45, yes = 3, no = NA))),
               young = ifelse(age_cat == 1, yes = 1, no = 0),
               old = ifelse(age_cat == 3, yes = 1, no = 0),
               temp_young = temp*young,
               temp_old = temp*old,
               temp_age_young = temp*age*young,
               temp_age_old = temp*age*old,
               age_cat = as.factor(age_cat),
               )

# library(haven)
# write_dta(data = df_country, path = paste0(data_files,"germany.dta"))

# prepare data

df_country_2 <- df_country_1

# create categorical dummy variables
df_dummy <- dummy(x = df_country_1$year)
df_country_2 <- cbind(df_country_2, df_dummy)
rm(df_dummy)

# center variables
vars = c(
        "unmp", 
        "temp",
        "age", "young", "old", 
        "temp_young", "temp_old", 
        "temp_age_young", "temp_age_old", 
        "year2000","year2001","year2002","year2003","year2004","year2005",
        "year2006","year2007","year2008","year2009","year2010","year2011",
        "year2012","year2013","year2014","year2015","year2016","year2017",
        "year2018",
        "ln_hourly_wage"
)

for (v in vars) {
        df_country_2$test <- group.center(df_country_2[[v]], df_country_2$pid) # create new variable (group centered)
        df_country_2$test <- as.numeric(df_country_2$test) # make it numeric (probably not necessary)
        df_country_2[[v]] <- NULL # drop old variable
        names(df_country_2)[names(df_country_2) == "test"] <- paste0(v) # rename new variable with old variable
        
}

# independent variables
iv_ols <-   "unmp + temp + 
                age + young + old + 
                temp_young + temp_old +
                temp_age_young + temp_age_old +
                year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 + year2018
"

# model data ----

# FE model
model_fe <- plm(ln_hourly_wage ~ unmp + temp + age + young + old + temp_young + temp_old + temp_age_young + temp_age_old + year,
                 data = data.frame(df_country_1), 
                 index = c("pid", "year"))

model_ols <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                data = data.frame(df_country_2))

# model_fe <- feis(ln_hourly_wage ~ ln_hourly_wage ~ unmp + temp + young + old + temp_young + temp_old + temp_age_young + temp_age_old + year | 1,
#                  data = data.frame(df_country),
#                  robust = TRUE,
#                  id = "pid")

screenreg(
        list(model_fe, model_ols),
        table = FALSE,
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
)

# graph data ----

yhat_younger_temp <- predict(object = model_ols, se.fit = TRUE, 
                         newdata = data.frame(unmp = 0, 
                                              temp = 1,
                                              age = c(25,30,34),
                                              young = 1, 
                                              old = 0,
                                              temp_young = 1,
                                              temp_old = 0,
                                              temp_age_young = c(25,30,34),
                                              temp_age_old = 0,
                                              year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                         ))


yhat_younger_perm <- predict(object = model_ols, se.fit = TRUE, 
                             newdata = data.frame(unmp = 0, 
                                                  temp = 0,
                                                  age = c(25,30,34),
                                                  young = 1, 
                                                  old = 0,
                                                  temp_young = 0,
                                                  temp_old = 0,
                                                  temp_age_young = c(25,30,34),
                                                  temp_age_old = 0,
                                                  year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                             ))

yhat_younger_temp
yhat_younger_perm

