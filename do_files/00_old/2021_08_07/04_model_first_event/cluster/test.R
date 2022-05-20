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

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
tables = "projects/mobility/tables/"
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(dummies)
library(robumeta) #group.center
library(beepr)
library(estimatr)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"df_sample_first_clean.rds"))

df_original <- readRDS(paste0(data_files,"df_sample_first_clean_sample.rds"))

# clean data -----

df_event_t_p <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_t_p")) %>%
        rename(event_time_pos = event_t_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_p <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_u_p")) %>%
        rename(event_time_pos = event_u_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_t <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_u_t")) %>%
        rename(event_time_pos = event_u_t_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_p_t <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_p_t")) %>%
        rename(event_time_pos = event_p_t_time_pos) %>%
        filter(!is.na(event_time_pos))

# examine some data ----

# df_test <- df_event_u_p %>%
#         filter(event_u_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,matches("event"))
# with(df_test,table(event_u_p_time,event_time_pos))

# df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)

# df_test <- df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)
# with(df_test,table(event_t_p_time,event_time_pos))

with(df_event_t_p,table(country,event_t_p_yes))

# Prepare for models ----
# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("t_p","p_t","u_p","u_t")
event <- c("t_p")

# Variables 

vars_annual = c("ln_hourly_wage", "event_time_pos_1", "event_time_pos_3", "event_time_pos_4", "event_time_pos_5", "event_time_pos_6", "event_time_pos_7")
vars_biannual = c("ln_hourly_wage", "event_time_pos_3", "event_time_pos_5", "event_time_pos_7")

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
country_all <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")
country <- c("DE")

# Models ---- 

for(c in country) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # create year variables
                df_dummy <- dummy(x = df_country$year,sep = "")
                df_country <- cbind(df_country,df_dummy)
                
                df_dummy <- dummy(x = df_country$event_time_pos,sep = "_")
                df_country <- cbind(df_country,df_dummy)
                
                year <- sort(unique(df_country$year))
                year_vars = c()
                for(y in year) {
                        year_vars <- c(year_vars,paste0("year",as.character(y)))
                }
                remove_base_year_vars <- c(year_vars[1]) # drop baseline
                year_vars <- year_vars [! year_vars %in% remove_base_year_vars]
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_annual,year_vars)
                # vars <- c(vars_annual)
                
                for (v in vars) {
                        df_country$test <- group.center(df_country[[v]], df_country$pid)
                        df_country$test <- as.numeric(df_country$test)
                        df_country[[v]] <- NULL
                        names(df_country)[names(df_country) == "test"] <- paste0(v)
                }
                
                # create independent variables
                vars_all = c()
                for (v in vars) {
                        vars_all <- paste(vars_all,v,"+")
                }
                vars_all <- sub("..$", "", vars_all)
                
                # model
                df_country <- data.frame(df_country)
                model_r <- lm_robust(as.formula(paste0("ln_hourly_wage ~ ",vars_all)),
                                   fixed_effects = ~pid,
                                   return_vcov = TRUE,
                                   se_type = "stata",
                                   data = df_country)

                model <- lm(as.formula(paste0("ln_hourly_wage ~ ",vars_all)),
                                     data = df_country)
                
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

test <- tidy(model)
test

#ROBUST
screenreg(list(model_r,model),
       # custom.coef.names = c("Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
       table = FALSE, include.ci = FALSE,
       include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
       omit.coef = c("year|Intercept"))


newdata_0 = data.frame(age=0,event_time_pos_1=1,event_time_pos_2=0, # pre event
                       event_time_pos_3=0, # event
                       event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0, # post event
                       year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                       year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

yhat_0 <- predict(object = model,
                             se.fit = TRUE,
                             newdata = newdata_0)
yhat_0

library(car)
bootfit1 <- bootCase(model, function(x)predict(x, newdata_0), B=100)
