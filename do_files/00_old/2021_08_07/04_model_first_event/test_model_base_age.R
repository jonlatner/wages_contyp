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
library(broom)
library(miceadds)
library(estimatr)
library(feisr)
library(plm)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"df_sample_first_clean.rds"))

# df_original <- readRDS(paste0(data_files,"df_sample_first_clean_sample.rds"))

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
# with(df_event_t_p,table(country,event_t_p_yes))

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("t_p","p_t","u_p","u_t")
event <- c("t_p")

# Variables 

iv_vars_annual =   c("age + event_time_pos_1 + event_time_pos_3 + event_time_pos_4 + event_time_pos_5 + event_time_pos_6 + event_time_pos_7")
iv_vars_biannual = c("age + event_time_pos_3 + event_time_pos_5 + event_time_pos_7")

vars_annual = c("ln_hourly_wage", "age", "event_time_pos_1", "event_time_pos_3", "event_time_pos_4", "event_time_pos_5", "event_time_pos_6", "event_time_pos_7")
vars_biannual = c("ln_hourly_wage", "age", "event_time_pos_3", "event_time_pos_5", "event_time_pos_7")

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
country_all <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")
country_de <- c("DE")

# Models ---- 

# Annual countries) 
for(c in country_de) {
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
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_annual,year_vars)
                
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

                vars_all <- sub("..$", "", vars_all) # replaces plus after last variable
                vars_all_2 <- sub(" + ", " ~ ", vars_all, fixed = TRUE)
                vars_all_2
                
                
                # model
                df_country <- data.frame(df_country)
                model <- lm(as.formula(paste0(vars_all_2)),
                                       data = df_country)

                df_output <- tidy(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}

# Tables ----

model_r <- lm_robust(as.formula(paste0(vars_all_2)),
                     clusters = pid,
                     se_type = "stata",
            data = df_country)

df_test <- df_event %>%
        filter(country == c) %>%
        group_by(country,pid) %>%
        mutate(test = ifelse())
df_test$event_time_pos <- relevel(factor(df_test$event_time_pos), ref = "2")
df_test <- data.frame(df_test)
model_feis <- feis(ln_hourly_wage ~ age + event_time_pos + as.factor(year) | 1,
              data = df_test, 
              robust = TRUE,
              id = "pid")

model_plm <- plm(ln_hourly_wage ~ age + event_time_pos + as.factor(year),
                   data = df_test, 
                   index = c("pid","year"))


screenreg(list(model,model_r,model_feis,model_plm),
          # custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
          # custom.coef.names = c("Age", "Pre event (-2)", "Pre event (-1)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
          table = FALSE,include.ci = FALSE,
          include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
          omit.coef = c("year|Intercept"),
          )
