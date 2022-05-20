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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"
results = "results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_original <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event"),unemployment_rate) 

df_original <- df_original %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup()
table(df_original$max)

df_original %>% select(country,pid,year,age,unmp,temp,perm,max) %>% filter(max == 1)

# clean data -----

df_event_t_p <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_t_p"),unemployment_rate) %>%
        rename(event = event_t_p_step,
               event_time_pos = event_t_p_time_pos)

df_event_p_t <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_p_t"),unemployment_rate) %>%
        rename(event = event_p_t_step,
               event_time_pos = event_p_t_time_pos)

# Model ----

model_fe <- feis(ln_hourly_wage ~ temp | 1,
                   data = data.frame(df_original), 
                   robust = TRUE,
                   id = "pid")

model_feis <- feis(ln_hourly_wage ~ temp | year,
                   data = data.frame(df_original), 
                   robust = TRUE,
                   id = "pid")

screenreg(
        list(model_fe, model_feis),
        custom.model.names = c("FE", "FEIS"),
        table = FALSE,
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
)


# step impact function
event <- c("t_p","p_t")
for (e in event) {
        print(paste0("event = ", e))
        df_event <- get(paste0("df_event_",e))
        
        # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
        df_event$event_time_pos <- relevel(factor(df_event$event_time_pos), ref = "2")
        df_event <- data.frame(df_event)
        model <- feis(ln_hourly_wage ~ event | 1,
                      data = df_event, 
                      robust = TRUE,
                      id = "pid")
        
        assign(paste0("model_step_",e),model)
}

# dummy impact function
event <- c("t_p","p_t")
for (e in event) {
        print(paste0("event = ", e))
        df_event <- get(paste0("df_event_",e))
        
        # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
        df_event$event_time_pos <- relevel(factor(df_event$event_time_pos), ref = "2")
        df_event <- data.frame(df_event)
        model <- feis(ln_hourly_wage ~ event_time_pos | 1,
                      data = df_event, 
                      robust = TRUE,
                      id = "pid")
        
        assign(paste0("model_dummy_",e),model)
}

screenreg(
        list(model_fe, model_feis, model_step_t_p, model_dummy_t_p, model_step_p_t, model_dummy_p_t),
        custom.model.names = c("FE", "FEIS", "Temp to perm (step IF)", "Temp to perm (dummy IF)", "Perm to temp (step IF)", "Perm to temp (dummy IF)"),
        table = FALSE,
        custom.coef.map = list(
                "temp"="temp",
                "event"="event",
                "event_time_pos1"="Pre event (-2)",
                "event_time_pos3"="Event",
                "event_time_pos4"="Post event (+1)",
                "event_time_pos5"="Post event (+2)",
                "event_time_pos6"="Post event (+3)",
                "event_time_pos7"="Post event (+4)"
        ),
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
)

texreg(
        list(model_fe, model_feis, model_step_t_p, model_dummy_t_p, model_step_p_t, model_dummy_p_t),
        custom.header = list("FE" = 1, "FEIS" = 2, "Temp to perm" = 3:4, "Perm to temp" = 5:6),
        custom.model.names = c("(1)", "(2)", "(3) Step IF", "(4) Dummy IF", "(5) Step IF", "(6) Dummy IF"),
        table = FALSE,
        custom.coef.map = list(
                "temp"="temp",
                "event"="event",
                "event_time_pos1"="Pre event (-2)",
                "event_time_pos3"="Event",
                "event_time_pos4"="Post event (+1)",
                "event_time_pos5"="Post event (+2)",
                "event_time_pos6"="Post event (+3)",
                "event_time_pos7"="Post event (+4)"
        ),
        booktabs = TRUE, use.packages = FALSE, 
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
        file = paste0(tables,"table_compare_models_single_country.tex"),
)
