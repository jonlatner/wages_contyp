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
options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

df_country <- df_original %>%
        filter(country == "DE")

# model -----

model_fe <- feis(ln_hourly_wage ~ age + unmp + temp + factor(year) | 1,
                 data = data.frame(df_country), 
                 robust = TRUE,
                 id = "pidseq")

model_feis <- feis(ln_hourly_wage ~ age + unmp + temp + factor(year) | year,
                 data = data.frame(df_country), 
                 robust = TRUE,
                 id = "pidseq")

df_country$event_u_p_time_pos <- relevel(factor(df_country$event_u_p_time_pos), ref = "2")
df_country$event_u_t_time_pos <- relevel(factor(df_country$event_u_t_time_pos), ref = "2")
df_country$event_t_p_time_pos <- relevel(factor(df_country$event_t_p_time_pos), ref = "2")
df_country$event_p_t_time_pos <- relevel(factor(df_country$event_p_t_time_pos), ref = "2")

model_fe_if <- feis(ln_hourly_wage ~ age + event_t_p_time_pos + event_p_t_time_pos + event_u_p_time_pos + event_u_t_time_pos + as.factor(year) | 1,
              data = data.frame(df_country), 
              robust = TRUE,
              id = "pidseq")

# model -----

screenreg(
        list(model_fe, model_feis, model_fe_if),
        table = FALSE,
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
)
