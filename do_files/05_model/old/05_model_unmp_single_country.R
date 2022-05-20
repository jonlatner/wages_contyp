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
library(broom)
library(broomExtra)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"03e_df_sample_cleaned_prepared_first_unmp_data.rds"))


df_original <- df_original %>%
        filter(country == "AU") %>%
        select(country, pid, year, age, ln_hourly_wage, matches("unmp")) %>%
        arrange(country, pid, year)

# create sample ----

# set.seed(1234)
# df_sample <- df_original %>%
#         select(country,pid) %>%
#         group_by(country,pid) %>%
#         slice(1) %>%
#         ungroup() %>%
#         sample_frac(.2,replace = FALSE) %>%
#         mutate(sample = 1) %>%
#         arrange(country,pid)
# df_original <- merge(df_original,df_sample,by = c("country", "pid")) %>%
#         filter(sample == 1) %>%
#         arrange(country, pid, year)

# Models ----

df_yhat <- data.frame()

# FE model
model_fe <- feis(ln_hourly_wage ~ age + unmp + factor(year) | 1,
                 data = data.frame(df_original), 
                 robust = TRUE,
                 id = "pid")

df_output <- tidy_parameters(model_fe)
df_output$event <- "FE"
df_yhat <- rbind(df_yhat,df_output)

# FEIS model
model_feis <- feis(ln_hourly_wage ~ age + unmp + factor(year) | year,
                   data = data.frame(df_original), 
                   robust = TRUE,
                   id = "pid")

df_output <- tidy_parameters(model_feis)
df_output$event <- "FEIS"
df_yhat <- rbind(df_yhat,df_output)

# Dummy IF Models ---- 

# model - baseline is 1 period before event.  in annual data this is event_unmp_time_pos==2
df_original$event_unmp_time_pos <- relevel(factor(df_original$event_unmp_time_pos), ref = "2")
model_event_dummy <- feis(ln_hourly_wage ~ age + event_unmp_time_pos + as.factor(year) | 1,
                        data = data.frame(df_original), 
                        robust = TRUE,
                        id = "pid")

df_output <- tidy_parameters(model_event_dummy)
df_output$event <- "dummy"
df_yhat <- rbind(df_yhat,df_output)


# Table ----

screenreg(
        list(model_fe,model_feis,model_event_dummy),
        custom.model.names = c("FE", "FEIS", "IF dummy"),
        table = FALSE,
        # custom.coef.map = list(
        #         "event_unmp_time_pos3"="event",
        #         "event_unmp_time_pos1"="Pre event (-2)",
        #         "event_unmp_time_pos2"="Pre event (-1)",
        #         "event_unmp_time_pos4"="Post event (+1)",
        #         "event_unmp_time_pos5"="Post event (+2)",
        #         "event_unmp_time_pos6"="Post event (+3)",
        #         "event_unmp_time_pos7"="Post event (+4)"
        # ),
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept|position"),
)


# beep()

