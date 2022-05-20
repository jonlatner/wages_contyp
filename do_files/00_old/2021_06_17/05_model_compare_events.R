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
results = "projects/mobility/results/balanced/"

# LIBRARY
library(tidyverse)
library(feisr)
library(texreg)
library(dummies)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(country == "DE") %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_2|event_3")) %>%
        mutate(post_event_2 = ifelse(post_event_2 > 5, yes = 5, no = post_event_2)) %>%
        mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3)) 

df_sample_2 <- df_sample_0 %>%
        filter(country == "DE") %>%
        filter(pid == 8302) %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_2|event_3")) %>%
        mutate(post_event_2 = ifelse(post_event_2 > 5, yes = 5, no = post_event_2)) %>%
        mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3)) 
filter(df_sample_2, event_2_yes == 1) %>% select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_2")) %>% print(n=20)
filter(df_sample_2, event_3_yes == 1) %>% select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_3")) %>% print(n=20)

df_dummy <- dummy(x = df_sample_1$post_event_2,sep = "_")
df_sample_1 <- cbind(df_sample_1,df_dummy)

df_dummy <- dummy(x = df_sample_1$post_event_3,sep = "_")
df_sample_1 <- cbind(df_sample_1,df_dummy)

df_sample_1 <- df_sample_1 %>%
        filter(pid < 10000)

# IVs  ----

iv_vars_event_2 = c("unmp", 
                    "pre_event_2",
                    "event_2", 
                    "post_event_2_1", "post_event_2_2", "post_event_2_3", "post_event_2_4", "post_event_2_5"
)

iv_vars_event_3 = c("unmp", 
                    "pre_event_3",
                    "event_3", 
                    "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)

# annual data
df_country <- df_sample_1

# create year variables
df_dummy <- dummy(x = df_country$year,sep = "")
df_country <- cbind(df_country,df_dummy)

year <- sort(unique(df_country$year))
year_vars = c()
for(y in year) {
        year_vars <- c(year_vars,paste0("year",as.character(y)))
}
remove <- c(year_vars[1]) # drop baseline
year_vars <- year_vars [! year_vars %in% remove]

# create independent variables
vars_event_2 <- c(iv_vars_event_2,year_vars)
vars_event_3 <- c(iv_vars_event_3,year_vars)

iv_all_event_2 = c()
for (v in vars_event_2) {
        iv_all_event_2 <- paste(iv_all_event_2,v,"+")
}
iv_all_event_2 <- sub("..$", "", iv_all_event_2)

iv_all_event_3 = c()
for (v in vars_event_3) {
        iv_all_event_3 <- paste(iv_all_event_3,v,"+")
}
iv_all_event_3 <- sub("..$", "", iv_all_event_3)

rm(y,year,year_vars,v,remove,vars_event_2,vars_event_3,iv_vars_event_2,iv_vars_event_3)

# model
df_country <- data.frame(df_country)
model_event_2 <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_all_event_2,"| 1")),
              data = df_country, 
              id = "pid")

df_country <- data.frame(df_country)
model_event_3 <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_all_event_3,"| 1")),
              data = df_country, 
              id = "pid")

screenreg(list(model_event_2,model_event_3), 
          custom.model.names = c("event 2 (unmp to perm)", "event 3 (unmp to temp)"),
          # omit.coef = c("year")
          omit.coef = c("year|post|pre")
          )
