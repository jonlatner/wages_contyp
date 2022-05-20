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
library(texreg)
library(dummies)
library(robumeta)
library(ggplot2)
library(broom)
library(estimatr) # cluster robust standard errors
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(sample_perm == 1 | sample_temp == 1) %>%
        filter(sample_perm == 1 | sample_temp == 1) %>%
        mutate(post_temp_2 = post_temp*post_temp,
               edu_cat = as.numeric(edu_cat)) %>%
        mutate(edu_cat = ifelse((country == "JP" & edu_cat == 1) | (country == "CH" & edu_cat == 1), yes = 2, no = edu_cat)) %>%
        mutate(age_cat = ifelse((country == "CH" & age_cat == 3), yes = 2, no = age_cat)) %>%
        filter(study_period>=2000 & study_period < 2013)

with(df_sample_1, table(study_period,country))

df_dummy <- dummy(x = df_sample_1$age_cat)
df_sample_1 <- cbind(df_sample_1, df_dummy)

df_dummy <- dummy(x = df_sample_1$edu_cat)
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

# prepare for output ----
df_table_output = data.frame() # output
df_model_yhat <- data.frame()

# model data ----

country <- c("NE-LISS")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)

                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat3", 
                        "age_cat1", 
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2",
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                # age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                # edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                # male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                
                # model
                ols_model_r <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period)
                assign(paste0("NE_",y),ols_model_r)
                

          }
}

screenreg(list(NE_2008,IT_2002,IT_2004,IT_2006,IT_2008,IT_2010), 
       file = paste0(tables,"Italy.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$",
                             "Younger age (25-34) $\\times$ Temp contract",
                             "Younger age (25-34) $\\times$ Years after",
                             "Younger age (25-34) $\\times$ Years after$^{2}$",
                             "Older age (45-54) $\\times$ Temp contract",
                             "Older age (45-54) $\\times$ Years after",
                             "Older age (45-54) $\\times$ Years after$^{2}$",
                             "Lower education ($<$ Secondary) $\\times$ Temp contract",
                             "Lower education ($<$ Secondary) $\\times$ Years after",
                             "Lower education ($<$ Secondary) $\\times$ Years after$^{2}$",
                             "Higher education ($>$ Secondary) $\\times$ Temp contract",
                             "Higher education ($>$ Secondary) $\\times$ Years after",
                             "Higher education ($>$ Secondary) $\\times$ Years after$^{2}$",
                             "Male $\\times$ Temp contract",
                             "Male $\\times$ Years after",
                             "Male $\\times$ Years after$^{2}$"
       ),
       custom.model.names = c("2000", "2002", "2004", "2006", "2008", "2010"),
       # stars = c(0.05, 0.01, 0.001), 
       # custom.note = c("Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

