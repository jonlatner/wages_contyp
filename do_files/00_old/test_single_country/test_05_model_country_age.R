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
library(feisr)
library(texreg)
library(dummies)
library(beepr)
library(robumeta) #group.center
library(estimatr)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds")) %>%
        filter(country=="DE")

# make sample -----

set.seed(1234)

df_sample_10_unique <- df_sample_0 %>%
        select(country,pid) %>%
        group_by(country,pid) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        sample_frac(.05) %>%
        mutate(keep=1)

df_sample_10 <- merge(df_sample_0,df_sample_10_unique) %>%
        filter(keep==1)

df_sample_0 <- df_sample_10

rm(df_sample_10_unique,df_sample_10)

# Save data -----

# library(haven)
# write_dta(df_sample_0,paste0(data_files,"sample.dta"))

# Clean data -----

df_event_1 <- df_sample_0 %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_1")) %>%
        rename(event_time_pos = event_1_time_pos)

# prepare for output ----

country <- c("DE")
event <- c(1)
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
        }
}

# Variables  ----

iv_vars_1 = c("age + event_time_pos_1 + event_time_pos_2 + event_time_pos_3 + event_time_pos_4 + event_time_pos_5 + event_time_pos_6 + event_time_pos_7 + event_time_pos_8 + year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 + year2018")

# Model  ----

df_country <- data.frame(df_country)
model_1 <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_vars_1,"| 1")),
              data = df_country, 
              id = "pid")

model_1_r <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_vars_1,"| 1")),
                data = df_country, robust = TRUE,
                id = "pid")


# Variables --------------------------------------------------------------

# Prepare data for transitory exit with individual fixed effects --------------------------------------------------------------

df_test <- df_country
vars = c("ln_hourly_wage", "age", "event_time_pos_1", "event_time_pos_2", "event_time_pos_3", "event_time_pos_4", "event_time_pos_5", "event_time_pos_6", "event_time_pos_7", "event_time_pos_8", "year2001", "year2002", "year2003", "year2004", "year2005", "year2006", "year2007", "year2008", "year2009", "year2010", "year2011", "year2012", "year2013", "year2014", "year2015", "year2016", "year2017", "year2018")

for (v in vars) {
        df_test$test <- group.center(df_test[[v]], df_test$pid)
        df_test$test <- as.numeric(df_test$test)
        df_test[[v]] <- NULL
        names(df_test)[names(df_test) == "test"] <- paste0(v)
        
}

model_felm <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_vars_1)),
                data = df_test)
# compute Stata like df-adjustment
G <- length(unique(df_test$pid))
N <- length(df_test$pid)
dfa <- (G/(G - 1)) * (N - 1)/model_felm$df.residual

# display with cluster VCE and df-adjustment
firm_c_vcov <- dfa * vcovHC(model_felm, type = "HC0", cluster = "group", adjust = T)
coeftest(model_felm, vcov = firm_c_vcov)

summary(model_felm)
summary(model_felm, cluster=c("pid"))

library(clubSandwich)

summary(model_1_r)

model_felm_r <- lm_robust(as.formula(paste0("ln_hourly_wage ~ ",iv_vars_1)),
                 data = df_test, se_type = "stata")
summary(model_felm_r)

library(multiwayvcov)
library(lmtest)
vcov_both <- cluster.vcov(model_felm, cbind(df_test$pid, df_test$year))
coeftest(model_felm, vcov_both)

# table ----

screenreg(list(model_1,model_1_r,model_felm), 
          omit.coef = c("year")
)

newdata_neg_2 = data.frame(age=0,
                           event_time_pos_1=1,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

yhat_neg_2 <- data.frame(predict(object = model_felm_r,
                                 se.fit = TRUE,
                                 newdata = newdata_neg_2
))

%>%
        select(fit,se.fit)
yhat_neg_2$post <- -2
yhat_neg_2
