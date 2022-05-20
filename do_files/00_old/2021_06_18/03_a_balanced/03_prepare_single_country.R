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
oecd_files = "projects/mobility/support_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data -----

df_ne_liss <- readRDS(paste0(data_files, "NE/LISS/ne_sample.rds")) 

df_sample_0 <- rbind(df_ne_liss)

# filter ----

df_sample_1 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018) %>%
        filter(lfp == 1) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(age >= 25 & age <= 54) %>%
        filter(unmp == 1 | (temp == 0 | temp == 1)) %>%
        filter(unmp == 1 | (unmp==0 & hours > 1)) %>%
        filter(unmp == 1 | (unmp==0 & ln_hourly_wage > 0)) %>%
        arrange(country,pid,year)

with(df_sample_1,table(unmp,year))

# cleaning ----

# remove individuals who are both unemployed and have a work contract (temp or permanent)
df_sample_1 <- df_sample_1 %>%
        mutate(test = ifelse(unmp == 1 & !is.na(temp), yes = 1, no = 0)) %>%
        filter(test == 0) %>%
        select(-test)
# with(df_sample_1,table(unmp,temp,useNA = "ifany"))

# make Inf = NA
# If unemployed, then log_hourly_wage == 0
df_sample_1 <- df_sample_1 %>%
        mutate(ln_hourly_wage = ifelse(is.infinite(ln_hourly_wage), yes = NA, no = ln_hourly_wage)) %>%
        mutate(ln_hourly_wage = ifelse(unmp==1, yes = 0, no = ln_hourly_wage))
describe(df_sample_1$ln_hourly_wage)

# test <- select(df_sample_1, country, pid, year, unmp, hours, wages, ln_hourly_wage) %>%
#                 filter(ln_hourly_wage>0) %>%
#                 mutate(hourly_wage = exp(ln_hourly_wage))
# describe(test$ln_hourly_wage)
# summary(test)

# test <- select(df_sample_1, country, pid, year, hours, wages, ln_hourly_wage) %>% 
# test <- arrange(test,hours)
# describe(test$ln_hourly_wage)
# describe(test$wages)
# table(test$country)

#Study period ----


country <- c("NE-LISS")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- unique(sort(df_country$year))
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 6)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)

                #keep each year of 7 year study period
                df_period <- group_by(df_period, pid) %>%
                        mutate(count = row_number(),
                               total = last(count)) %>%
                        ungroup() %>%
                        filter(total==7) %>%
                        mutate(study_period = min(year))
                        
        
                assign(paste("df", c, y, sep = "_"), df_period)
        }
}

rm(df_period,df_country)

#Append data sets ----

df_sample_2 = data.frame()
country <- sort(unique(df_sample_1$country))
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- sort(unique(df_country$year))
        for(y in year) {
                print(y)
                df_country_year <- get(paste("df", c, y, sep = "_"))
                df_sample_2 <- rbind(df_sample_2,df_country_year)
        }
}
rm(df_country,df_country_year)

# remove objects
for(c in country) {
        rm(list=ls(pattern=paste0("df_",c)))
}

# Save data sets ----

# saveRDS(df_sample_2, file = paste0(data_files, "df_sample.rds"))

# Save filtering steps ----

# # raw data
# step0a <- df_sample_0 %>%
#         group_by(pid) %>%
#         filter(row_number()==n()) %>%
#         ungroup()
# 
# step0b <- with(step0a,table(country))
# 
# # individual filters
# step1a <- df_sample_1 %>%
#         group_by(pid) %>%
#         filter(row_number()==n()) %>%
#         ungroup()
# 
# step1b <- with(step1a,table(country))
# 
# # panel filters - present in each year of the study period
# step2a <- df_sample_2 %>%
#         group_by(country,pid) %>%
#         filter(row_number()==n()) %>%
#         ungroup()
# step2b <- with(step2a,table(country))
# step2b
# 
# # total n
# step3a <- df_sample_2
# step3b <- with(step3a,table(country))
# 
# df_filter_steps <- rbind(step0b,step1b,step2b,step3b)
# df_filter_steps
# 
# write.csv(df_filter_steps, file = paste0(data_files, "df_filter_steps.csv"))

