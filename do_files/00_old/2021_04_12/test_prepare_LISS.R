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
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data -----

df_ne_liss <- readRDS(paste0(data_files, "NE/LISS/ne_sample.rds")) 

df_sample_0 <- df_ne_liss

# filter ----
df_sample_0 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)

df_sample_1 <- df_sample_0 %>%
        filter(lfp == 1) %>%
        filter(slf!=1) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        mutate(ln_hourly_wage = ifelse(is.infinite(ln_hourly_wage), yes = NA, no = ln_hourly_wage)) %>%
        filter(unmp == 1 | (unmp==0 & !is.na(temp))) %>% # unemployed or employed with a work contract (temp or perm)
        filter(unmp == 1 | (unmp==0 & ln_hourly_wage >= 1)) %>% # unemployed or employed with hourly wages > 1
        filter(age >= 25 & age <= 54) %>%
        select(-lfp,-slf,-occ,-wages,-hours) %>%
        arrange(country,pid,year)

step0 <- with(df_sample_0,table(country))
step1 <- with(df_sample_1,table(country))

#Study period ----

country <- c("NE-LISS")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- unique(sort(df_country$year))
        year <- 2010
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 6)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)

                #keep each year of 7 year study period
                df_period <- group_by(df_period, pid) %>%
                        mutate(count = row_number(),
                               # first = first(year),
                               # last = last(year),
                               total = last(count)) %>%
                        mutate(sample_perm = ifelse(row_number()==1 & unmp == 0 & temp == 0, yes = 1, no = 0),
                               sample_perm = first(sample_perm)) %>%
                        mutate(sample_temp = ifelse(row_number()==1 & unmp == 0 & temp == 1, yes = 1, no = 0),
                               sample_temp = first(sample_temp)) %>%
                        mutate(sample_unmp = ifelse(row_number()==1 & unmp == 1, yes = 1, no = 0),
                               sample_unmp = first(sample_unmp)) %>%
                        ungroup() %>%
                        # filter(total==7) %>%
                        mutate(study_period = min(year))

                assign(paste("df", c, y, sep = "_"), df_period)
                
                }
}
with(df_period,table(total,year))      

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

