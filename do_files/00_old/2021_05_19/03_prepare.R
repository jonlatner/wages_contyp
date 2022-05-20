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

df_au <- readRDS(paste0(data_files, "AU/au_sample.rds")) 
df_ch <- readRDS(paste0(data_files, "CH/ch_sample.rds")) 
df_de <- readRDS(paste0(data_files, "DE/de_sample.rds")) 
df_uk <- readRDS(paste0(data_files, "UK/uk_sample.rds")) 
df_jp <- readRDS(paste0(data_files, "JP/jp_sample.rds")) 
df_ko <- readRDS(paste0(data_files, "KO/ko_sample.rds")) 
df_ne_liss <- readRDS(paste0(data_files, "NE/LISS/ne_sample.rds")) 
df_ne_lsp <- readRDS(paste0(data_files, "NE/LSP/ne_sample.rds")) 
df_it <- readRDS(paste0(data_files, "IT/it_sample.rds")) 

df_sample_0 <- rbind(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne_liss,df_ne_lsp,df_it)

rm(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne_liss,df_it,df_ne_lsp)

# cleaning ----
df_sample_0 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)

df_sample_0 <- df_sample_0 %>%
        mutate(test = ifelse(unmp == 1 & !is.na(temp), yes = 1, no = 0)) %>%
        filter(test == 0) %>%
        select(-test)

# filter ----

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

#Study period ----

country <- c("IT", "NE-LSP")
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
                        filter(total==4) %>%
                        mutate(study_period = min(year))

assign(paste("df", c, y, sep = "_"), df_period)
}
}

# problem with UK data
# First interviews with BHPS participants in Understanding Society were carried out in Wave 2 of the Study in 2010-2011.
# Therefore, we include everyone with at least 6 observations and the first and last observation in a given study period

country <- c("UK")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year_1 <- c(2000,2001,2002)
        year_2 <- c(seq(2009,2018,1))
        year <- c(year_1,year_2)
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 6)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)
                
                #keep each year of 7 year study period
                df_period <- group_by(df_period, pid) %>%
                        mutate(count = row_number(),
                               first = first(year),
                               last = last(year),
                               total = last(count)) %>%
                        mutate(sample_perm = ifelse(row_number()==1 & unmp == 0 & temp == 0, yes = 1, no = 0),
                               sample_perm = first(sample_perm)) %>%
                        mutate(sample_temp = ifelse(row_number()==1 & unmp == 0 & temp == 1, yes = 1, no = 0),
                               sample_temp = first(sample_temp)) %>%
                        mutate(sample_unmp = ifelse(row_number()==1 & unmp == 1, yes = 1, no = 0),
                               sample_unmp = first(sample_unmp)) %>%
                        ungroup() %>%
                        filter(total==7) %>%
                        mutate(study_period = min(year)) %>%
                        select(-first,-last)
                
                assign(paste("df", c, y, sep = "_"), df_period)
        }
}

country <- c("UK")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- c(2003)
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 5)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)
                
                #keep each year of 7 year study period
                df_period <- group_by(df_period, pid) %>%
                        mutate(count = row_number(),
                               first = first(year),
                               last = last(year),
                               total = last(count)) %>%
                        mutate(sample_perm = ifelse(row_number()==1 & unmp == 0 & temp == 0, yes = 1, no = 0),
                               sample_perm = first(sample_perm)) %>%
                        mutate(sample_temp = ifelse(row_number()==1 & unmp == 0 & temp == 1, yes = 1, no = 0),
                               sample_temp = first(sample_temp)) %>%
                        mutate(sample_unmp = ifelse(row_number()==1 & unmp == 1, yes = 1, no = 0),
                               sample_unmp = first(sample_unmp)) %>%
                        ungroup() %>%
                        filter(total>=6 & first == min(year) & last == max(year)) %>%
                        mutate(study_period = min(year)) %>%
                        select(-first,-last)
                
                assign(paste("df", c, y, sep = "_"), df_period)
        }
}

country <- c("UK")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- c(seq(2004,2008,1))
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 6)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)
                
                #keep each year of 7 year study period
                df_period <- group_by(df_period, pid) %>%
                        mutate(count = row_number(),
                               first = first(year),
                               last = last(year),
                               total = last(count)) %>%
                        mutate(sample_perm = ifelse(row_number()==1 & unmp == 0 & temp == 0, yes = 1, no = 0),
                               sample_perm = first(sample_perm)) %>%
                        mutate(sample_temp = ifelse(row_number()==1 & unmp == 0 & temp == 1, yes = 1, no = 0),
                               sample_temp = first(sample_temp)) %>%
                        mutate(sample_unmp = ifelse(row_number()==1 & unmp == 1, yes = 1, no = 0),
                               sample_unmp = first(sample_unmp)) %>%
                        ungroup() %>%
                        filter(total>=6 & first == min(year) & last == max(year)) %>%
                        mutate(study_period = min(year)) %>%
                        select(-first,-last)

                assign(paste("df", c, y, sep = "_"), df_period)
        }
}

country <- c("AU","CH","DE","JP","KO","NE-LISS")
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

saveRDS(df_sample_2, file = paste0(data_files, "df_sample.rds"))

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

