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
data_files_unbalanced = "projects/mobility/data_files/"
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
# cases where people are both unemployed and work contract (temp or perm)
with(df_sample_0,table(unmp,temp,useNA = "ifany"))

df_sample_0 <- df_sample_0 %>%
        mutate(test = ifelse(unmp == 1 & !is.na(temp), yes = 1, no = 0)) %>%
        filter(test == 0) %>%
        select(-test)

with(df_sample_0,table(unmp,temp,useNA = "ifany"))

# filter ----
df_sample_0 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)
        
df_sample_1 <- df_sample_0 %>%
        filter(slf!=1) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(age >= 25 & age <= 54) %>%
        mutate(ln_hourly_wage = ifelse(is.infinite(ln_hourly_wage), yes = NA, no = ln_hourly_wage)) %>%
        filter(unmp == 1 | (unmp==0 & !is.na(temp))) %>% # unemployed or employed with a work contract (temp or perm)
        filter(unmp == 1 | (unmp==0 & ln_hourly_wage >= 1)) %>% # unemployed or employed with hourly wages > 1
        select(-lfp,-slf,-occ,-wages,-hours) %>%
        arrange(country,pid,year)

with(df_sample_1,table(country))

# df_sample_1 %>% 
#         group_by(year,country,unmp) %>%
#         summarise(mean=mean(ln_hourly_wage)) %>%
#         ungroup()

#Study period ----

country <- c("AU","CH","DE","JP","KO","NE-LISS","IT", "NE-LSP","UK")
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- c(seq(2000,2012,1))
        for(y in year) {
                print(y)
                df_period <- filter(df_country, year >= y & year <= y + 6)
                df_period <- unique(df_period) %>%
                        arrange(pid,year)

                #keep if present in first year of 7 year observation period
                df_period <- group_by(df_period, pid) %>%
                        mutate(first = first(year)) %>%
                        filter(first == y) %>%
                        mutate(study_period = min(year))

assign(paste("df", c, y, sep = "_"), df_period)
}
}

#Append data sets ----

df_sample_2 = data.frame()
country <- sort(unique(df_sample_1$country))
for(c in country) {
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        print(c)
        year <- c(seq(2000,2012,1))
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

saveRDS(df_sample_2, file = paste0(data_files_unbalanced, "df_sample_unbalanced.rds"))

with(df_sample_2,table(study_period,country))

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
# 
