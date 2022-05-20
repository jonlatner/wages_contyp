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
data_files_unbalanced = "projects/mobility/data_files/unbalanced/"
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

with(df_sample_0,table(country))
with(df_sample_1,table(country))

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
# for(c in country) {
#         rm(list=ls(pattern=paste0("df_",c)))
# }

# Save data sets ----

saveRDS(df_sample_2, file = paste0(data_files_unbalanced, "df_sample.rds"))

with(df_sample_2,table(study_period,country))

