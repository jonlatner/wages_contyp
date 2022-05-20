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

# Save data sets ----

saveRDS(df_sample_1, file = paste0(data_files, "df_sample.rds"))

