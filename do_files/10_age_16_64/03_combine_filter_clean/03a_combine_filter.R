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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)
library(beepr)
library(data.table)

options(scipen = 999) # disable scientific notation

# Load data -----

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "OECD/oecd_exchange_rate_2010.csv"), sep = ";")

# Unemployment rate - world bank
df_unmp_rate <- read.csv(paste0(support_files, "world_bank/world_bank_unmp.csv"), sep = ",")

# Country data
df_au <- readRDS(paste0(data_files, "AU/au_sample_v02.rds")) # all non permanent jobs are temporary (except casual)
df_ch <- readRDS(paste0(data_files, "CH/ch_sample.rds")) 
df_de <- readRDS(paste0(data_files, "DE/de_sample.rds")) 
df_uk <- readRDS(paste0(data_files, "UK/uk_sample_v01.rds")) # all non permanent jobs are temporary
df_jp <- readRDS(paste0(data_files, "JP/jp_sample.rds")) 
df_ko <- readRDS(paste0(data_files, "KO/ko_sample.rds")) 
df_ne_liss <- readRDS(paste0(data_files, "NE/LISS/ne_sample.rds")) 
df_ne_lsp <- readRDS(paste0(data_files, "NE/LSP/ne_sample.rds")) 
df_it <- readRDS(paste0(data_files, "IT/it_sample.rds")) 

df_sample_0 <- rbind(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne_liss,df_ne_lsp,df_it)

rm(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne_liss,df_it,df_ne_lsp)

step_0 <- df_sample_0 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_0 <- data.table(step_0, key = "country,pid")
step_0 <- step_0[, head(.SD, 1), by = key(step_0)]
# step_0 <- step_0[, .I[1], by = key(step_0)] # this also works and is slightly faster
step_0 <- data.table(step_0, key = "country")
step_0 <- step_0[, .(count = .N), by = key(step_0)]
step_0$step <- 0

# Filter ----

df_sample_1 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)

step_1 <- df_sample_1 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_1 <- data.table(step_1, key = "country,pid")
step_1 <- step_1[, head(.SD, 1), by = key(step_1)]
step_1 <- data.table(step_1, key = "country")
step_1 <- step_1[, .(count = .N), by = key(step_1)]
step_1$step <- 1

df_sample_2 <- df_sample_1 %>%
        filter(age >= 16 & age <= 64)

step_2 <- df_sample_2 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_2 <- data.table(step_2, key = "country,pid")
step_2 <- step_2[, head(.SD, 1), by = key(step_2)]
# step_2 <- step_2[, .I[1], by = key(step_2)] # this also works and is slightly faster
step_2 <- data.table(step_2, key = "country")
step_2 <- step_2[, .(count = .N), by = key(step_2)]
step_2$step <- 2

# keep if labor force participant
df_sample_3 <- df_sample_2 %>%
        filter(lfp == 1)

step_3 <- df_sample_3 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_3 <- data.table(step_3, key = "country,pid")
step_3 <- step_3[, head(.SD, 1), by = key(step_3)]
step_3 <- data.table(step_3, key = "country")
step_3 <- step_3[, .(count = .N), by = key(step_3)]
step_3$step <- 3

# keep if unemployed or employed, with contract
with(df_sample_3, table(unmp,emp_status,useNA = "ifany"))

df_sample_4 <- df_sample_3 %>%
        filter(unmp == 1 | (unmp == 0 & !is.na(emp_status)))

with(df_sample_4, table(unmp,emp_status,useNA = "ifany"))

step_4 <- df_sample_4 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_4 <- data.table(step_4, key = "country,pid")
step_4 <- step_4[, head(.SD, 1), by = key(step_4)]
step_4 <- data.table(step_4, key = "country")
step_4 <- step_4[, .(count = .N), by = key(step_4)]
step_4$step <- 4

# keep if unemployed or employed and monthly hours between 40 and 320 (i.e. no less than 10 hours per week and no more than 80 hours per week)

df_sample_4 %>% group_by(unmp,country) %>% summarise(min = min(hours, na.rm = TRUE), max = max(hours, na.rm = TRUE))

df_sample_5 <- df_sample_4 %>%
        filter(unmp == 1 | (unmp == 0 & (hours >= 40 & hours <= 320))) %>%
        filter(unmp == 1 | (unmp == 0 & (hourly_wage > 0)))

df_sample_5 %>% group_by(unmp,country) %>% summarise(min = min(hours, na.rm = TRUE), max = max(hours, na.rm = TRUE))

step_5 <- df_sample_5 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_5 <- data.table(step_5, key = "country,pid")
step_5 <- step_5[, head(.SD, 1), by = key(step_5)]
step_5 <- data.table(step_5, key = "country")
step_5 <- step_5[, .(count = .N), by = key(step_5)]
step_5$step <- 5
# step_5$total <- sum(step_5$count)
step_5

# keep if non missing values on key demographic variables
df_sample_6 <- df_sample_5 %>%
        # filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        arrange(country,pid,year)

step_6 <- df_sample_6 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_6 <- data.table(step_6, key = "country,pid")
step_6 <- step_6[, head(.SD, 1), by = key(step_6)]
step_6 <- data.table(step_6, key = "country")
step_6 <- step_6[, .(count = .N), by = key(step_6)]
step_6$step <- 6

# Clean and merge unemployment rate data ----

df_unmp_rate <- pivot_longer(df_unmp_rate,cols = !year, names_to = "country", values_to = "unemployment_rate")
df_unmp_rate_NE <- df_unmp_rate %>%
        filter(country=="NE")
df_unmp_rate_NE_LISS <- df_unmp_rate_NE %>%
        mutate(country="NE-LISS")
df_unmp_rate_NE_LSP <- df_unmp_rate_NE %>%
        mutate(country="NE-LSP")
df_unmp_rate <- df_unmp_rate %>%
        filter(country!="NE")
df_unmp_rate <- rbind(df_unmp_rate,df_unmp_rate_NE_LISS,df_unmp_rate_NE_LSP)
rm(df_unmp_rate_NE_LISS,df_unmp_rate_NE_LSP,df_unmp_rate_NE)

df_sample_7 <- merge(df_sample_6,df_unmp_rate, by = c("country","year")) %>%
        arrange(country,pid,year)

rm(df_unmp_rate)

# Calculate wage distribution to determine bottom/top 0.005  ----

# merge exchange rate data
df_sample_8 <- merge(df_sample_7,df_exchange, by = c("country"))

# convert wages to U.S. dollars 
df_sample_8 <- df_sample_8 %>%
        filter(unmp == 1 | (unmp==0 & hourly_wage > 0)) %>% # employed and wages are greater than 0
        mutate(wages_dollars = wages/exchange,
               hourly_wage_dollars = hourly_wage/exchange)

df_wages_1 <- df_sample_8 %>%
        filter(unmp==0 & hourly_wage > 0) %>% # employed and wages are greater than 0
        group_by(country,year) %>%
        summarise(bot_1 = quantile(hourly_wage, 0.005, na.rm = TRUE), 
                  top_1 = quantile(hourly_wage, 0.995, na.rm = TRUE)) %>%
        ungroup()

# merge top/bot wage cutoffs
df_sample_9 <- merge(df_sample_8,df_wages_1,by=c("country","year")) %>%
        arrange(country,pid,year)

df_sample_9 <- df_sample_9 %>%
        mutate(hourly_wage = ifelse(unmp==0 & (hourly_wage < bot_1 | hourly_wage > top_1), yes = NA, no = hourly_wage))

# keep if unemployed or monthly wages not missing
df_sample_10 <- df_sample_9 %>%
        filter(unmp == 1 | (unmp==0 & !is.na(hourly_wage)))

df_wages_2 <- df_sample_10 %>%
        filter(unmp==0 & hourly_wage > 0) %>% # employed and wages are greater than 0
        group_by(country,year) %>%
        summarise(bot = min(wages_dollars), 
                  top = max(wages_dollars)) %>%
        ungroup()

table(df_sample_9$country)
table(df_sample_10$country)

df_test_10 <- df_sample_10 %>%
        filter(unmp==0)

describe(df_test_10$hourly_wage_dollars)
describe(df_test_10$hours)

step_7 <- df_sample_10 %>%
        select(country,pid) %>%
        mutate(total = sum(n())) 
step_7 <- data.table(step_7, key = "country,pid")
step_7 <- step_7[, head(.SD, 1), by = key(step_7)]
step_7 <- data.table(step_7, key = "country")
step_7 <- step_7[, .(count = .N), by = key(step_7)]
step_7$step <- 7
step_6
step_7

# Save data ----

saveRDS(df_sample_10, file = paste0(data_files, "age_16_64/03a_df_sample.rds"))

df_filter_steps <- rbind(step_0,step_1,step_2,step_3,step_4,step_5,step_6,step_7)
saveRDS(df_filter_steps, file = paste0(data_files, "age_16_64/3a_df_filter_steps.rds"))

beep()
