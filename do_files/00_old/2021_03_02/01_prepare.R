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

options(scipen = 999) # disable scientific notation

# load data -----

df_au <- readRDS(paste0(data_files, "AU/au_sample.rds")) 
df_ch <- readRDS(paste0(data_files, "CH/ch_sample.rds")) 
df_de <- readRDS(paste0(data_files, "DE/de_sample.rds")) 
df_uk <- readRDS(paste0(data_files, "UK/uk_sample.rds")) 
df_jp <- readRDS(paste0(data_files, "JP/jp_sample.rds")) 
df_ko <- readRDS(paste0(data_files, "KO/ko_sample.rds")) 
df_ne <- readRDS(paste0(data_files, "NE/ne_sample.rds")) 
df_it <- readRDS(paste0(data_files, "IT/it_sample.rds")) 

df_sample_0 <- rbind(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne,df_it)

rm(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne,df_it)

# clean/filter ----

df_sample_1 <- df_sample_0 %>%
        filter(year>=2000) %>%
        filter(lfp == 1) %>%
        filter(slf!=1) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(!is.na(unmp)) %>%
        filter(!is.na(temp)) %>%
        filter(age >= 25 & age <= 54) %>%
        select(-lfp,-slf)
summary(df_sample_1)

filter(ln_hourly_wage>=1) %>%
        filter(prestige>=0) %>%
        
df_it <- df_it %>%
        filter(!is.na(contyp),
               !is.na(hours_total),
               !is.na(hours_overtime),
               !is.na(lfs)) %>%
        arrange(hid, pid, year)
summary(df_it)


%>%
        select(pid, year, age, age_cat, edu_cat, male, unmp, temp, ln_hourly_wage, prestige)


# prepare data for spell analysis ----

df_sample_2 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(start = ifelse(year - lag(year) == 1, yes = NA, no = 1),
               start = ifelse(row_number()==1, yes = 1, no = start),
               spell = cumsum(ifelse(is.na(start), 0, start))) %>%
        ungroup() %>%
        group_by(country, pid, spell) %>%
        mutate(count=row_number(),
               max = max(count)) %>%
        ungroup() 

# Temporary employment spell
df_sample_3 <- df_sample_2 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid, spell) %>%
        mutate(start_temp = ifelse(temp==1 & lag(temp,1)!=1, yes = year, no = 0),
               start_temp = ifelse(temp == 1 & row_number()==1, yes = year, no = start_temp),
               end_temp = ifelse(lag(temp,1)==1 & temp!=1, yes = year-1, no = NA)) %>%
        ungroup() %>%
        group_by(country, pid) %>%
        mutate(spell_temp = cumsum(ifelse(start_temp>0, yes = 1, no = 0))) %>%
        ungroup() %>%
        mutate(start_temp = ifelse(start_temp == 0, yes = NA, no = start_temp),
               spell_temp = ifelse(spell_temp == 0, yes = NA, no = spell_temp),
               end_temp = ifelse(end_temp == 0, yes = NA, no = end_temp))

# Post temporary employment spell
df_sample_4 <- df_sample_3 %>%
        group_by(country, pid,spell,spell_temp) %>%
        arrange(country,pid,year) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE),
               post_temp = year - end_temp,
               post_temp = ifelse(is.na(post_temp), yes = 0, no = post_temp)
        ) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE)) %>%
        ungroup() %>%
        arrange(pid,year)

# Post first temporary employment spell
df_sample_5 <- df_sample_4 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(end_temp_first = end_temp) %>%
        arrange(pid,end_temp) %>%
        mutate(end_temp_first = first(end_temp)) %>%
        arrange(pid,year) %>%
        mutate(post_temp_first = cumsum(post_temp),
               post_temp_first = ifelse(post_temp_first>1, yes = 1, no = post_temp_first),
               post_temp_first = ifelse(post_temp_first==1, yes = year-end_temp_first, no = NA),
               post_temp_first = ifelse(is.na(post_temp_first), yes = 0, no = post_temp_first)) %>%
        ungroup()

t <- with(subset(df_sample_5, post_temp>0), prop.table(table(post_temp)))
t <- data.frame(t)
t$cumsum <- cumsum(t$Freq)

# Top code
df_sample_6 <- df_sample_5
# df_sample_6 <- df_sample_5 %>%
#         mutate(post_temp_first = ifelse(post_temp_first > 7, yes = 7, no = post_temp_first)) %>%
#         mutate(post_temp = ifelse(post_temp > 7, yes = 7, no = post_temp))

# Clean
df_sample_7 <- df_sample_6 %>%
        # filter(max >= 5) %>%
        arrange(country, pid, year) %>%
        select(country,pid,year,unmp,temp,ln_hourly_wage,prestige,edu,edu_cat,age,age_cat,male,post_temp,post_temp_first,max)

df_sample_7 <- df_sample_7 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(edu_cat = last(edu_cat),
               age_cat = first(age_cat)) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_7, file = paste0(data_files, "df_sample_clean.rds"))
