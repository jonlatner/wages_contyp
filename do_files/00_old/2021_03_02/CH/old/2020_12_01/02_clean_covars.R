# Top commands ----
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
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "SECCOPA/projects/mobility/data_files/CH/"
raw_data = "SECCOPA/CH_FORS/support_files/"

# PACKAGES
# install.packages("dplyr")

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ch <- readRDS(paste0(data_files,"covars.rds"))

# inflation data 
df_inflation <- read.csv(paste0(raw_data, "ch_inflation.csv"), sep = ";", skip = 1)
colnames(df_inflation) <- c("year", "cpi", "extra")
df_inflation$year <- substr(df_inflation$year,1,4)
df_inflation <- df_inflation %>%
        select(year, cpi) %>%
        mutate_all(as.numeric)

df_ch <- merge(df_ch,df_inflation)
rm(df_inflation)

# Clean ------------------------

df_ch <- df_ch %>%
        rename(occ = is4maj,
               prestige = tr1maj,
               edu = isced,
               pid=idpers) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_ch$lfp <- recode(df_ch$wstat, "lo:0 = NA; 3=0; 1:2=1")

# employment status
df_ch$unmp <- recode(df_ch$wstat, "lo:0 = NA; 1=0; 2=1; else=NA")

# contract type
df_ch$temp  <- recode(df_ch$pw36, "lo:0 = NA; 1=1; 2=0")
# with(df_ch,table(pw37,pw36,useNA = "ifany"))

# Self-employed
df_ch$slf <- recode(df_ch$pw29, "3=1; else=0")

# Occupation (ISCO 88)
df_ch <- df_ch %>%
        mutate(occ_grp = ifelse(occ>=1100 & occ<9999, yes = as.integer(occ/1000), no = NA))

# Prestige (Treiman)
df_ch$prestige <- recode(df_ch$prestige, "lo:0 = NA")

# Hours (weekly)
df_ch$hours <- recode(df_ch$hours, "lo:0 = NA; 40:hi=40")

# Wages (annual)
df_ch$wages <- recode(df_ch$wages, "lo:0 = NA")
df_ch <- df_ch %>%
        mutate(wages = wages/cpi*100,
               hourly_wage = wages/(hours*4))

# Recode: if you are unemployed, then you don't have a contract type or prestige score
df_ch <- df_ch %>%
        mutate(temp = ifelse(unmp == 1, yes = 0, no = temp),
               prestige = ifelse(unmp == 1, yes = 0, no = prestige),
               hourly_wage = ifelse(unmp==1, yes = 1, no = hourly_wage),
               ln_hourly_wage = log(hourly_wage))

# demographics -------------------------
# education (ISCED)
df_ch$edu_cat <- recode(df_ch$edu, "lo:-1 = NA; c(0,10,20)=1; 30:33=2; 40:hi=3")

# sex
df_ch$male <- recode(df_ch$sex, "2 = 0")

df_ch <- df_ch %>%
        arrange(pid,year) %>%
        group_by(pid) %>%
        mutate(male = first(male)) %>%
        ungroup()

# Age
df_ch <- df_ch %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_sample_1 <- df_ch %>%
        filter(year>=2000) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(lfp == 1) %>%
        filter(!is.na(unmp)) %>%
        filter(slf!=1) %>%
        filter(!is.na(temp)) %>%
        filter((unmp==1 & is.na(ln_hourly_wage)) | (unmp == 0 & ln_hourly_wage>1)) %>%
        filter(age >= 25 & age <= 54)
summary(df_sample_1)

df_sample_2 <- df_sample_1 %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(start = ifelse(year - lag(year) == 1, yes = NA, no = 1),
               start = ifelse(row_number()==1, yes = 1, no = start),
               spell = cumsum(ifelse(is.na(start), 0, start))) %>%
        ungroup() %>%
        group_by(pid, spell) %>%
        mutate(count=row_number(),
               max = max(count)) %>%
        ungroup() %>%
        filter(max >= 2)

# Temporary employment spell
df_sample_3 <- df_sample_2 %>%
        arrange(pid, year) %>%
        group_by(pid, spell) %>%
        mutate(start_temp = ifelse(temp==1 & lag(temp,1)!=1, yes = year, no = 0),
               start_temp = ifelse(temp == 1 & row_number()==1, yes = year, no = start_temp),
               end_temp = ifelse(lag(temp,1)==1 & temp!=1, yes = year-1, no = NA)) %>%
        ungroup() %>%
        group_by(pid) %>%
        mutate(spell_temp = cumsum(ifelse(start_temp>0, yes = 1, no = 0))) %>%
        ungroup() %>%
        mutate(start_temp = ifelse(start_temp == 0, yes = NA, no = start_temp),
               spell_temp = ifelse(spell_temp == 0, yes = NA, no = spell_temp),
               end_temp = ifelse(end_temp == 0, yes = NA, no = end_temp))

# Post temporary employment spell
df_sample_4 <- df_sample_3 %>%
        group_by(pid,spell,spell_temp) %>%
        arrange(pid,year) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE),
               post_temp = year - end_temp,
               post_temp = ifelse(is.na(post_temp), yes = 0, no = post_temp)
        ) %>%
        mutate(end_temp = na.locf(end_temp,na.rm = FALSE)) %>%
        ungroup() %>%
        arrange(pid,year)

# Post first temporary employment spell
df_sample_5 <- df_sample_4 %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(end_temp_first = end_temp) %>%
        arrange(pid,end_temp) %>%
        mutate(end_temp_first = first(end_temp)) %>%
        arrange(pid,year) %>%
        mutate(post_temp_first = cumsum(post_temp),
               post_temp_first = ifelse(post_temp_first>1, yes = 1, no = post_temp_first),
               post_temp_first = ifelse(post_temp_first==1, yes = year-end_temp_first, no = NA),
               post_temp_first = ifelse(is.na(post_temp_first), yes = 0, no = post_temp_first)) %>%
        ungroup()

# with(subset(df_sample_5, post_temp>0), prop.table(table(post_temp)))

# Top code
df_sample_6 <- df_sample_5 %>%
        mutate(post_temp_first = ifelse(post_temp_first > 5, yes = 5, no = post_temp_first)) %>%
        mutate(post_temp = ifelse(post_temp > 5, yes = 5, no = post_temp))

# Clean
df_sample_7 <- df_sample_6 %>%
        arrange(pid, year) %>%
        select(pid,year,unmp,temp,ln_hourly_wage,prestige,edu,edu_cat,age,age_cat,male,post_temp,post_temp_first)

df_sample_7 <- df_sample_7 %>%
        arrange(pid,year) %>%
        group_by(pid) %>%
        mutate(edu_cat = last(edu_cat),
               age_cat = first(age_cat)) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_7, file = paste0(data_files, "ch_fors.rds"))
