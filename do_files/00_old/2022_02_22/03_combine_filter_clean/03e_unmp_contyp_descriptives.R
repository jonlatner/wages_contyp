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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

# cleaned
df_cleaned_01 <- readRDS(paste0(data_files, "03b_df_sample_cleaned.rds"))
df_cleaned_01 <- df_cleaned_01 %>%
        filter(country == "IT")

# sample
df_sample <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))
df_sample <- df_sample %>%
        filter(country == "IT")

# clean sample data -----

# Unmp to temp
df_sample_u_t <- df_sample %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_t")) %>%
        filter((event_u_t_yes == 1 & event_u_t_drop_01 == 0)) %>%
        filter((event_u_t_yes == 1 & event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0)) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2) %>%
        select(-number,-max)

df_sample_u_t <- df_sample_u_t %>%
        group_by(country,pid) %>%
        summarise(event_u_t = sum(event_u_t, na.rm = TRUE),
        ) %>%
        ungroup()

# Unmp to perm
df_sample_u_p <- df_sample %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_p")) %>%
        filter((event_u_p_yes == 1 & event_u_p_drop_01 == 0)) %>%
        filter((event_u_p_yes == 1 & event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0)) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2) %>%
        select(-number,-max)

df_sample_u_p <- df_sample_u_p %>%
        group_by(country,pid) %>%
        summarise(event_u_p = sum(event_u_p, na.rm = TRUE),
        ) %>%
        ungroup()

with(df_sample_u_t,table(country,event_u_t))
with(df_sample_u_p,table(country,event_u_p))

df_events <- merge(df_sample_u_t,df_sample_u_p,all = TRUE)

with(df_events,table(event_u_p,event_u_t,useNA = "ifany"))


# load cleaned data -----

df_cleaned_01 <- df_cleaned_01 %>%
        filter(country == "IT") %>%
        select(country, pid, age, year, year_lag, unmp, perm, temp)

# Transition indicator
df_cleaned_02 <- df_cleaned_01 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

# create new data set for each transition
df_transition <- df_cleaned_02 %>%
        select(country,pid,year,trans,transseq,matches("event_")) %>%
        filter(trans==1) %>%
        group_by(country,pid,transseq) %>%
        filter(row_number()==1) %>%
        mutate(eventtime=0) %>%
        ungroup()

# keep data set with information on yearly employment status
df_sample_data <- df_cleaned_02 %>%
        select(-trans,-transseq,-matches("event_"))

# generate sequence indicator: 2 years before transition and 4 years after transition 
df_sample_events <- data.frame()
event <- c(-2,-1,1,2,3,4)
for (e in event) {
        df_test <- df_transition %>%
                mutate(eventtime=e)
        df_sample_events <- rbind(df_sample_events,df_test)
        rm(df_test)
}                

df_transition_events <- rbind(df_transition,df_sample_events) %>%
        arrange(country,pid,transseq,eventtime) %>%
        mutate(year = year+eventtime) %>%
        arrange(country,pid,transseq,year) %>%
        select(-trans)


# merge new data with old data
df_multiple_events_01 <- merge(df_transition_events,df_sample_data,all.x = TRUE) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(country,pidseq,pid,transseq,year,everything()) %>%
        arrange(country,pid,transseq,year)

rm(list=ls(pattern="df_sample"))
rm(list=ls(pattern="df_transition"))

df_multiple_events_02 <- df_multiple_events_01 %>%
        filter(!is.na(year_lag)) %>%
        arrange(country, pidseq, year) %>%
        group_by(country, pidseq) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
               exit_unmp_yes = max(event_exit_unmp, na.rm = TRUE),
               unmp_ever = max(unmp, na.rm = TRUE),
               count = row_number(),
               max = max(count, na.rm = TRUE),
               event_year = ifelse(event_exit_unmp == 1, yes = year, no = NA),
               event_year = min(event_year, na.rm=TRUE),
               event_time = ifelse(exit_unmp_yes == 1, yes = year - event_year, no = NA),
               event_drop_01 = ifelse(exit_unmp_yes==1 & year > event_year, yes = 0, no = 1), # keep if observable after treatment
               event_drop_01 = last(event_drop_01),
               event_drop_02 = ifelse(exit_unmp_yes==1 & event_time > 0 & unmp == 1, yes = 1, no = 0), # keep if experience treatment and employed after treatment event
               event_drop_02 = max(event_drop_02),
               unique = ifelse(row_number()==1, yes = 1, no = 0)
        ) %>%
        ungroup()

df_multiple_events_02 %>% select(country,pid,year,year_lag,unmp,temp,perm,matches("event"))

# Clean cleaned data ----

# Event - unmp into emp 

df_cleaned_02 <- df_cleaned_01 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
               exit_unmp_yes = max(event_exit_unmp),
               unmp_ever = max(unmp),
        ) %>%
        filter(unmp_ever==1) %>%
        mutate(count = row_number(),
               max = max(count),
               event_year = ifelse(event_exit_unmp == 1, yes = year, no = NA),
               event_year = min(event_year, na.rm=TRUE),
               event_time = ifelse(exit_unmp_yes == 1, yes = year - event_year, no = NA),
               event_drop_01 = ifelse(exit_unmp_yes==1 & year > event_year, yes = 0, no = 1), # keep if observable after treatment
               event_drop_01 = last(event_drop_01),
               event_drop_02 = ifelse(exit_unmp_yes==1 & event_time > 0 & unmp == 1, yes = 1, no = 0), # keep if experience treatment and employed after treatment event
               event_drop_02 = max(event_drop_02),
               unique = ifelse(row_number()==1, yes = 1, no = 0)
        ) %>%
        ungroup()

beep()

# Total possible events
t_02 <- with(subset(df_cleaned_02,unique == 1),table(country,unmp_ever,useNA = "ifany"))
t_02


# must experience unemployment
# must be observable at least 2 to experience post employment
df_cleaned_03 <- df_cleaned_02 %>%
        filter(unmp_ever == 1) %>%
        filter(max > 1)

t_03 <- with(subset(df_cleaned_03,unique == 1),table(country,unmp_ever,useNA = "ifany"))
t_03

# must exit unemployment
df_cleaned_04 <- df_cleaned_03 %>%
        filter(exit_unmp_yes == 1) %>%
        group_by(pid) %>%
        mutate(max_age = max(age)) %>%
        ungroup()

t_04 <- with(subset(df_cleaned_04,unique == 1),table(country,unmp_ever,useNA = "ifany"))
t_04

with(subset(df_cleaned_04,unique == 1),table(max_age,useNA = "ifany"))

# must be observable after transition into employment
df_cleaned_05 <- df_cleaned_04 %>%
        filter(event_drop_01 == 0)


t_05 <- with(subset(df_cleaned_05,unique == 1),table(country,unmp_ever,useNA = "ifany"))
t_05

# must be observable after transition into employment
df_cleaned_06 <- df_cleaned_05 %>%
        filter(event_drop_02 == 0)

t_06 <- with(subset(df_cleaned_06,unique == 1),table(country,unmp_ever,useNA = "ifany"))
t_06

df_test <- df_cleaned_06 %>%
        filter(unique == 1) %>%
        select(pid, country, unmp, temp, perm, matches("event")) %>%
        mutate(test = 1) 
df_merge <- merge(df_test,df_events,all = TRUE)

t_02 # Total possible events
t_04 # must exit unemployment into employment (i.e. 3 periods)
t_05 # must be observable after transition into employment (i.e. 4 periods)
t_06 # must be employed after transition into employment (i.e. 4 periods)
with(df_events,table(event_u_p,event_u_t,useNA = "ifany"))


df_cleaned_04 %>%
        filter(pid == 6721421) %>%
        select(pid, country, unmp, temp, perm, matches("event"))

