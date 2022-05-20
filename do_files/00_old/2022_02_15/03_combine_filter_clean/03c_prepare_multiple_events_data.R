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

df_sample_0 <- readRDS(paste0(data_files, "03b_df_sample_cleaned.rds"))

df_original <- df_sample_0 %>%
        # filter(country == "DE") %>%
        select(country, pid, year, year_lag, wages, ln_wages, hourly_wage, ln_hourly_wage, emp_status, unmp, perm, temp, age, edu_cat, male, unemployment_rate)

# Transition indicator ----

df_sample_2 <- df_original %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_t_p_yes = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_p_t_yes = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_p_yes = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_t_yes = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

df_sample_2 %>% filter(pid==20096101) %>% 
        select(country, pid, transseq, year, unmp,temp, perm, matches("event_"), trans, transseq) %>% print(n=15)

# create new data set for each transition
df_transition <- df_sample_2 %>%
        select(country,pid,year,trans,transseq,matches("event_")) %>%
        filter(trans==1) %>%
        group_by(country,pid,transseq) %>%
        filter(row_number()==1) %>%
        mutate(eventtime=0) %>%
        ungroup()

df_transition %>% filter(pid==20096101)

# keep data set for individuals without any transitions
df_transition_non <- df_sample_2 %>%
        group_by(country,pid) %>%
        mutate(transseq=max(transseq)) %>%
        ungroup() %>%
        filter(transseq==0) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-trans,-matches("event_"))

# keep data set with information on employment status, wages, and other IVs for merging into df_transition
df_transition_data <- df_sample_2 %>%
        select(-trans,-transseq,-matches("event_"))

df_transition_data %>% filter(pid==20096101)

# generate sequence indicator: 2 years before transition and 3 years after transition 
df_sample_events <- data.frame()
event <- c(-2,-1,1,2,3,4)
for (e in event) {
        df_test <- df_transition %>%
                mutate(eventtime=e)
        df_sample_events <- rbind(df_sample_events,df_test)
        rm(df_test)
}                

df_sample_events %>% filter(pid==20096101)

df_transition_events <- rbind(df_transition,df_sample_events) %>%
        arrange(country,pid,transseq,eventtime) %>%
        mutate(year = year+eventtime) %>%
        arrange(country,pid,transseq,year) %>%
        select(-trans)

df_transition_events %>% filter(pid==20096101) %>% print(n=20)

# merge new data with old data
df_multiple_events <- merge(df_transition_events,df_transition_data) %>%
        arrange(country,pid,transseq,year) %>%
        mutate(pidseq=pid*100+transseq) # new identifier


df_multiple_events %>% filter(pid==20096101) %>% 
        select(country, pid, transseq, eventtime, year, unmp,temp, perm, matches("event_"), transseq)

df_multiple_events <-df_multiple_events %>%
        select(-eventtime)

df_multiple_events <- bind_rows(df_multiple_events,df_transition_non)

df_multiple_events %>% filter(pid==20096101) %>% 
        select(country, pid, transseq, year, unmp,temp, perm, matches("event_"), transseq)

# rm(list=ls(pattern="df_sample"))
# rm(list=ls(pattern="df_transition"))

# Event 1 - temp into perm ----

df_sample_t_p <- suppressWarnings(df_multiple_events %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_t_p_yes = ifelse(!is.na(event_t_p_yes) == 1, yes = 1, no = 0),
                                                 event_t_p = ifelse(event_t_p_yes == 1 & perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = 0),
                                                 event_t_p_time = ifelse(event_t_p_time < -2, yes = NA,
                                                                         ifelse(event_t_p_time > 4, yes = NA, no = event_t_p_time)),
                                                 event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

table(df_sample_t_p$event_t_p, useNA = "ifany")

df_sample_t_p %>% 
        filter(pid==20096101) %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, matches("event_t_p")) %>% print(n=40)


# Event 2 - unmp into perm ----

df_sample_u_p <- suppressWarnings(df_sample_t_p %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_u_p_yes = ifelse(!is.na(event_u_p_yes) == 1, yes = 1, no = 0),
                                                 event_u_p = ifelse(event_u_p_yes == 1 & perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_p_year = ifelse(event_u_p == 1, yes = year, no = NA),
                                                 event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                 event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = 0),
                                                 event_u_p_time = ifelse(event_u_p_time < -2, yes = NA,
                                                                         ifelse(event_u_p_time > 4, yes = NA, no = event_u_p_time)),
                                                 event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

df_sample_u_p %>% filter(event_u_p_yes==1) %>% select(pid,year,unmp,perm,matches("event_u_p"))

# Event 3 - unmp into temp ----

df_sample_u_t <- suppressWarnings(df_sample_u_p %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_u_t_yes = ifelse(!is.na(event_u_t_yes) == 1, yes = 1, no = 0),
                                                 event_u_t = ifelse(event_u_t_yes == 1 & temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_t_year = ifelse(event_u_t == 1, yes = year, no = NA),
                                                 event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                 event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = 0),
                                                 event_u_t_time = ifelse(event_u_t_time < -2, yes = NA,
                                                                         ifelse(event_u_t_time > 4, yes = NA, no = event_u_t_time)),
                                                 event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

# Event 4 - perm into temp ----

df_sample_p_t <- suppressWarnings(df_sample_u_t %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_p_t_yes = ifelse(!is.na(event_p_t_yes) == 1, yes = 1, no = 0),
                                                 event_p_t = ifelse(event_p_t_yes == 1 & temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_p_t_yes = max(event_p_t),
                                                 event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                 event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                 event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = 0),
                                                 event_p_t_time = ifelse(event_p_t_time < -2, yes = NA,
                                                                         ifelse(event_p_t_time > 4, yes = NA, no = event_p_t_time)),
                                                 event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

# Save data ----

df_events_all <- df_sample_p_t %>%
        select(country,pid,year,pidseq,transseq,everything())

rm(list=ls(pattern="df_sample"))

saveRDS(df_events_all, file = paste0(data_files, "03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

beep()
