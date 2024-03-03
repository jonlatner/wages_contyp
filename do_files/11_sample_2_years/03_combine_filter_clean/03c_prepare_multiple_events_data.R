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
data_files_new = "data_files/sample_2_years/"

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

# create new data set for each transition
df_transition <- df_sample_2 %>%
        select(country,pid,year,trans,transseq,matches("event_")) %>%
        filter(trans==1) %>%
        group_by(country,pid,transseq) %>%
        filter(row_number()==1) %>%
        mutate(eventtime=0,
               eventyear=year) %>%
        ungroup()

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

# generate sequence indicator: 2 years before transition and 3 years after transition 
df_sample_events <- data.frame()
event <- c(-4,-3,-2,-1,1,2,3,4,5,6)
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
df_multiple_events <- merge(df_transition_events,df_transition_data) %>%
        arrange(country,pid,transseq,year) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-eventtime)

df_multiple_events <- bind_rows(df_multiple_events,df_transition_non)

rm(list=ls(pattern="df_sample"))
rm(list=ls(pattern="df_transition"))

# Event 1 - temp into perm ----

# Definition of event: Observable in 3 time periods
# 1 period pre event (t_0)
# 1 period at event (t_1)

# Event 1 - temp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_t_p = ifelse(event_t_p_yes == 1 & perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_t_p_year = ifelse(event_t_p == 1, yes = eventyear, no = NA),
                                                      event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                      event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = NA),
                                                      event_t_p_time = ifelse(event_t_p_time < -3, yes = -3,
                                                                              ifelse(event_t_p_time >= 5, yes = 5, no = event_t_p_time)),
                                               ) %>%
                                               ungroup()) %>%
        arrange(country, pidseq, year) %>%
        mutate(event_t_p_yes = ifelse(is.na(event_t_p_yes), yes = 0, no = event_t_p_yes),
               event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0), # bring all to positive values
        ) 

# View(select(df_multiple_events, country, pidseq, year, matches("event_t_p"), temp, perm))

# Event 2 - unmp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_p = ifelse(event_u_p_yes == 1 & perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_u_p_year = ifelse(event_u_p == 1, yes = eventyear, no = NA),
                                                      event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                      event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = NA),
                                                      event_u_p_time = ifelse(event_u_p_time < -3, yes = -3,
                                                                              ifelse(event_u_p_time >= 5, yes = 5, no = event_u_p_time)),
                                               ) %>%
                                               ungroup()) %>%
  arrange(country, pidseq, year) %>%
  mutate(event_u_p_yes = ifelse(is.na(event_u_p_yes), yes = 0, no = event_u_p_yes),
         event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 3, no = 0), # bring all to positive values
  )

df_multiple_events %>% select(pid,pidseq,year,unmp,temp,perm,event_u_p,event_u_p_year,event_u_p_yes,event_u_p_yes) %>% filter(pid==101502)

# Event 3 - unmp into temp ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_t = ifelse(event_u_t_yes == 1 & temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                      event_u_t_year = ifelse(event_u_t == 1, yes = eventyear, no = NA),
                                                      event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                      event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = NA),
                                                      event_u_t_time = ifelse(event_u_t_time < -3, yes = -3,
                                                                              ifelse(event_u_t_time >= 5, yes = 5, no = event_u_t_time)),
                                               ) %>%
                                               ungroup()) %>%
  arrange(country, pidseq, year) %>%
        mutate(event_u_t_yes = ifelse(is.na(event_u_t_yes), yes = 0, no = event_u_t_yes),
               event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0), # bring all to positive values
        )

# Event 4 - perm into temp ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_p_t = ifelse(event_p_t_yes == 1 & temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                      event_p_t_year = ifelse(event_p_t == 1, yes = eventyear, no = NA),
                                                      event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                      event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = NA),
                                                      event_p_t_time = ifelse(event_p_t_time < -3, yes = -3,
                                                                              ifelse(event_p_t_time >= 5, yes = 5, no = event_p_t_time)),
                                               ) %>%
                                               ungroup()) %>%
  arrange(country, pidseq, year) %>%
        mutate(event_p_t_yes = ifelse(is.na(event_p_t_yes), yes = 0, no = event_p_t_yes),
               event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0), # bring all to positive values
        )

# Save data sets ----

df_events_all <- df_multiple_events %>%
        select(country,pid,pidseq,year,everything()) 

df_events_all <- df_events_all %>%
        group_by(country, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0)) %>%
        ungroup() %>%
        filter(max>1)

rm(list=ls(pattern="df_sample"))

saveRDS(df_events_all, file = paste0(data_files_new, "03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

beep()
