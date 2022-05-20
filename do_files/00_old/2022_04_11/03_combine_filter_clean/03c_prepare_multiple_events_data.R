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
        # filter(country == "IT") %>%
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
        mutate(eventtime=0) %>%
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
# 1 period post event (t_1, ..., t_4) and employed
# Post event:
# must be employed at least 1 period < 5 years after treatment
# Event 1 - temp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_t_p_yes = max(event_t_p),
                                                      event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                      event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                      event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = NA),
                                                      event_t_p_time = ifelse(event_t_p_time < -3, yes = -3,
                                                                              ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                               ) %>%
                                               ungroup())

df_event_t_p <- df_multiple_events %>%
        filter(event_t_p_yes == 1 & event_t_p_time > 0 & unmp == 0) %>% # must experience event, must be observable after event, and must be employed
        select(country, pidseq, year, perm, temp, unmp, event_t_p, event_t_p_year, event_t_p_time) %>%
        group_by(country, pidseq) %>%
        mutate(difference = year - event_t_p_year,
               keep = ifelse(year > event_t_p_year & difference < 5, yes = 1, no = 0),  # identify if employed observations < 5 years after treatment
               max = max(keep)) %>%
        filter(max>0) %>% # keep if employed at least 1 period within 5 years after event
        slice(1) %>%
        ungroup() %>%
        mutate(event_t_p_yes_final = 1) %>%
        select(country, pidseq, event_t_p_yes_final)

with(df_event_t_p,table(country,event_t_p_yes_final,useNA = "ifany"))

df_multiple_events <- merge(df_multiple_events,df_event_t_p,all.x =TRUE) %>%
        arrange(country, pidseq, year) %>%
        mutate(event_t_p_yes_final = ifelse(is.na(event_t_p_yes_final), yes = 0, no = event_t_p_yes_final),
               event_t_p_time_pos = ifelse(event_t_p_yes_final == 1, yes = event_t_p_time + 3, no = 0), # bring all to positive values
               event_t_p_step = event_t_p, # transform temp dummy into step function if there is an event, as defined above
               test = max(event_t_p_time_pos, na.rm = TRUE),
               event_t_p_step = ifelse(test>0, yes = event_t_p_step, no = 0),
               event_t_p_step_cum = cumsum(event_t_p_step),
               event_t_p_step = ifelse(test>0, yes = temp, no = 0),
        ) 

# Event 2 - unmp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_u_p_yes = max(event_u_p),
                                                      event_u_p_year = ifelse(event_u_p == 1, yes = year, no = NA),
                                                      event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                      event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = NA),
                                                      event_u_p_time = ifelse(event_u_p_time < -3, yes = -3,
                                                                              ifelse(event_u_p_time > 4, yes = 4, no = event_u_p_time)),
                                               ) %>%
                                               ungroup())

df_event_u_p <- df_multiple_events %>%
        filter(event_u_p_yes == 1 & event_u_p_time > 0 & unmp == 0) %>% # must experience event, must be observable after event, and must be employed
        select(country, pidseq, year, perm, temp, unmp, event_u_p, event_u_p_year, event_u_p_time) %>%
        group_by(country, pidseq) %>%
        mutate(difference = year - event_u_p_year,
               keep = ifelse(year > event_u_p_year & difference < 5, yes = 1, no = 0),  # identify if employed observations < 5 years after treatment
               max = max(keep)) %>%
        filter(max>0) %>% # keep if employed at least 1 period within 5 years after event
        slice(1) %>%
        ungroup() %>%
        mutate(event_u_p_yes_final = 1) %>%
        select(country, pidseq, event_u_p_yes_final)

with(df_event_u_p,table(country,event_u_p_yes_final,useNA = "ifany"))

df_multiple_events <- merge(df_multiple_events,df_event_u_p,all.x =TRUE) %>%
        arrange(country, pidseq, year) %>%
        mutate(event_u_p_yes_final = ifelse(is.na(event_u_p_yes_final), yes = 0, no = event_u_p_yes_final),
               event_u_p_time_pos = ifelse(event_u_p_yes_final == 1, yes = event_u_p_time + 3, no = 0), # bring all to positive values
               event_u_p_step = event_u_p, # transform event dummy into step function
               test = max(event_u_p_time_pos, na.rm = TRUE),
               event_u_p_step = ifelse(test>0, yes = event_u_p_step, no = 0),
               event_u_p_step_cum = cumsum(event_u_p_step),
        )

# Event 3 - unmp into temp ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                      event_u_t_yes = max(event_u_t),
                                                      event_u_t_year = ifelse(event_u_t == 1, yes = year, no = NA),
                                                      event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                      event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = NA),
                                                      event_u_t_time = ifelse(event_u_t_time < -3, yes = -3,
                                                                              ifelse(event_u_t_time > 4, yes = 4, no = event_u_t_time)),
                                               ) %>%
                                               ungroup())

df_event_u_t <- df_multiple_events %>%
        filter(event_u_t_yes == 1 & event_u_t_time > 0 & unmp == 0) %>% # must experience event, must be observable after event, and must be employed
        select(country, pidseq, year, perm, temp, unmp, event_u_t, event_u_t_year, event_u_t_time) %>%
        group_by(country, pidseq) %>%
        mutate(difference = year - event_u_t_year,
               keep = ifelse(year > event_u_t_year & difference < 5, yes = 1, no = 0),  # identify if employed observations < 5 years after treatment
               max = max(keep)) %>%
        filter(max>0) %>% # keep if employed at least 1 period within 5 years after event
        slice(1) %>%
        ungroup() %>%
        mutate(event_u_t_yes_final = 1) %>%
        select(country, pidseq, event_u_t_yes_final)

with(df_event_u_t,table(country,event_u_t_yes_final,useNA = "ifany"))

df_multiple_events <- merge(df_multiple_events,df_event_u_t,all.x =TRUE) %>%
        arrange(country, pidseq, year) %>%
        mutate(event_u_t_yes_final = ifelse(is.na(event_u_t_yes_final), yes = 0, no = event_u_t_yes_final),
               event_u_t_time_pos = ifelse(event_u_t_yes_final == 1, yes = event_u_t_time + 3, no = 0), # bring all to positive values
               event_u_t_step = event_u_t, # # transform temp dummy into step function if there is an event, as defined above
               test = max(event_u_t_time_pos, na.rm = TRUE),
               event_u_t_step = ifelse(test>0, yes = event_u_t_step, no = 0),
               event_u_t_step_cum = cumsum(event_u_t_step),
        )

# Event 4 - perm into temp ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                      event_p_t_yes = max(event_p_t),
                                                      event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                      event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                      event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = NA),
                                                      event_p_t_time = ifelse(event_p_t_time < -3, yes = -3,
                                                                              ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                               ) %>%
                                               ungroup())


df_event_p_t <- df_multiple_events %>%
        filter(event_p_t_yes == 1 & event_p_t_time > 0 & unmp == 0) %>% # must experience event, must be observable after event, and must be employed
        select(country, pidseq, year, perm, temp, unmp, event_p_t, event_p_t_year, event_p_t_time) %>%
        group_by(country, pidseq) %>%
        mutate(difference = year - event_p_t_year,
               keep = ifelse(year > event_p_t_year & difference < 5, yes = 1, no = 0),  # identify if employed observations < 5 years after treatment
               max = max(keep)) %>%
        filter(max>0) %>% # keep if employed at least 1 period within 5 years after event
        slice(1) %>%
        ungroup() %>%
        mutate(event_p_t_yes_final = 1) %>%
        select(country, pidseq, event_p_t_yes_final)

with(df_event_p_t,table(country,event_p_t_yes_final,useNA = "ifany"))

df_multiple_events <- merge(df_multiple_events,df_event_p_t,all.x =TRUE) %>%
        arrange(country, pidseq, year) %>%
        mutate(event_p_t_yes_final = ifelse(is.na(event_p_t_yes_final), yes = 0, no = event_p_t_yes_final),
               event_p_t_time_pos = ifelse(event_p_t_yes_final == 1, yes = event_p_t_time + 3, no = 0), # bring all to positive values
               event_p_t_step = event_p_t, # transform event dummy into step function
               test = max(event_p_t_time_pos, na.rm = TRUE),
               event_p_t_step = ifelse(test>0, yes = event_p_t_step, no = 0),
               event_p_t_step_cum = cumsum(event_p_t_step),
               event_p_t_step = ifelse(test>0, yes = temp, no = 0),
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
        filter(max>2)

rm(list=ls(pattern="df_sample"))

saveRDS(df_events_all, file = paste0(data_files, "03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

beep()






