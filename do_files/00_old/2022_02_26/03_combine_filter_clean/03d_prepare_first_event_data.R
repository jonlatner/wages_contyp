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

df_original <- readRDS(paste0(data_files, "03b_df_sample_cleaned.rds"))

df_original <- df_original %>%
        # filter(country == "IT") %>%
        select(country, pid, year, year_lag, wages, ln_wages, hourly_wage, ln_hourly_wage, emp_status, unmp, perm, temp, age, edu_cat, male, unemployment_rate)

# Event 1 - temp into perm ----

# event_drop_01 means keep if observable after treatment (but still could be unemployed)
# event_drop_02 means keep if employed after treatment
# both are important because event_drop_02 will not identify individuals who are not observable after treatment

df_sample_01 <- suppressWarnings(df_original %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                 event_t_p_yes = max(event_t_p),
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = NA),
                                                 event_t_p_time = ifelse(event_t_p_time < -3, yes = -3,
                                                                         ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                                 ) %>%
                                          ungroup())

df_event_t_p <- df_sample_01 %>%
        filter(event_t_p_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_t_p_drop_01 = ifelse(year > event_t_p_year, yes = 0, no = 1), # keep if observable after treatment
               event_t_p_drop_01 = last(event_t_p_drop_01),
               event_t_p_drop_02 = ifelse(year > event_t_p_year & event_t_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_t_p_drop_01 == 0 & event_t_p_drop_02 == 0) %>%
        mutate(difference = year - event_t_p_year,
               number = ifelse(year > event_t_p_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(event_t_p_yes_final = 1) %>%
        select(country, pid, event_t_p_yes_final)

with(df_event_t_p,table(country,event_t_p_yes_final,useNA = "ifany"))

df_sample_02 <- merge(df_sample_01,df_event_t_p,all.x =TRUE) %>%
        arrange(country, pid, year) %>%
        mutate(event_t_p_yes_final = ifelse(is.na(event_t_p_yes_final), yes = 0, no = event_t_p_yes_final),
               event_t_p_time_pos = ifelse(event_t_p_yes_final == 1, yes = event_t_p_time + 3, no = 0), # bring all to positive values
        )

# Event 2 - unmp into perm ----

df_sample_03 <- suppressWarnings(df_sample_02 %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                 event_u_p_yes = max(event_u_p),
                                                 event_u_p_year = ifelse(event_u_p == 1, yes = year, no = NA),
                                                 event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                 event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = NA),
                                                 event_u_p_time = ifelse(event_u_p_time < -3, yes = -3,
                                                                         ifelse(event_u_p_time > 4, yes = 4, no = event_u_p_time)),
                                          ) %>%
                                          ungroup())

df_event_u_p <- df_sample_03 %>%
        filter(event_u_p_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_u_p_drop_01 = ifelse(year > event_u_p_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_p_drop_01 = last(event_u_p_drop_01),
               event_u_p_drop_02 = ifelse(year > event_u_p_year & event_u_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0) %>%
        mutate(difference = year - event_u_p_year,
               number = ifelse(year > event_u_p_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(event_u_p_yes_final = 1) %>%
        select(country, pid, event_u_p_yes_final)

with(df_event_u_p,table(country,event_u_p_yes_final,useNA = "ifany"))

df_sample_04 <- merge(df_sample_03,df_event_u_p,all.x =TRUE) %>%
        arrange(country, pid, year) %>%
        mutate(event_u_p_yes_final = ifelse(is.na(event_u_p_yes_final), yes = 0, no = event_u_p_yes_final),
               event_u_p_time_pos = ifelse(event_u_p_yes_final == 1, yes = event_u_p_time + 3, no = 0), # bring all to positive values
        )

# Event 3 - unmp into temp ----

df_sample_05 <- suppressWarnings(df_sample_04 %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_t_yes = max(event_u_t),
                                                 event_u_t_year = ifelse(event_u_t == 1, yes = year, no = NA),
                                                 event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                 event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = NA),
                                                 event_u_t_time = ifelse(event_u_t_time < -3, yes = -3,
                                                                         ifelse(event_u_t_time > 4, yes = 4, no = event_u_t_time)),
                                          ) %>%
                                          ungroup())

df_event_u_t <- df_sample_05 %>%
        filter(event_u_t_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_u_t_drop_01 = ifelse(year > event_u_t_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_t_drop_01 = last(event_u_t_drop_01),
               event_u_t_drop_02 = ifelse(year > event_u_t_year & event_u_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0) %>%
        mutate(difference = year - event_u_t_year,
               number = ifelse(year > event_u_t_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(event_u_t_yes_final = 1) %>%
        select(country, pid, event_u_t_yes_final)

with(df_event_u_t,table(country,event_u_t_yes_final,useNA = "ifany"))

df_sample_06 <- merge(df_sample_05,df_event_u_t,all.x =TRUE) %>%
        arrange(country, pid, year) %>%
        mutate(event_u_t_yes_final = ifelse(is.na(event_u_t_yes_final), yes = 0, no = event_u_t_yes_final),
               event_u_t_time_pos = ifelse(event_u_t_yes_final == 1, yes = event_u_t_time + 3, no = 0),
        )

# Event 4 - perm into temp ----

df_sample_07 <- suppressWarnings(df_sample_06 %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_p_t_yes = max(event_p_t),
                                                 event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                 event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                 event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = NA),
                                                 event_p_t_time = ifelse(event_p_t_time < -3, yes = -3,
                                                                         ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                          ) %>%
                                          ungroup())


df_event_p_t <- df_sample_07 %>%
        filter(event_p_t_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_p_t_drop_01 = ifelse(year > event_p_t_year, yes = 0, no = 1), # keep if observable after treatment
               event_p_t_drop_01 = last(event_p_t_drop_01),
               event_p_t_drop_02 = ifelse(year > event_p_t_year & event_p_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_p_t_drop_01 == 0 & event_p_t_drop_02 == 0) %>%
        mutate(difference = year - event_p_t_year,
               number = ifelse(year > event_p_t_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(event_p_t_yes_final = 1) %>%
        select(country, pid, event_p_t_yes_final)

with(df_event_p_t,table(country,event_p_t_yes_final,useNA = "ifany"))

df_sample_08 <- merge(df_sample_07,df_event_p_t,all.x =TRUE) %>%
        arrange(country, pid, year) %>%
        mutate(event_p_t_yes_final = ifelse(is.na(event_p_t_yes_final), yes = 0, no = event_p_t_yes_final),
               event_p_t_time_pos = ifelse(event_p_t_yes_final == 1, yes = event_p_t_time + 3, no = 0),
        )

# Save data sets ----

df_events_all <- df_sample_08 %>%
        select(country,pid,year,everything()) 

df_events_all <- df_events_all %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0)) %>%
        ungroup() %>%
        filter(max>2)

saveRDS(df_events_all, file = paste0(data_files, "03d_df_sample_cleaned_prepared_first_event_data.rds"))

beep()

rm(list=ls(pattern="df_sample"))
