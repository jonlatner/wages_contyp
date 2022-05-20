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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wages_contyp/")

data_files = "data_files/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)
library(beepr)

options(scipen = 999) # disable scientific notation


# Load data -----

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "OECD/oecd_exchange_rate_2010.csv"), sep = ";")

# Unemployment rate - world bank
df_unmp_rate <- read.csv(paste0(support_files, "world_bank/world_bank_unmp.csv"), sep = ",")

# Country data
df_au <- readRDS(paste0(data_files, "AU/au_sample_v01.rds")) # distinguish between ftc and casual

df_sample_0 <- rbind(df_au)

# Filter (03a_combine_filter.R) ----

df_sample_1 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)

# keep if prime age
df_sample_2 <- df_sample_1 %>%
        filter(age >= 25 & age <= 54)

# keep if labor force participant
df_sample_3 <- df_sample_2 %>%
        filter(lfp == 1)

df_sample_4 <- df_sample_3 %>%
        filter(unmp == 1 | (unmp == 0 & !is.na(emp_status)))

# keep if unemployed or employed and monthly hours between 40 and 320 (i.e. no less than 10 hours per week and no more than 80 hours per week)
df_sample_5 <- df_sample_4 %>%
        filter(unmp == 1 | (unmp == 0 & (hours >= 40 & hours <= 320))) %>%
        filter(unmp == 1 | (unmp == 0 & (hourly_wage > 0)))

# keep if non missing values on key demographic variables
df_sample_6 <- df_sample_5 %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        arrange(country,pid,year)

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
        mutate(wages_dollars = wages/exchange,
               hourly_wage_dollars = hourly_wage/exchange)

df_test_8 <- df_sample_8 %>%
        filter(unmp==0)

df_wages_1 <- df_sample_8 %>%
        filter(unmp==0 & hourly_wage > 0) %>% # employed and wages are greater than 0
        group_by(country,year) %>%
        summarise(bot_1 = quantile(hourly_wage, 0.005, na.rm = TRUE), 
                  top_1 = quantile(hourly_wage, 0.995, na.rm = TRUE)) %>%
        ungroup()
summary(df_wages_1)

# merge top/bot wage cutoffs
df_sample_9 <- merge(df_sample_8,df_wages_1,by=c("country","year")) %>%
        arrange(country,pid,year)

df_sample_9 <- df_sample_9 %>%
        mutate(hourly_wage = ifelse(hourly_wage < bot_1, yes = NA, no = hourly_wage),
               hourly_wage = ifelse(hourly_wage > top_1, yes = NA, no = hourly_wage))

# keep if unemployed or monthly wages not missing
df_sample_10 <- df_sample_9 %>%
        filter(unmp == 1 | (unmp==0 & !is.na(hourly_wage)))

df_part_01 <- df_sample_10

rm(list=ls(pattern="df_sample"))

# Clean wages (03b_clean.R) ----

# If unemployed, then wages == 0
df_sample_1 <- df_part_01 %>%
        mutate(
                wages = ifelse(unmp==1, yes = 0, no = wages),
                ln_wages = log(wages),
                ln_wages = ifelse(wages == 0 & unmp==1, yes = 0, no = ln_wages),
                hourly_wage = ifelse(unmp==1, yes = 0, no = hourly_wage),
                ln_hourly_wage = log(hourly_wage),
                ln_hourly_wage = ifelse(hourly_wage == 0 & unmp==1, yes = 0, no = ln_hourly_wage),
                hourly_wage_dollars = ifelse(unmp==1, yes = 0, no = hourly_wage_dollars),
                ln_hourly_wage_dollars = log(hourly_wage_dollars),
                ln_hourly_wage_dollars = ifelse(hourly_wage_dollars == 0 & unmp==1, yes = 0, no = ln_hourly_wage_dollars),
        )

# Clean employment status ----

# If unemployed, then no work contract (temp or permanent)
# recode employment status (0=unemployed; 1=temp contract; 2=perm contract; 3=temp, not not FTC)
df_sample_1 <- df_sample_1 %>%
        mutate(emp_status = ifelse(unmp == 1, yes = 0, no = emp_status),
               perm = ifelse(unmp==0 & emp_status==1, yes = 1, no = 0),
               temp = ifelse(unmp==0 & emp_status==2, yes = 1, no = 0))

# determine minimum difference between sample periods (i.e. annual or biannual)
df_sample_1 <- df_sample_1 %>%
        group_by(country, pid) %>%
        mutate(year_lag = year - lag(year,1,default = NA)) %>%
        group_by(country) %>%
        mutate(year_lag = min(year_lag,na.rm = TRUE)) %>%
        ungroup()

df_part_02 <- df_sample_1
rm(list=ls(pattern="df_sample"))

# (03c_prepare_multiple_events_data) ----

df_multiple_events <- df_part_02 %>%
        select(country, pid, year, year_lag, wages, ln_wages, hourly_wage, ln_hourly_wage, emp_status, unmp, perm, temp, age, edu_cat, male, unemployment_rate)

# Transition indicator ----

df_sample_2 <- df_multiple_events %>%
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
# 1 period post event (t_1, ..., t_4) and employed
# Post event:
# must be employed at least 1 period < 5 years after treatment
# Event 1 - temp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_t_p = ifelse(event_t_p_yes == 1 & perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_t_p_year = ifelse(event_t_p == 1, yes = eventyear, no = NA),
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
        ) 

# Event 2 - unmp into perm ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_p = ifelse(event_u_p_yes == 1 & perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                      event_u_p_year = ifelse(event_u_p == 1, yes = eventyear, no = NA),
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
        )

# Event 3 - unmp into temp ----

df_multiple_events <- suppressWarnings(df_multiple_events %>%
                                               arrange(country, pidseq, year) %>%
                                               group_by(country, pidseq) %>%
                                               mutate(event_u_t = ifelse(event_u_t_yes == 1 & temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                      event_u_t_year = ifelse(event_u_t == 1, yes = eventyear, no = NA),
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

saveRDS(df_events_all, file = paste0(data_files, "df_AU_sensitivity.rds"))

