# https://www.online.fbe.unimelb.edu.au/HILDAodd/srchVarnameUsingCategoriesCrossWave.aspx

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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/AU/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----
# This data declares that temporary jobs are jobs with a fixed-term contract (i.e. not seasonal, casual, or temp agency)

df_au <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ",") %>%
        select(year, AU) %>%
        rename(cpi=AU)

# 2010 exchange rate data - OECD 
df_exchange <- read.csv(paste0(support_files, "oecd_exchange_rate_2010.csv"), sep = ";")

# Unemployment rate - world bank
df_unmp_rate <- read.csv(paste0(support_files, "world_bank_unmp.csv"), sep = ",")

# Merge data ----

df_au <- merge(df_au,df_inflation)
rm(df_inflation)

# Clean ----

df_au <- df_au %>%
        rename(occ = jbm682,
               prestige = jbmo6s,
               lfs = esdtl,
               hours=jbmhruc,
               wages=wsfei, # DV: Financial year gross wages & salary ($) [imputed] [weighted topcode]
               edu = edhigh1,
               contyp = jbmcnt,
               sex = hgsex,
               empstat = esempdt,
               age=hgage,
               pid = xwaveid) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_au$lfp <- recode(df_au$lfs, "lo:0=NA; 1:4=1; 5:7=0")

# employment status
df_au$unmp <- recode(df_au$lfs, "lo:0=NA; 1:2=0; 3:4=1; else=NA")

# contract type
with(df_au,table(year,contyp,useNA = "ifany"))

# jbmcnt 1 Contract/fixed time
# jbmcnt 2 Casual
# jbmcnt 3 Permanent
# Employment status (1=permanent, 2=temporary, 3=employed, but not temp or perm)
df_au <- df_au %>%
        mutate(emp_status = ifelse(contyp == 3, yes = 1,
                                     ifelse(contyp == 1, yes = 2, # temporary and ftc
                                            ifelse(contyp == 2, yes = 3, no = NA)))) # temporary and not ftc

with(df_au,table(year,emp_status,useNA = "ifany"))

# Self-employed
df_au$slf <- recode(df_au$empstat, "4:5=1; else=0")

# Occupation (ISCO 88)
df_au$occ <- recode(df_au$occ, "lo:0=NA")

# Prestige (Treiman)
df_au$prestige <- recode(df_au$prestige, "lo:0 = NA")

# Hours (weekly)
df_au$hours <- recode(df_au$hours, "lo:-.001 = NA")

# Wages (annual)
df_au$wages <- recode(df_au$wages, "lo:-.001 = NA")
summary(df_au$wages)
df_au <- df_au %>%
        mutate(wages = wages/(cpi/100),
               wages=wages/12, # monthly wages
               hourly_wage = wages/(hours*4), #monthly wages/monthly hours
               )

# demographics -----
# education (ISCED)
df_au$edu_cat <- recode(df_au$edu, "lo:-1 = NA; 6:9=1; 4:5=2; 1:3=3; 10=NA")

# sex
df_au$male <- recode(df_au$sex, "lo:0 = NA; 2 = 0")

# Age
df_au <- df_au %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# save 

df_sample_0 <- df_au %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "AU"

# Filter (from do_files/03_combine_filter_clean/03a_combine_filter.R) ----

df_sample_1 <- df_sample_0 %>%
        filter(year>=2000 & year<=2018)

df_sample_2 <- df_sample_1 %>%
        filter(lfp == 1)

df_sample_3 <- df_sample_2 %>%
        filter(age >= 25 & age <= 54)

# keep if unemployed or employed, with contract
df_sample_4 <- df_sample_3 %>%
        filter(unmp == 1 | (unmp==0&!is.na(emp_status)))

# keep if unemployed or employed and hours greater than 1
df_sample_5 <- df_sample_4 %>%
        filter(unmp == 1 | (unmp == 0 & hours > 1))

# keep if non missing values on key demographic variables
df_sample_6 <- df_sample_5 %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        arrange(country,pid,year)

# Clean and merge unemployment rate data

df_unmp_rate <- pivot_longer(df_unmp_rate,cols = !year, names_to = "country", values_to = "unemployment_rate")

df_sample_7 <- merge(df_sample_6,df_unmp_rate, by = c("country","year")) %>%
        arrange(country,pid,year)

rm(df_unmp_rate)

# Calculate wage distribution to determine bottom/top 0.01

# merge exchange rate data
df_sample_8 <- merge(df_sample_7,df_exchange, by = c("country"))

# convert wages to U.S. dollars 
df_sample_8 <- df_sample_8 %>%
        mutate(wages_dollars = wages/exchange)

df_wages_1 <- df_sample_8 %>%
        filter(unmp==0 & wages > 0) %>% # employed and wages are greater than 0
        group_by(country,year) %>%
        summarise(bot_1 = quantile(wages, 0.01, na.rm = TRUE), 
                  top_1 = quantile(wages, 0.99, na.rm = TRUE)) %>%
        ungroup()

# merge top/bot wage cutoffs
df_sample_9 <- merge(df_sample_8,df_wages_1,by=c("country","year")) %>%
        arrange(country,pid,year)

df_sample_9 <- df_sample_9 %>%
        mutate(wages = ifelse(wages < bot_1, yes = NA, no = wages),
               wages = ifelse(wages > top_1, yes = NA, no = wages))

df_sample_10 <- df_sample_9 %>%
        filter(unmp == 1 | (unmp==0 & !is.na(wages))) # keep if unemployed or monthly wages not missing

df_wages_2 <- df_sample_10 %>%
        filter(unmp==0 & wages > 0) %>% # employed and wages are greater than 0
        group_by(country,year) %>%
        summarise(bot = min(wages_dollars), 
                  top = max(wages_dollars)) %>%
        ungroup()
summary(df_wages_2)

df_filter <- df_sample_10
rm(list=ls(pattern="df_sample"))
rm(df_wages_1,df_wages_2,df_exchange)

# Clean wages (from do_files/03_combine_filter_clean/03b_clean.R)  ----

df_sample_1 <- df_filter

# If unemployed, then wages == 0
df_sample_1 <- df_sample_1 %>%
        mutate(
                wages = ifelse(unmp==1, yes = 0, no = wages),
                ln_wages = log(wages),
                ln_wages = ifelse(wages == 0 & unmp==1, yes = 0, no = ln_wages),
                hourly_wage = ifelse(unmp==1, yes = 0, no = hourly_wage),
                ln_hourly_wage = log(hourly_wage),
                ln_hourly_wage = ifelse(hourly_wage == 0 & unmp==1, yes = 0, no = ln_hourly_wage),
        )

# Clean employment status 
# If unemployed, then no work contract (temp or permanent)
# recode employment status (0=unemployed; 1=temp contract; 2=perm contract; 3=temp, not not FTC)
df_sample_1 <- df_sample_1 %>%
        mutate(emp_status = ifelse(unmp == 1, yes = 0, no = emp_status),
               perm = ifelse(unmp==0 & emp_status==1, yes = 1, no = 0),
               temp = ifelse(unmp==0 & emp_status==2, yes = 1, no = 0))

table(df_sample_1$emp_status, useNA = "ifany")
with(df_sample_1,table(emp_status,perm,useNA = "ifany"))
with(df_sample_1,table(emp_status,temp,useNA = "ifany"))

# determine minimum difference between sample periods (i.e. annual or biannual)
df_sample_1 <- df_sample_1 %>%
        group_by(country, pid) %>%
        mutate(year_lag = year - lag(year,1,default = NA)) %>%
        group_by(country) %>%
        mutate(year_lag = min(year_lag,na.rm = TRUE)) %>%
        ungroup()

# must be in at least X consecutive time periods 

# restrict sample to 2 consecutive time periods
# df_sample_2 <- df_sample_1 %>%
#         group_by(country, pid) %>%
#         mutate(test = ifelse(year - lag(year,1)==year_lag & row_number()>0, yes = 1, no = 0),
#                test = ifelse(is.na(test),yes=0,no=test),
#                test = max(test)
#         ) %>%
#         ungroup() %>%
#         filter(test==1) %>%
#         select(-test)

# restrict sample to 3 consecutive time periods
df_sample_2 <- df_sample_1 %>%
        group_by(country, pid) %>%
        mutate(test = ifelse(year - lag(year,1)==year_lag & year - lag(year,2)==year_lag+year_lag & row_number()>0, yes = 1, no = 0),
               test = ifelse(is.na(test),yes=0,no=test),
               test = max(test)
        ) %>%
        ungroup() %>%
        filter(test==1) %>%
        select(-test)

df_filter_clean <- df_sample_2

rm(list=ls(pattern="df_sample"))

# Prepare multiple events data (from do_files/03_combine_filter_clean/03c_prepare_multiple_events.R)  ----

df_sample_0 <- df_filter_clean

df_sample_1 <- df_sample_0 %>%
        select(country, pid, year, year_lag, wages, ln_wages, hourly_wage, ln_hourly_wage, emp_status, unmp, perm, temp, age, edu_cat, male, unemployment_rate)

# Transition indicator 

df_sample_2 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()


# create new data set for each transition
df_transition <- df_sample_2 %>%
        select(country,pid,year,trans,transseq) %>%
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
        mutate(pidseq=pid*10+transseq) %>% # new identifier
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
        mutate(pidseq=pid*10+transseq) %>% # new identifier
        select(-eventtime)

df_multiple_events <- rbind(df_multiple_events,df_transition_non)

rm(list=ls(pattern="df_sample"))
rm(list=ls(pattern="df_transition"))

# Event 1 - temp into perm 

df_sample_t_p <- suppressWarnings(df_multiple_events %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_t_p_yes = max(event_t_p),
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = 0),
                                                 event_t_p_time = ifelse(event_t_p_time < -2, yes = NA,
                                                                         ifelse(event_t_p_time > 4, yes = NA, no = event_t_p_time)),
                                                 event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
                                          ) %>%
                                          ungroup())
table(df_sample_t_p$event_t_p_yes)

# Event 2 - unmp into perm 

df_sample_u_p <- suppressWarnings(df_sample_t_p %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_p_yes = max(event_u_p),
                                                 event_u_p_year = ifelse(event_u_p == 1, yes = year, no = NA),
                                                 event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                 event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = 0),
                                                 event_u_p_time = ifelse(event_u_p_time < -2, yes = NA,
                                                                         ifelse(event_u_p_time > 4, yes = NA, no = event_u_p_time)),
                                                 event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

df_sample_u_p %>% filter(event_u_p_yes==1) %>% select(pid,year,unmp,perm,matches("event_u_p"))

# Event 3 - unmp into temp 

df_sample_u_t <- suppressWarnings(df_sample_u_p %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_t_yes = max(event_u_t),
                                                 event_u_t_year = ifelse(event_u_t == 1, yes = year, no = NA),
                                                 event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                 event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = 0),
                                                 event_u_t_time = ifelse(event_u_t_time < -2, yes = NA,
                                                                         ifelse(event_u_t_time > 4, yes = NA, no = event_u_t_time)),
                                                 event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

# Event 4 - perm into temp 

df_sample_p_t <- suppressWarnings(df_sample_u_t %>%
                                          arrange(country, pidseq, year) %>%
                                          group_by(country, pidseq) %>%
                                          mutate(event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
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

saveRDS(df_events_all, file = paste0(data_files, "au_sample_v02.rds"))
