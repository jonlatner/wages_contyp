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

df_sample_t_p <- suppressWarnings(df_original %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                 event_t_p_yes = max(event_t_p),
                                                 event_t_p_step = na_if(event_t_p, 0), # recode 0 to missing
                                                 event_t_p_step = na.locf(event_t_p_step, na.rm = FALSE), # last observation carry forward
                                                 event_t_p_step = ifelse(is.na(event_t_p_step),0,event_t_p_step), # recode missing to 0
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = NA),
                                                 event_t_p_drop_01 = ifelse(event_t_p_yes==1 & year > event_t_p_year, yes = 0, no = 1), # keep if observable after treatment
                                                 event_t_p_drop_01 = last(event_t_p_drop_01),
                                                 event_t_p_drop_02 = ifelse(event_t_p_yes==1 & event_t_p_time > 0 & unmp == 1, yes = 1, no = 0),  # keep if experience treatment and employed after treatment event
                                                 event_t_p_time = ifelse(event_t_p_time < -3, yes = -3,
                                                                         ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                                 event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0), # bring all to positive values
                                                 ) %>%
                                          ungroup())

# Event 2 - unmp into perm ----

df_sample_u_p <- suppressWarnings(df_sample_t_p %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                 event_u_p_yes = max(event_u_p),
                                                 event_u_p_step = na_if(event_u_p, 0), # recode 0 to missing
                                                 event_u_p_step = na.locf(event_u_p_step, na.rm = FALSE), # last observation carry forward
                                                 event_u_p_step = ifelse(is.na(event_u_p_step),0,event_u_p_step), # recode missing to 0
                                                 event_u_p_year = ifelse(event_u_p == 1, yes = year, no = NA),
                                                 event_u_p_year = min(event_u_p_year, na.rm=TRUE),
                                                 event_u_p_time = ifelse(event_u_p_yes == 1, yes = year - event_u_p_year, no = NA),
                                                 event_u_p_drop_01 = ifelse(event_u_p_yes==1 & year > event_u_p_year, yes = 0, no = 1), # keep if observable after treatment
                                                 event_u_p_drop_01 = last(event_u_p_drop_01),
                                                 event_u_p_drop_02 = ifelse(event_u_p_yes==1 & year > event_u_p_year & event_u_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
                                                 event_u_p_time = ifelse(event_u_p_time < -3, yes = -3,
                                                                         ifelse(event_u_p_time > 4, yes = 4, no = event_u_p_time)),
                                                 event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

# Event 3 - unmp into temp ----

df_sample_u_t <- suppressWarnings(df_sample_u_p %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_u_t_yes = max(event_u_t),
                                                 event_u_t_step = na_if(event_u_t, 0), # recode 0 to missing
                                                 event_u_t_step = na.locf(event_u_t_step, na.rm = FALSE), # last observation carry forward
                                                 event_u_t_step = ifelse(is.na(event_u_t_step),0,event_u_t_step), # recode missing to 0
                                                 event_u_t_year = ifelse(event_u_t == 1, yes = year, no = NA),
                                                 event_u_t_year = min(event_u_t_year, na.rm=TRUE),
                                                 event_u_t_time = ifelse(event_u_t_yes == 1, yes = year - event_u_t_year, no = NA),
                                                 event_u_t_drop_01 = ifelse(event_u_t_yes==1 & year > event_u_t_year, yes = 0, no = 1), # keep if observable after treatment
                                                 event_u_t_drop_01 = last(event_u_t_drop_01),
                                                 event_u_t_drop_02 = ifelse(event_u_t_yes==1 & year > event_u_t_year & event_u_t_time > 0 & unmp == 1, yes = 1, no = 0),
                                                 event_u_t_time = ifelse(event_u_t_time < -3, yes = -3,
                                                                         ifelse(event_u_t_time > 4, yes = 4, no = event_u_t_time)),
                                                 event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

# df_sample_u_t %>% filter(pid==252451) %>% select(pid, year, unmp, temp, perm, matches("u_t"))


table(df_sample_u_t$event_u_t_yes)

# Event 4 - perm into temp ----

df_sample_p_t <- suppressWarnings(df_sample_u_t %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                 event_p_t_yes = max(event_p_t),
                                                 event_p_t_step = na_if(event_u_p, 0), # recode 0 to missing
                                                 event_p_t_step = na.locf(event_u_p_step, na.rm = FALSE), # last observation carry forward
                                                 event_p_t_step = ifelse(is.na(event_u_p_step),0,event_u_p_step), # recode missing to 0
                                                 event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                 event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                 event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = NA),
                                                 event_p_t_drop_01 = ifelse(event_p_t_yes==1 & year > event_p_t_year, yes = 0, no = 1), # keep if observable after treatment
                                                 event_p_t_drop_01 = last(event_p_t_drop_01),
                                                 event_p_t_drop_02 = ifelse(event_p_t_yes==1 & event_p_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep if observable and employed in at least 1 period after the event (>2 periods of observation are necessary for trend)
                                                 event_p_t_time = ifelse(event_p_t_time < -3, yes = -3,
                                                                         ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                                 event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
                                          ) %>%
                                          ungroup())

table(df_sample_p_t$event_p_t_yes)
table(df_sample_p_t$event_p_t_time)

# Save data sets ----

df_events_all <- df_sample_p_t %>%
        select(country,pid,year,everything())


saveRDS(df_events_all, file = paste0(data_files, "03d_df_sample_cleaned_prepared_first_event_data.rds"))

beep()

rm(list=ls(pattern="df_sample"))
