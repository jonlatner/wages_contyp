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
library(Hmisc)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files, "df_sample_post_2010.rds"))
df_original <- df_original %>%
        select(country, pid, year, year_lag, emp_status, unmp, perm, temp, ln_hourly_wage, age, unemployment_rate)

# Event 1 - temp into perm ----

df_sample_t_p <- suppressWarnings(df_original %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
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

# df_sample_t_p %>% filter(event_t_p_yes==1) %>% select(pid,year,unmp,perm,matches("event_t_p"))

# Event 2 - unmp into perm ----

df_sample_u_p <- suppressWarnings(df_sample_t_p %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
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

# Event 3 - unmp into temp ----

df_sample_u_t <- suppressWarnings(df_sample_u_p %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
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

# Event 4 - perm into temp ----

df_sample_p_t <- suppressWarnings(df_sample_u_t %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
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

# Save data sets ----

df_events_all <- df_sample_p_t

saveRDS(df_events_all, file = paste0(data_files, "df_sample_first_clean_post_2010.rds"))

rm(list=ls(pattern="df_sample"))

beep()
