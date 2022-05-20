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
        select(country, pid, year, year_lag, ln_hourly_wage, unmp, perm, temp, age, unemployment_rate)

# Event 1 - temp into perm ----

# event_drop_01 means keep if observable after treatment (but still could be unemployed)
# event_drop_02 means keep if employed after treatment
# both are important because event_drop_02 will not identify individuals who are not observable after treatment

df_sample_01 <- suppressWarnings(df_original %>%
                                          arrange(country, pid, year) %>%
                                          group_by(country, pid) %>%
                                          mutate(event_unmp = ifelse(unmp == 1 & lag(unmp,1) == 0 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                 event_unmp_yes = max(event_unmp),
                                                 event_unmp_year = ifelse(event_unmp == 1, yes = year, no = NA),
                                                 event_unmp_year = min(event_unmp_year, na.rm=TRUE),
                                                 event_unmp_time = ifelse(event_unmp_yes == 1, yes = year - event_unmp_year, no = NA),
                                                 event_unmp_time = ifelse(event_unmp_time < -3, yes = -3,
                                                                         ifelse(event_unmp_time > 4, yes = 4, no = event_unmp_time)),
                                                 ) %>%
                                          ungroup())

df_original %>% filter(pid==252451)

df_event_unmp <- df_sample_01 %>%
        filter(event_unmp_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_unmp_drop_01 = ifelse(year > event_unmp_year, yes = 0, no = 1), # keep if observable after treatment
               event_unmp_drop_01 = last(event_unmp_drop_01),
               event_unmp_drop_02 = ifelse(year > event_unmp_year & event_unmp_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
               event_unmp_drop_02 = max(event_unmp_drop_02),
        ) %>%
        filter(event_unmp_drop_01 == 0 & event_unmp_drop_02 == 0) %>%
        mutate(difference = year - event_unmp_year,
               number = ifelse(year > event_unmp_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(event_unmp_yes_final = 1) %>%
        select(country, pid, event_unmp_yes_final)

# df_event_unmp %>% filter(pid==71651) %>% select(country, pid, year, unmp, temp, perm, matches("drop"))


with(df_event_unmp,table(country,event_unmp_yes_final,useNA = "ifany"))

df_sample_02 <- merge(df_sample_01,df_event_unmp,all.x =TRUE) %>%
        arrange(country, pid, year) %>%
        mutate(event_unmp_yes_final = ifelse(is.na(event_unmp_yes_final), yes = 0, no = event_unmp_yes_final),
               event_unmp_time_pos = ifelse(event_unmp_yes_final == 1, yes = event_unmp_time + 3, no = 0), # bring all to positive values
        )


# Save data sets ----

df_events_all <- df_sample_02 %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0)) %>%
        ungroup() %>%
        filter(max>2)

saveRDS(df_events_all, file = paste0(data_files, "03e_df_sample_cleaned_prepared_first_unmp_data.rds"))

beep()


