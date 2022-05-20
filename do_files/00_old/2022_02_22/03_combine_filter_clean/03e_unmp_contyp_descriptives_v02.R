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
library(car)
library(forcats)

options(scipen = 999) # disable scientific notation

# load data -----

df_first <- readRDS(paste0(data_files, "03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_first <- df_first %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

# df_first <- df_first %>%
#         filter(country == "IT")

# clean sample data -----

# Unmp to temp
df_sample_u_t <- df_first %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_t")) %>%
        filter((event_u_t_yes == 1 & event_u_t_drop_01 == 0)) %>%
        filter((event_u_t_yes == 1 & event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0)) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               age_max = max(age),
               unique = ifelse(row_number()==1, yes = 1, no = 0)) %>%
        ungroup() %>%
        filter(max>2) %>%
        select(-number,-max)

with(df_sample_u_t,table(country,event_u_t))

# Unmp to perm
df_sample_u_p <- df_first %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_p")) %>%
        filter((event_u_p_yes == 1 & event_u_p_drop_01 == 0)) %>%
        filter((event_u_p_yes == 1 & event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0)) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               age_max = max(age),
               unique = ifelse(row_number()==1, yes = 1, no = 0)) %>%
        ungroup() %>%
        filter(max>2) %>%
        select(-number,-max)

with(df_sample_u_p,table(country,event_u_p))

# Event - unmp into emp ----

df_unmp_01 <- df_first %>%
        select(country, pid, age, year, year_lag, unmp, perm, temp, matches("u_t"), matches("u_p")) %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
               exit_unmp_yes = max(event_exit_unmp),
               unmp_ever = max(unmp),
               count = row_number(),
               max = max(count),
               age_max = max(age),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        filter(unmp_ever==1)

# event_drop_01 means keep if observable after treatment
# event_drop_02 means keep if employed after treatment
# both are important because event_drop_02 will not identify individuals who are not observable after treatment

# Total possible events
t_01 <- with(subset(df_unmp_01,unique == 1),table(country,useNA = "ifany"))
t_01 <- data.frame(t_01)
t_01$step <- 1

# must experience unemployment
# must be observable at least 2 to experience post employment
df_unmp_02 <- df_unmp_01 %>%
        filter(max > 1)

t_02 <- with(subset(df_unmp_02,unique == 1),table(country,useNA = "ifany"))
t_02 <- data.frame(t_02)
t_02$step <- 2

df_test <- df_unmp_02 %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),unique)
with(df_test,table(unique,exit_unmp_yes,useNA = "ifany"))

# must exit unemployment
df_unmp_03 <- df_unmp_02 %>%
        filter(exit_unmp_yes == 1)

t_03 <- with(subset(df_unmp_03,unique == 1),table(country,useNA = "ifany"))
t_03 <- data.frame(t_03)
t_03$step <- 3
t_03

df_test <- df_unmp_03 %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),unique,matches("drop"))
with(subset(df_test,unique==1),table(event_u_p_drop_01,event_u_t_drop_01,useNA = "ifany"))

# must be observable after treatment (i.e. 3 periods of observation)
df_unmp_04_u_p <- df_unmp_03 %>%
        select(country,pid,unmp,temp,perm,matches("age"),matches("unmp"),matches("event_u_p"),unique) %>%
        filter(event_u_p_drop_01 == 0) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
               ) %>%
        ungroup() %>% 
        filter(max > 2)
        
df_unmp_04_u_t <- df_unmp_03 %>%
        select(country,pid,unmp,temp,perm,matches("unmp"),matches("event_u_t"),unique) %>%
        filter(event_u_t_drop_01 == 0) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        ungroup() %>% 
        filter(max > 2)

with(subset(df_unmp_04_u_p,unique == 1),table(country,useNA = "ifany"))

with(subset(df_unmp_04_u_t,unique == 1),table(country,useNA = "ifany"))

t_04_u_p <- with(subset(df_unmp_04_u_p,unique == 1),table(country,useNA = "ifany"))
t_04_u_p <- data.frame(t_04_u_p)
t_04_u_p

t_04_u_t <- with(subset(df_unmp_04_u_t,unique == 1),table(country,useNA = "ifany"))
t_04_u_t <- data.frame(t_04_u_t)
t_04_u_t

t_04 <- rbind(t_04_u_p,t_04_u_t) %>%
        group_by(country) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup()

t_04$step <- 4
t_04

# must be employed after transition into employment
df_unmp_05_u_p <- df_unmp_04_u_p %>%
        select(country,pid,unmp,temp,perm,matches("age"),matches("unmp"),matches("event_u_p"),unique,max) %>%
        filter(event_u_p_drop_02 == 0) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        ungroup() %>% 
        filter(max > 2)

df_unmp_05_u_t <- df_unmp_04_u_t %>%
        select(country,pid,unmp,temp,perm,matches("unmp"),matches("event_u_t"),unique,max) %>%
        filter(event_u_t_drop_02 == 0) %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        ungroup() %>% 
        filter(max > 2)

with(subset(df_unmp_05_u_p,unique == 1),table(country,useNA = "ifany"))

with(subset(df_unmp_05_u_t,unique == 1),table(country,useNA = "ifany"))

t_05_u_p <- with(subset(df_unmp_05_u_p,unique == 1),table(country,useNA = "ifany"))
t_05_u_p <- data.frame(t_05_u_p)
t_05_u_p

t_05_u_t <- with(subset(df_unmp_05_u_t,unique == 1),table(country,useNA = "ifany"))
t_05_u_t <- data.frame(t_05_u_t)
t_05_u_t

t_05 <- rbind(t_05_u_p,t_05_u_t) %>%
        group_by(country) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup()

t_05$step <- 5
t_05

# compare step 4 to step 5 (unmp to temp)

df_step_04_u_t <- df_unmp_04_u_t %>%
        filter(unique == 1) %>%
        select(country, pid) %>%
        mutate(test_01 = 1)

df_step_05_u_t <- df_unmp_05_u_t %>%
        filter(unique == 1) %>%
        select(country, pid) %>%
        mutate(test_02 = 1)

df_merge <- merge(df_step_04_u_t,df_step_05_u_t,all = TRUE)
df_merge %>% filter(is.na(test_02))

df_unmp_04_u_t %>% filter(pid==8515851)

# compare step 4 to step 5 (unmp to perm)

df_step_04_u_p <- df_unmp_04_u_p %>%
        filter(unique == 1) %>%
        select(country, pid) %>%
        mutate(test_01 = 1)

df_step_05_u_p <- df_unmp_05_u_p %>%
        filter(unique == 1) %>%
        select(country, pid) %>%
        mutate(test_02 = 1)

df_merge <- merge(df_step_04_u_p,df_step_05_u_p,all = TRUE)
df_merge %>% filter(is.na(test_02))

df_unmp_04_u_p %>% filter(pid==8624661)

# summarise selection criteria ----

beep()

# There are a few duplicates where individuals move back to unemployment, but this does not affect unique case counts
with(df_sample_u_t,table(country,event_u_t))
with(subset(df_sample_u_t,unique==1),table(country,event_u_t))

with(df_sample_u_p,table(country,event_u_p))
with(subset(df_sample_u_p,unique==1),table(country,event_u_p))

df_table <- rbind(t_01,t_02,t_03,t_04,t_05)
df_table$event <- "NA"

t_05_u_p$event <- "u_p"
t_05_u_p$step <- 6
t_05_u_t$event <- "u_t"
t_05_u_t$step <- 6

df_table <- bind_rows(df_table,t_05_u_p,t_05_u_t) %>%
        arrange(country, step, event)

df_table

df_table_total <- df_table %>%
        group_by(step,event) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup() %>%
        mutate(country = "Total")

df_table_total

df_append <- rbind(df_table,df_table_total)

df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'; 'Total'='Total' ")
df_append$country_name <- fct_relevel(df_append$country_name, "Total", after = 0) # forcats
df_append$event <- as.factor(df_append$event)

df_table_wide <- df_append %>%
        select(-country) %>%
        arrange(country_name,step) %>%
        group_by(country_name) %>%
        mutate(row = row_number()) %>%
        pivot_wider(names_from = c("country_name"),
                    values_from = c("Freq"),
                    ) %>%
        ungroup()

df_table_wide

