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
results = "results/"
tables = "tables/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(car)
library(forcats)

options(scipen = 999) # disable scientific notation

# Load data -----

df_sample_0 <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

df_sample_0 <- df_sample_0 %>%
        filter(country == "DE") %>%
        filter(country!="NE-LISS")
df_sample_0$country <- recode(df_sample_0$country, "'NE-LSP'='NE'")
df_sample_0$country_name <- recode(df_sample_0$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")


# Clean data -----

df_event_t_p <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_t_p, event_t_p_yes, event_t_p_time) %>%
        filter(event_t_p_yes==1&!is.na(event_t_p_time)) %>%
        filter(pid==20096101)


        group_by(pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        group_by(pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        mutate(pidseq = pid*100+transseq)

table(df_event_t_p$transseq)
df_event_t_p %>% filter(transseq==7)
df_event_t_p %>% filter(pid==20096101) %>% print(n=40)

df_event_t_p %>% print(n=20)

head(df_event_t_p)

with(df_event_t_p,table(event_t_p,event_t_p_time))

df_event_t_p %>% filter(event_t_p==1&event_t_p_time==4&event_t_p_yes==1)

df_sample_0 %>% filter(pid==20096101) %>% print(n=40)







