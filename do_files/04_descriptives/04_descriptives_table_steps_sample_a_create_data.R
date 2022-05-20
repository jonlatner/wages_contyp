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
results = "results/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(forcats)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter_01 <- readRDS(file = paste0(data_files,"3a_df_filter_steps.rds"))

df_filter_01 <- df_filter_01 %>%
  filter(country!="NE-LISS")
table(df_filter_01$country)
df_filter_01$country <- recode(df_filter_01$country, "'NE-LSP'='NE'")

# load data -----

df_event_data_01 <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))
df_event_data_01 <- df_event_data_01 %>%
  filter(country!="NE-LISS")
table(df_event_data_01$country)
df_event_data_01$country <- recode(df_event_data_01$country, "'NE-LSP'='NE'")

step_8 <- df_event_data_01 %>%
        select(country,pid,pidseq) %>%
        group_by(country,pid,pidseq) %>%
        slice(1) %>%
        group_by(country,pid) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(count = sum(n())) %>%
        ungroup()
step_8$step <- 8

step_9 <- df_event_data_01 %>%
        filter(unmp==0) %>%
        select(country,pid,pidseq) %>%
        group_by(country,pid,pidseq) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country,pid) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(count = sum(n())) %>%
        ungroup()
step_9$step <- 9

df_filter_02 <- rbind(df_filter_01,step_8,step_9)

# clean data -----


# if treated, then must be employed after treatment
# if not treated, then must be employed
# must be observable at least 3 times

# Unmp to perm

df_event_u_p_total <- df_event_data_01 %>%
        select(country,pid,pidseq,year,event_u_p_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_u_p_yes_final) %>% # this ensures that if there are duplicate pid, that we select the one who experienced the event
        slice(1) %>%
        ungroup() %>%
        summarise(obs_diff = mean(event_u_p_yes_final),
                  total = sum(event_u_p_yes_final)) %>%
        ungroup() %>%
        mutate(step = "C") %>%
        mutate(country = "Total") %>%
        filter(row_number()==1)

df_event_u_p_country <- df_event_data_01 %>%
        select(country,pid,pidseq,year,event_u_p_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_u_p_yes_final) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(total = n(),
                  obs_diff = mean(event_u_p_yes_final),
                  total = sum(event_u_p_yes_final)) %>%
        ungroup() %>%
        mutate(step = "C")

df_event_u_p <- rbind(df_event_u_p_total,df_event_u_p_country)

# Unmp to temp

df_event_u_t_total <- df_event_data_01 %>%
        select(country,pid,pidseq,year,event_u_t_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_u_t_yes_final) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(total = n(),
                  obs_diff = mean(event_u_t_yes_final),
                  total = sum(event_u_t_yes_final)) %>%
        ungroup() %>%
        mutate(step = "D") %>%
        mutate(country = "Total") %>%
        filter(row_number()==1)

df_event_u_t_country <- df_event_data_01 %>%
        select(country,pid,pidseq,year,event_u_t_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_u_t_yes_final) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(total = n(),
                  obs_diff = mean(event_u_t_yes_final),
                  total = sum(event_u_t_yes_final)) %>%
        ungroup() %>%
        mutate(step = "D")

df_event_u_t <- rbind(df_event_u_t_total,df_event_u_t_country)

# Temp to perm

df_event_t_p_total <- df_event_data_01 %>%
  filter(unmp==0) %>%
  select(country,pid,pidseq,year,event_t_p_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_t_p_yes_final) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(total = n(),
                  obs_diff = mean(event_t_p_yes_final),
                  total = sum(event_t_p_yes_final)) %>%
        ungroup() %>%
        mutate(step = "A") %>%
        mutate(country = "Total") %>%
        filter(row_number()==1)

df_event_t_p_country <- df_event_data_01 %>%
        filter(unmp==0) %>%
        select(country,pid,pidseq,year,event_t_p_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_t_p_yes_final) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(total = n(),
                  obs_diff = mean(event_t_p_yes_final),
                  total = sum(event_t_p_yes_final)) %>%
        ungroup() %>%
        mutate(step = "A")
        
df_event_t_p <- rbind(df_event_t_p_total,df_event_t_p_country)

# Perm to temp

df_event_p_t_total <- df_event_data_01 %>%
        filter(unmp==0) %>%
        select(country,pid,pidseq,year,event_p_t_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_p_t_yes_final) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(total = n(),
                  obs_diff = mean(event_p_t_yes_final),
                  total = sum(event_p_t_yes_final)) %>%
        ungroup() %>%
        mutate(step = "B") %>%
        mutate(country = "Total") %>%
        filter(row_number()==1)

df_event_p_t_country <- df_event_data_01 %>%
        filter(unmp==0) %>%
        select(country,pid,pidseq,year,event_p_t_yes_final) %>%
        group_by(country, pid, pidseq) %>%
        mutate(number = row_number(),
               max = max(number),
        ) %>%
        filter(max>2) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        arrange(country,pid,-event_p_t_yes_final) %>%
        slice(1) %>%
        group_by(country) %>%
        summarise(total = n(),
                  obs_diff = mean(event_p_t_yes_final),
                  total = sum(event_p_t_yes_final)) %>%
        ungroup() %>%
        mutate(step = "B")

df_event_p_t <- rbind(df_event_p_t_total,df_event_p_t_country)


# rm(df_event_t_p_total,df_event_t_p_country)
# rm(df_event_p_t_total,df_event_p_t_country)
# rm(df_event_u_p_total,df_event_u_p_country)
# rm(df_event_u_t_total,df_event_u_t_country)


# Append ----

df_event_data_02 <- rbind(df_event_u_p,df_event_u_t,df_event_t_p,df_event_p_t)
# rm(df_employed,df_event_t_p,df_event_p_t,df_event_u_p,df_event_u_t)

df_event_data_02 <- df_event_data_02 %>%
  mutate(obs_diff = paste0(round(obs_diff*100,0),"\\%"))

df_event_data_02

df_event_data_02$country_name <- recode(df_event_data_02$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'; 'Total'='Total' ")

df_event_data_03 <- df_event_data_02 %>%
  mutate(notes = ifelse(step == "A", yes = "Temp $\\rightarrow$ perm",
                        ifelse(step == "B", yes = "Perm $\\rightarrow$ temp",
                               ifelse(step == "C", yes = "Unmp $\\rightarrow$ perm",
                                      ifelse(step == "D", yes = "Unmp $\\rightarrow$ temp",
                                             no = NA))))) %>%
  mutate(step = ifelse(step == "A", yes = "B", 
                       ifelse(step == "B", yes = "B",
                              ifelse(step == "C", yes = "A",
                                     ifelse(step == "D", yes = "A", no = step)))))
  
# Clean/summarize data ----

df_filter_02$country_name <- recode(df_filter_02$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_filter_country <- df_filter_02 %>%
  group_by(country,step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(country,step) %>%
  select(country_name,step,total)

df_filter_total <- df_filter_02 %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  mutate(country_name = "Total") %>%
  select(country_name,step,total)

df_filter_03 <- rbind(df_filter_country,df_filter_total)
df_filter_03 <- df_filter_03 %>%
  arrange(country_name,step)

df_filter_03$country_name <- fct_relevel(df_filter_03$country_name, "Total", after = 0) # forcats

# Clean data ----

df_filter_04 <- df_filter_03 %>%
        filter(step != 3) %>%
        filter(step != 4) %>%
        mutate(step = ifelse(step > 4, yes = step - 2, no = step)) %>%
        group_by(country_name) %>%
        mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
               obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
               ) %>%
        ungroup()

df_filter_05 <- df_filter_04 %>%
  mutate(notes = ifelse(step == 0, yes = "Raw data",
                        ifelse(step == 1, yes = "Panel years between 2000 and 2018",
                               ifelse(step == 2, yes = "Prime age (25 - 54)",
                                      ifelse(step == 3, yes = "Unemployed or employed with contract type, monthly hours (40 -- 320), and wages $>$ 0",
                                             ifelse(step == 4, yes = "Non missing education or gender",
                                                    ifelse(step == 5, yes = "Hourly wages within the top/bottom 0.005 percentile",
                                                           ifelse(step == 6, yes = "Data set A: At least 3 observations",
                                                                  ifelse(step == 7, yes = "Data set B: + always employed",
                                                                         no = NA)))))))))
df_filter_05

# Reshape data ----

df_filter_wide <- df_filter_05 %>%
  pivot_wider(names_from = country_name, values_from = c("total","obs_diff"))
df_filter_wide <- df_filter_wide %>%
  select(step, notes, 
         matches("_Total"),
         matches("Australia"), 
         matches("Germany"), 
         matches("Italy"), 
         matches("Japan"), 
         matches("Korea"), 
         matches("Netherlands"),
         matches("Switzerland"),
         matches("United Kingdom"),
  )
df_filter_wide

df_events_data_wide <- df_event_data_03 %>%
  select(-country) %>%
  pivot_wider(names_from = country_name, values_from = c("total","obs_diff"))
df_events_data_wide <- df_events_data_wide %>%
  select(step, notes,
         matches("_Total"),
         matches("Australia"), 
         matches("Germany"), 
         matches("Italy"), 
         matches("Japan"), 
         matches("Korea"), 
         matches("Netherlands"),
         matches("Switzerland"),
         matches("United Kingdom"),
  )
df_events_data_wide

df_append <- rbind(df_filter_wide,df_events_data_wide)

# Save data  ----

saveRDS(df_append, file = paste0(data_files, "04_df_descriptives_table_steps_sample.rds"))

beep()

