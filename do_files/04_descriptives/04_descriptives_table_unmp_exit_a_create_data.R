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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)

options(scipen = 999) # disable scientific notation

# load data -----


# Country data
df_au <- readRDS(paste0(data_files, "AU/au_sample_v02.rds")) # all non permanent jobs are temporary (except casual)
df_ch <- readRDS(paste0(data_files, "CH/ch_sample.rds"))
df_de <- readRDS(paste0(data_files, "DE/de_sample.rds"))
df_uk <- readRDS(paste0(data_files, "UK/uk_sample_v01.rds")) # all non permanent jobs are temporary
df_jp <- readRDS(paste0(data_files, "JP/jp_sample.rds"))
df_ko <- readRDS(paste0(data_files, "KO/ko_sample.rds"))
df_ne_lsp <- readRDS(paste0(data_files, "NE/LSP/ne_sample.rds"))
df_it <- readRDS(paste0(data_files, "IT/it_sample.rds")) 

df_sample_0 <- rbind(df_au,df_ch,df_de,df_uk,df_jp,df_ko,df_ne_lsp,df_it)
# df_sample_0 <- rbind(df_it)

df_sample_0 <- df_sample_0 %>%
  filter(year>=2000 & year<=2018) %>%
  filter(age >= 25 & age <= 54) %>%
  filter(lfp == 1) %>%
  filter(unmp == 1 | (unmp == 0 & !is.na(emp_status))) %>%
  select(country, pid, year, unmp, emp_status)

table(df_sample_0$unmp)

# Clean unemployment so that if you have a contract you are employed
# Add variable for employed, permanent employment, temporary employment, year_lag
with(df_sample_0,table(unmp,emp_status,useNA = "ifany"))
df_sample_0 <- df_sample_0 %>%
  mutate(unmp = ifelse(!is.na(emp_status), yes = 0, no = 1),
         perm = ifelse(unmp==0 & emp_status==1, yes = 1, no = 0),
         temp = ifelse(unmp==0 & emp_status==2, yes = 1, no = 0),
         emp = ifelse(temp == 1 | perm == 1, yes = 1,
                      ifelse(unmp == 1, yes = 0, no = NA)),
         year_lag = ifelse(country == "IT" | country == "NE-LSP", yes = 2, no = 1)) # indicates biannual data

with(df_sample_0,table(unmp,emp_status,useNA = "ifany"))
table(df_sample_0$unmp)

df_step_00 <- df_sample_0 %>%
  group_by(country, pid) %>%
  mutate(count = row_number(),
         periods = max(count),
         periods_2 = ifelse(periods>1, yes = 1, no = 0),
         periods_3 = ifelse(periods>2, yes = 1, no = 0)
         ) %>%
  slice(1) %>%
  group_by(country) %>%
  mutate(country = country) %>%
  summarise(total = max(row_number()),
            total_2 = sum(periods_2),
            total_3 = sum(periods_3)
  ) %>%
  ungroup()

df_step_00

# Step 1: must experience unemployment ----
df_step_01 <- df_sample_0 %>%
  arrange(country, pid, year) %>%
  group_by(country, pid) %>%
  mutate(unmp_ever = max(unmp),
  ) %>%
  ungroup()

# Total possible events 
df_step_01a <- df_step_01 %>%
  group_by(country, pid) %>%
  slice(1) %>%
  group_by(country) %>%
  mutate(country = country) %>%
  summarise(total = max(row_number()),
            unmp_ever = sum(unmp_ever, na.rm = TRUE),
  ) %>%
  ungroup()

df_step_01a

# Step 2: must exit unemployment ----
df_step_02 <- df_step_01 %>%
  arrange(country, pid, year) %>%
  group_by(country, pid) %>%
  mutate(exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
         exit_unmp_year = ifelse(exit_unmp, yes = year, no = NA),
         exit_unmp_year = min(exit_unmp_year, na.rm = TRUE),
         exit_unmp_yes = max(exit_unmp),
         unmp_ever = max(unmp),
  ) %>%
  ungroup()

# Total possible events 
df_step_02a <- df_step_02 %>%
  group_by(country, pid) %>%
  slice(1) %>%
  group_by(country) %>%
  mutate(country = country) %>%
  summarise(total = max(row_number()),
            unmp_ever = sum(unmp_ever, na.rm = TRUE),
            exit_unmp = sum(exit_unmp_yes, na.rm = TRUE),
  ) %>%
  ungroup()

df_step_02a


# Step 3: must be employed at least 1 period within 4 years after exit ----
df_step_03 <- df_step_02 %>%
  arrange(country, pid, year) %>%
  group_by(country, pid) %>%
  mutate(
    post = ifelse(!is.na(exit_unmp_year), yes = year - exit_unmp_year, no = 0),
    post_emp = ifelse(post>0 & post < 5 & exit_unmp_yes == 1 & unmp == 0, yes = 1, no = 0),
    post_emp_yes = max(post_emp),
  ) %>%
  ungroup()

# Total possible events 
df_step_03a <- df_step_03 %>%
  group_by(country, pid) %>%
  slice(1) %>%
  group_by(country) %>%
  mutate(country = country) %>%
  summarise(unmp_ever = sum(unmp_ever),
            exit_unmp = sum(exit_unmp_yes),
            exit_unmp_post = sum(post_emp_yes),
  ) %>%
  ungroup()

df_step_03a
# Step 4: must be employed at least 1 period within 4 years after exit ----
df_step_04 <- df_step_03 %>%
  arrange(country, pid, year) %>%
  group_by(country, pid) %>%
  mutate(
    unmp_temp = ifelse(post_emp_yes == 1 & temp == 1 & lag(unmp,1)==1, yes = 1, no = 0),
    unmp_temp_yes = max(unmp_temp,na.rm = TRUE),
    unmp_perm = ifelse(post_emp_yes == 1 & perm == 1 & lag(unmp,1)==1, yes = 1, no = 0),
    unmp_perm_yes = max(unmp_perm,na.rm = TRUE),
  ) %>%
  ungroup() 

# Total possible events 
df_step_04a <- df_step_04 %>%
  group_by(country, pid) %>%
  slice(1) %>%
  group_by(country) %>%
  mutate(country = country) %>%
  summarise(unmp_ever = sum(unmp_ever),
            exit_unmp = sum(exit_unmp_yes),
            exit_unmp_post = sum(post_emp_yes, na.rm = TRUE),
            exit_unmp_temp = sum(unmp_temp_yes, na.rm = TRUE),
            exit_unmp_perm = sum(unmp_perm_yes, na.rm = TRUE),
  ) %>%
  ungroup()

df_step_04a

# Save data  ----

write.csv(df_step_04, file = paste0(data_files, "04_df_descriptives_table_unmp_exit.csv"),row.names = FALSE)
