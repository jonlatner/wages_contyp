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

data_files = "projects/booth_etal_2002/data_files/update/"
raw_data = "BHPS/raw_data/supporting_files/"
prestige_files = "projects/gebel_2010/DE/data_files/"

# LIBRARY
library(tidyverse)
library(ggplot2)
library(car)
library(data.table)
library(readxl)
library(haven)
library(zoo) #na.locf

options(scipen = 999) # disable scientific notation

# load data --------------------------------------------------------------

df_covars <- readRDS(paste0(data_files, "covars.rds"))
df_contyp <- readRDS(paste0(data_files, "covars_contyp.rds"))
df_prestige <- readRDS(paste0(prestige_files, "prestige.rds"))
df_inflation <- read_excel(paste0(raw_data, "inflation/uk_cpih_index.xls"))

# inflation data 
colnames(df_inflation) <- c("wave_yr", "cpi")

df_inflation <- df_inflation %>%
        filter(row_number()>=6) %>%
        mutate(wave_yr = as.integer(wave_yr)) %>%
        mutate(cpi = round(as.numeric(cpi),2))

# merge in relevant variables data --------------------------------------------------------------

df_uk <- merge(data.table(df_covars), data.table(df_contyp), all.x = TRUE)
df_uk <- merge(data.table(df_uk), data.table(df_prestige), all.x = TRUE, by = c("jbisco88_cc"))
df_uk <- merge(data.table(df_uk), data.table(df_inflation), by = c("wave_yr"), all.x = TRUE)

df_uk <- df_uk %>%
        arrange(pidp, wave_yr)

rm(df_covars,df_contyp,df_inflation,df_prestige)

# Contract type --------------------------------------------------------------
# Contract type (0=permanent, 1=fixed term contract, 2=other temporary)

df_uk <- df_uk %>%
        mutate(perm = ifelse(wave_yr <= 1997 & jbterm == 1, yes = 1,
                             ifelse(wave_yr >= 1998 & jbterm1 == 1, yes = 1, no = 0)),
               perm = ifelse(wave_yr <= 1997 & jbterm < 0, yes = NA, 
                             ifelse(wave_yr >= 1998 & jbterm1 < 0, yes = NA, no = perm)),
               ftc = ifelse(wave_yr <= 1997 & jbterm == 3, yes = 1,
                            ifelse(wave_yr >= 1998 & jbterm1 == 2 & jbterm2 == 2, yes = 1, no = 0)),
               ftc = ifelse(wave_yr <= 1997 & jbterm < 0, yes = NA, 
                            ifelse(wave_yr >= 1998 & jbterm1 < 0, yes = NA, no = ftc)),
               tmp = ifelse(wave_yr <= 1997 & jbterm == 2, yes = 1,
                            ifelse(wave_yr >= 1998 & jbterm1 == 2, yes = 1, no = 0)),
               tmp = ifelse(wave_yr <= 1997 & jbterm < 0, yes = NA, 
                            ifelse(wave_yr >= 1998 & jbterm1 < 0, yes = NA, no = tmp)))

df_uk <- df_uk %>%
        mutate(contyp = ifelse(perm == 1, yes = 1,
                               ifelse(ftc == 1, yes = 2, 
                                      ifelse(tmp == 1, yes = 3, no = NA))))

# Occupation (ISCO 88) --------------------------------------------------------------


df_uk$occ_grp <- recode(df_uk$jbisco88_cc, "lo:0 = NA; 10:199 = 1; 200:299 = 2; 300:399 = 3; 400:499 = 4; 500:599 = 5; 600:699 = 6; 700:799 = 7; 800:89 = 8; 900:999 = 9") # drop armed forces (0:10)
df_uk <- select(df_uk, -jbisco88_cc)

# employment status --------------------------------------------------------------

df_uk <- df_uk %>%
        mutate(emp = ifelse(jbstat==2, yes=1,
                            ifelse(jbstat == 3, yes = 0, no = NA)),
               unmp = ifelse(jbstat==3, yes=1,
                             ifelse(jbstat == 2, yes = 0, no = NA)),
               lfp = ifelse(jbstat==1 | jbstat==2 | jbstat==3, yes = 1,
                            ifelse(jbstat < 0, yes = NA, no = 0)))

df_uk$ftime <- recode(df_uk$jbft_dv, "1 = 1; 2=0; else = NA") 
df_uk$ptime <- recode(df_uk$jbft_dv, "1 = 0; 2=1; else = NA") 

df_uk <- df_uk %>%
        select(-jbft_dv)

# income --------------------------------------------------------------

df_uk <- df_uk %>%
        mutate(wages = fimnlabgrs_dv/cpi*100,
               ln_wages = log(wages)) %>%
        mutate(hourly_wage = ifelse(jbhrs>0, yes=fimnlabgrs_dv/jbhrs/cpi*100, no = 0),
               ln_hourly_wage = log(hourly_wage)) 
        
summary(df_uk$hourly_wage)

# Education --------------------------------------------------------------
df_uk <- df_uk %>%
        rename(edu=hiqual_dv)

df_uk$edu_cat <- recode(df_uk$edu, "lo:0 = NA; c(5,9)=1; c(2,3,4)=2; c(1)=3")
df_uk <- df_uk %>%
        mutate(edu_lo = ifelse(edu_cat == 1, 1, 0),
               edu_mi = ifelse(edu_cat == 2, 1, 0),
               edu_hi = ifelse(edu_cat == 3, 1, 0)) %>%
        select(-edu)

# sex
df_uk$male <- recode(df_uk$sex, "lo:0 = NA; 1=1; 2=0") 

df_uk <- df_uk %>%
        select(-sex)

df_uk <- df_uk %>%
        mutate(age = ifelse(doby>0, yes = wave_yr - doby, no = NA),
               age_2 = age*age)

# df_uk <- df_uk %>%
#         mutate(age_1 = ifelse(age < 25, 1, 0),
#                age_2 = ifelse(age >= 25 & age < 35, 1, 0),
#                age_3 = ifelse(age >= 35 & age < 45, 1, 0),
#                age_4 = ifelse(age >= 45 & age < 55, 1, 0),
#                age_5 = ifelse(age >= 55 & age < 65, 1, 0)
#         ) %>%
#         mutate(age_cat = ifelse(age_1 == 1, yes = 1,
#                                 ifelse(age_2 == 1, yes = 2,
#                                        ifelse(age_3 == 1, yes = 3,
#                                               ifelse(age_4 == 1, yes = 4,
#                                                      ifelse(age_5 == 1, yes = 5, no = NA))))))

df_uk <- df_uk %>%
        rename(year=wave_yr,
               pid =pidp) %>%
        select(-wave_no)

# Sample creation --------------------------------------------------------------

df_sample_1 <- df_uk %>%
        # filter(age >= 25 & age <= 54) %>%
        # filter(doby>=1936) %>%
        filter(age < 65) %>%
        filter(!is.na(age)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter(!is.na(contyp)) %>%
        filter(emp == 1 | unmp == 1) %>%
        # filter(ftime == 1 | ptime == 1) %>%
        filter(!is.na(prestige)) %>%
        # filter(hourly_wage>1) %>%
        filter(wages > 100) %>%
        filter(jbhrs>0) %>%
        select(-jbstat, -matches("jbterm"), -fimnlabgrs_dv, -cpi, -jbhrs, -doby, -wages, -hourly_wage)

df_sample_2 <- df_sample_1 %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(start = ifelse(year - lag(year) == 1, yes = NA, no = 1),
               start = ifelse(row_number()==1, yes = 1, no = start),
               spell = cumsum(ifelse(is.na(start), 0, start))) %>%
        ungroup() %>%
        group_by(pid, spell) %>%
        mutate(count=row_number(),
               max = max(count)) %>%
        ungroup()

# %>%
#         filter(max >= 5)

# %>%
#         select(-start,-spell,-max,-count)

# View(select(df_sample, pid, year, start, spell, count, max))
# View(select(df_sample, pid, year, start, spell, count, max) %>% filter(spell >3 & max>4))

# Temporary employment spell
df_sample_3 <- df_sample_2 %>%
        arrange(pid, year) %>%
        group_by(pid, spell) %>%
        mutate(start_ftc = ifelse(contyp==2 & lag(contyp)==1, yes = 1, no = 0),
               spell_ftc = cumsum(start_ftc),
               start_ftc = ifelse(contyp==2 & lag(contyp)==1, yes = year, no = NA),
               end_ftc = ifelse(contyp==1 & lead(contyp)==2 & lead(spell_ftc)==1, yes = year + 1, no = NA)) %>%
        ungroup()

# Post temporary employment spell
df_sample_4 <- df_sample_3 %>%
        arrange(pid, year) %>%
        group_by(pid,spell_ftc) %>%
        arrange(pid,year) %>%
        mutate(start_ftc = na.locf(start_ftc,na.rm = FALSE),
               post_ftc = year-start_ftc) %>%
        ungroup() %>%
        arrange(pid,year)
View(filter(df_sample_4,pid == 409956365) %>% select(pid, year, spell, contyp, matches("ftc")))

# Post first temporary employment spell
df_sample_5 <- df_sample_4 %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(start_ftc_first = ifelse(start_ftc > 1 & spell_ftc == 1 & ftc == 1, yes = year, no = NA)) %>%
        ungroup()

View(filter(df_sample_5,pid == 409956365) %>% select(pid, year, spell, contyp, matches("ftc")))

,
start_ftc_first = na.locf(start_ftc_first,na.rm = FALSE)
,
post_ftc_first = year-start_ftc_first
%>%
        mutate(post_ftc_first = ifelse(is.na(post_ftc_first), yes = 0, no = post_ftc_first))

# Pre first temporary employment spell


df_sample <- df_sample %>%
        mutate(pre_ftc = pre_ftc*-1,
               pre_ftc = ifelse(pre_ftc>2, yes = 2, no = pre_ftc),
               post_ftc = ifelse(post_ftc > 5, yes = 5, no = post_ftc),
               post_ftc_first = ifelse(post_ftc_first > 5, yes = 5, no = post_ftc_first))

# View(filter(df_sample,pid == 409956365) %>% select(pid, year, spell, contyp, matches("ftc")))
# View(filter(df_sample,start_ftc==1 & spell_ftc == 3) %>% select(pid, year, contyp, matches("ftc")))


df_unique <- df_sample %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

# Save data sets --------------------------------------------------------------

df_sample <- arrange(df_sample, pid, year) %>%
        select(-spell, -max, -count, -spell_ftc, -start, -start_ftc)
saveRDS(df_sample, file = paste0(data_files, "bhps.rds"))
