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

data_files = "projects/mobility/data_files/UK/"
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

# employment status --------------------------------------------------------------

df_uk <- df_uk %>%
        mutate(jbstat = ifelse(jbstat<0, yes = NA, no = jbstat),
               emp = ifelse(jbstat==2, yes=1,
                            ifelse(jbstat == 3, yes = 0, no = NA)),
               unmp = ifelse(jbstat==3, yes=1,
                             ifelse(jbstat == 2, yes = 0, no = NA)),
               lfp = ifelse(jbstat==1 | jbstat==2 | jbstat==3, yes = 1,
                            ifelse(jbstat < 0, yes = NA, no = 0)))

# Full-time/part-time
df_uk$ftime <- recode(df_uk$jbft_dv, "1 = 1; 2=0; else = NA") 

df_uk <- df_uk %>%
        select(-jbft_dv)

# Contract type 
# Contract type (0=permanent, 1=fixed term contract, 2=other temporary)

df_uk <- df_uk %>%
        mutate(jbterm1 = ifelse(jbterm1 < 0, yes = NA, no = jbterm1),
               jbterm2 = ifelse(jbterm2 < 0, yes = NA, no = jbterm2),
               contyp_1 = ifelse(wave_yr >= 1998 & jbterm1 == 1, yes = 1,
                               ifelse(wave_yr >= 1998 & jbterm1 == 2 & jbterm2 == 2, yes = 3,
                                      ifelse(wave_yr >= 1998 & jbterm1 == 2 & (jbterm2 == 1 | jbterm2 > 2), yes = 2, no = NA))))

with(df_uk,table(wave_yr, contyp_1, useNA = "ifany"))

df_uk <- df_uk %>%
        mutate(jbterm = ifelse(jbterm < 0, yes = NA, no = jbterm),
               contyp = ifelse(wave_yr <= 1997, yes = jbterm, contyp_1)) %>%
        select(-contyp_1) 

with(df_uk,table(wave_yr, contyp, useNA = "ifany"))


# Recode: if you are unemployed, then you don't have a contract type, full/part-time employment, or prestige
df_uk <- df_uk %>%
        mutate(contyp = ifelse(unmp == 1, yes = NA, no = contyp),
               ftime = ifelse(unmp == 1, yes = NA, no = ftime),
               prestige = ifelse(unmp == 1, yes = NA, no = prestige))

df_uk <- df_uk %>%
        mutate(ftc = ifelse(contyp == 3, yes = 1, 
                            ifelse(contyp < 3, yes = 0, no = NA)),
               tmp = ifelse(contyp == 2, yes = 1, 
                            ifelse(contyp == 1 | contyp == 3, yes = 0, no = NA)))

with(df_uk,table(wave_yr, contyp, useNA = "ifany"))
with(df_uk,table(contyp, ftc, useNA = "ifany"))

# Occupation (ISCO 88) --------------------------------------------------------------


df_uk$occ_grp <- recode(df_uk$jbisco88_cc, "lo:0 = NA; 10:199 = 1; 200:299 = 2; 300:399 = 3; 400:499 = 4; 500:599 = 5; 600:699 = 6; 700:799 = 7; 800:899 = 8; 900:999 = 9") # drop armed forces (0:10)
df_uk <- select(df_uk, -jbisco88_cc)


# income --------------------------------------------------------------

df_uk <- df_uk %>%
        mutate(wages = fimnlabgrs_dv/cpi*100) %>%
        mutate(hourly_wage = ifelse(jbhrs>0, yes=fimnlabgrs_dv/jbhrs/cpi*100, no = 0)) 

# Education --------------------------------------------------------------
df_uk <- df_uk %>%
        rename(edu=hiqual_dv)

df_uk$edu_cat <- recode(df_uk$edu, "lo:0 = NA; c(5,9)=1; c(2,3,4)=2; c(1)=3")
df_uk <- df_uk %>%
        select(-edu)

# sex
df_uk$male <- recode(df_uk$sex, "lo:0 = NA; 1=1; 2=0") 

df_uk <- df_uk %>%
        select(-sex)

df_uk <- df_uk %>%
        mutate(age = ifelse(doby>0, yes = wave_yr - doby, no = NA),
               age_2 = age*age) %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1, 
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))
        
df_uk <- df_uk %>%
        rename(year=wave_yr,
               pid =pidp) %>%
        select(-wave_no)

# Sample creation --------------------------------------------------------------

df_sample_1 <- df_uk %>%
        filter(year>=2000) %>%
        filter(!is.na(age_cat)) %>%
        filter(!is.na(edu_cat)) %>%
        filter(!is.na(male)) %>%
        filter((emp == 1 & !is.na(prestige)) | unmp == 1) %>%
        filter((emp == 1 & !is.na(contyp)) | unmp == 1) %>%
        filter(age >= 25 & age <= 54)

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
        ungroup() %>%
        filter(max >= 2)

# View(select(df_sample, pid, year, start, spell, count, max))
# View(select(df_sample, pid, year, start, spell, count, max) %>% filter(spell >3 & max>4))

# Temporary employment spell
df_sample_3 <- df_sample_2 %>%
        arrange(pid, year) %>%
        group_by(pid, spell) %>%
        mutate(start_ftc = ifelse(contyp==3 & lag(contyp,1)!=3, yes = year, no = 0),
               start_ftc = ifelse(contyp == 3 & row_number()==1, yes = year, no = start_ftc),
               end_ftc = ifelse(lag(contyp,1)==3 & contyp!=3, yes = year-1, no = NA)) %>%
        ungroup() %>%
        group_by(pid) %>%
        mutate(spell_ftc = cumsum(ifelse(start_ftc>0, yes = 1, no = 0))) %>%
        ungroup() %>%
        mutate(start_ftc = ifelse(start_ftc == 0, yes = NA, no = start_ftc),
               spell_ftc = ifelse(spell_ftc == 0, yes = NA, no = spell_ftc),
               end_ftc = ifelse(end_ftc == 0, yes = NA, no = end_ftc))
# View(filter(df_sample_3,pid == 409956365) %>% select(pid, year, spell, contyp, ftc, matches("ftc")))

# Post temporary employment spell
df_sample_4 <- df_sample_3 %>%
        arrange(pid, year) %>%
        group_by(pid,spell,spell_ftc) %>%
        arrange(pid,year) %>%
        mutate(end_ftc = na.locf(end_ftc,na.rm = FALSE),
               post_ftc = year - end_ftc,
               post_ftc = ifelse(is.na(end_ftc), yes = 0, no = post_ftc)) %>%
        mutate(end_ftc = na.locf(end_ftc,na.rm = FALSE),
               post_ftc = year - end_ftc,
               post_ftc = ifelse(is.na(end_ftc), yes = 0, no = post_ftc)) %>%
        ungroup() %>%
        arrange(pid,year)
# View(filter(df_sample_4,pid == 409956365) %>% select(pid, year, spell, contyp, matches("ftc")))
# View(filter(df_sample_4,pid == 22445) %>% select(pid, year, spell, contyp, matches("ftc")))

# Post first temporary employment spell
df_sample_5 <- df_sample_4 %>%
        arrange(pid, year) %>%
        group_by(pid) %>%
        mutate(post_ftc_first = cumsum(post_ftc),
               post_ftc_first = ifelse(post_ftc_first==1, yes = year-1, no = NA),
               post_ftc_first = na.locf(post_ftc_first, na.rm = FALSE),
               post_ftc_first = ifelse(post_ftc_first>1, yes = year-post_ftc_first, no = 0)) %>%
        ungroup()
# View(filter(df_sample_5,pid == 22445) %>% select(pid, year, spell, contyp, matches("ftc")))
# View(filter(df_sample_5,pid == 409956365) %>% select(pid, year, spell, contyp, matches("ftc")))
# View(filter(df_sample_5,pid == 69667365) %>% select(pid, year, spell, contyp, matches("ftc")))

# Clean
df_sample_6 <- df_sample_5 %>%
        mutate(post_ftc_first = ifelse(is.na(post_ftc_first), yes = 0, no = post_ftc_first),
               post_ftc_first = ifelse(post_ftc_first > 5, yes = 5, no = post_ftc_first)) %>%
        mutate(post_ftc = ifelse(is.na(post_ftc), yes = 0, no = post_ftc),
               post_ftc = ifelse(post_ftc > 5, yes = 5, no = post_ftc)) %>%
        select(-start_ftc, -end_ftc)
# View(filter(df_sample_6,pid == 409956365) %>% select(pid, year, spell, contyp, ftc, matches("ftc")))
# View(filter(df_sample_6,pid == 22445) %>% select(pid, year, spell, contyp, matches("ftc")))
# View(filter(df_sample_6,pid == 69667365) %>% select(pid, year, spell, contyp, ftc, matches("ftc")))
# View(filter(df_sample_6) %>% select(pid, year, spell, emp, contyp, ftc, matches("ftc")))

df_unique <- df_sample_6 %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

# Save data sets --------------------------------------------------------------

df_sample_6 <- df_sample_6 %>%
        arrange(pid, year) %>%
        select(-start, -count) %>%
        select(-jbstat, -matches("jbterm"), -fimnlabgrs_dv, -cpi, -jbhrs, -doby)
saveRDS(df_sample_6, file = paste0(data_files, "bhps.rds"))
