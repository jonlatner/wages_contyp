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

data_files = "data_files/UK/"
support_files = "support_files/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# load data ----
# This data declares that all job contracts that are not permanent are temporary

df_uk <- readRDS(paste0(data_files, "covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, UK) %>%
        rename(cpi=UK,
               wave_yr = year)

# Merge data ----

df_uk <- merge(df_uk,df_inflation)
rm(df_inflation)

# clean data ----

df_uk <- df_uk %>%
        arrange(pidp, wave_yr) %>%
        rename(year=wave_yr,
               pid =pidp) %>%
        select(-wave_no)


# employment status ----
df_uk <- df_uk %>%
        mutate(unmp = ifelse(jbstat==3, yes=1,
                             ifelse(jbstat == 2, yes = 0, no = NA)),
               lfp = ifelse(jbstat==2 | jbstat==3, yes = 1,
                            ifelse(jbstat < 0, yes = NA, no = 0)))

with(df_uk,table(year,jbterm, useNA = "ifany"))

# jbterm 1 Permanent job
# jbterm 2 Seasonal/tmp job
# jbterm 3 Contract/fixed time
# Contract type (0=permanent, 1=temporary)
# Contract type if year < 1999
df_uk <- df_uk %>%
        mutate(emp_status_1 = ifelse(jbterm == 1, yes = 1,
                             ifelse(jbterm == 2 | jbterm == 3, yes = 2, no = NA)))

with(df_uk,table(year,jbterm1, useNA = "ifany"))
# jbterm1 1 Permanent job
# jbterm1 2 Or is there some way that it is not permanent?
# Contract type if year >= 1999
df_uk <- df_uk %>%
        mutate(emp_status_2 = ifelse(jbterm1 == 1, yes = 1,
                               ifelse(jbterm1 == 2, yes = 2, no = NA)))

df_uk <- df_uk %>%
        mutate(emp_status = ifelse(year < 1999, yes = emp_status_1, no = emp_status_2)) %>%
        select(-emp_status_1, -emp_status_2)

# Self-employed
df_uk$slf <- recode(df_uk$jbsemp, "2=1; else=0")

# Occupation (ISCO 88)
df_uk$occ <- recode(df_uk$jbisco88_cc, "lo:0 = NA")
df_uk <- df_uk %>%
        mutate(occ = ifelse(occ>100 | occ == 10, yes = occ*10, no=occ*100))

# Hours (weekly)
df_uk$hours <- recode(df_uk$jbhrs, "lo:-.001 = NA")
describe(df_uk$hours)

# Wages (monthly)
df_uk$wages <- recode(df_uk$fimnlabgrs_dv, "lo:-.001 = NA")

df_uk <- df_uk %>%
        mutate(wages = wages/(cpi/100),
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours, 
        )

# Demographic characteristics ----

# Education
df_uk <- df_uk %>%
        rename(edu=hiqual_dv)

df_uk$edu_cat <- recode(df_uk$edu, "lo:0 = NA; c(5,9)=1; c(2,3,4)=2; c(1)=3")

# sex
df_uk$male <- recode(df_uk$sex, "lo:0 = NA; 1=1; 2=0") 

df_uk <- df_uk %>%
        mutate(age = ifelse(doby>0, yes = year - doby, no = NA)) %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1, 
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_uk %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# employment recode
df_descriptives$emp <- recode(df_descriptives$unmp,
                               "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")
                                 
# employment original
df_descriptives$lfp <- recode(df_descriptives$jbstat, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 Self employed';
                                 2 = '02 Paid employment';
                                 3 = '03 Unemployed';
                                 4 = '04 Retired';
                                 5 = '05 On maternity leave';
                                 6 = '06 Family care or home';
                                 7 = '07 Full-time student';
                                 8 = '08 LT sick or disabled';
                                 9 = '09 Govt training scheme';
                                 10 = '10 Unpaid, family business';
                                 11 = '11 On apprenticeship';
                                 97 = '97 Something else'")

# Write table
t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "United Kingdom (jbstat)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_uk.csv"), row.names = FALSE)

# Contract type, conditional on employed
df_descriptives <- df_uk %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$jbterm1,useNA = "ifany")

df_descriptives$contyp <- recode(df_descriptives$jbterm1, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 A permanent job';
                                 2 = '02 Or is there some way that it is not permanent?'")

# contract type recode
table(df_descriptives$emp_status,useNA = "ifany")
df_descriptives$emp_status  <- recode(df_descriptives$emp_status, 
                                      "1= '01 Permanent'; 
                                      2= '02 Temporary';
                                      NA = 'NA Missing'")


# Write table
t = data.frame(with(df_descriptives, table(emp_status,contyp))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "United Kingdom (jbterm1)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_uk.csv"), row.names = FALSE)

# Education
df_descriptives <- df_uk %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_descriptives$edu)
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "lo:0 = '00 Missing';
                                 1 = '01 Degree';
                                 2 = '02 Other higher degree';
                                 3 = '03 A-level';
                                 4 = '04 GCSE etc';
                                 5 = '05 Other qualification';
                                 9 = '09 No qualification'")

# education recode
df_descriptives$edu_cat <- recode(df_descriptives$edu_cat, 
                                  "NA = 'Missing';
                                1 = '01 less than secondary';
                                2 = '02 secondary';
                                3 = '03 More than secondary'")



# Contract status
t = data.frame(with(df_descriptives, table(edu,edu_cat))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "United Kingdom (hiqual_dv)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/edu_uk.csv"), row.names = FALSE)

# Sample creation ----

df_sample_0 <- df_uk %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "UK"

saveRDS(df_sample_0, file = paste0(data_files, "uk_sample_v01.rds"))


