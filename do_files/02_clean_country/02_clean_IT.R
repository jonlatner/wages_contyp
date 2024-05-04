# https://www.online.fbe.unimelb.edu.au/HILDAodd/srchVarnameUsingCategoriesCrossWave.aspx

# Top commands ----
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS (ADAPT THIS PATHWAY!)
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/IT/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_it <- readRDS(paste0(data_files,"covars.rds"))

with(df_it, table(year,contyp))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, IT) %>%
        rename(cpi=IT)

# Merge data ----

df_it <- merge(df_it,df_inflation)
rm(df_inflation)

# Create uniform pid ----

df_it <- df_it %>%
        arrange(hid,pid,year) %>%
        unite(pid, c(hid, pid), sep = "", remove = TRUE) %>%
        mutate(pid = as.numeric(as.character(pid))) %>%
        arrange(pid,year)

# Keep one observation per individual per year ----

# View(filter(df_it, pid==241943 & year == 2012))

df_it <- df_it %>%
  filter(!is.na(birth_year)) %>%
        group_by(pid,year) %>%
        arrange(pid,year,-months_worked) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        arrange(pid,year)

# Employment status ----


# labor force participation
df_it$lfp <- recode(df_it$lfs, "lo:-1=NA; c(0,5)=1; else=0")

# unemployment status
df_it$unmp <- recode(df_it$lfs, "lo:-1=NA; 0=0; 5=1; else=NA")

# Employment status (1=permanent, 2=temporary)
df_it$emp_status  <- recode(df_it$contyp, "lo:0=NA; 1=1; 2:3=2; else=NA")

# Self-employed
df_it$slf <- recode(df_it$empstat, "2=1; else=0")

# Hours (weekly)
df_it$hours <- df_it$hours_total - is.na(df_it$hours_overtime)
df_it$hours <- recode(df_it$hours, "lo:-.001 = NA")

describe(df_it$hours)

# Wages (annual)
df_it$wages <- recode(df_it$wages, "lo:-.001 = NA")

df_it <- df_it %>%
        mutate(wages = wages/(cpi/100),
               wages=wages/12, # monhtly wages
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours,
               )

# demographics -----
# education (ISCED)
df_it$edu_cat <- recode(df_it$edu, "1:3=1; 4=2; 5:6=3; else=NA")

# sex
df_it$male <- recode(df_it$gender, "lo:0 = NA; 2 = 0")

# Age
df_it <- df_it %>%
        mutate(age = year - birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_it %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")

# employment original
table(df_descriptives$lfs,useNA = "ifany")
df_descriptives$lfp <- recode(df_descriptives$lfs, 
                              "0 = '00 employed';
                                 1 = '01 first-job seeker';
                                 2 = '02 homemaker';
                                 3 = '03 well off';
                                 4 = '04 pensioner';
                                 5 = '05 Unemployed';
                                 6 = '06 Student';
                                 7 = '07 Other, not employed';
                               NA = 'NA Missing'")

# Write table

t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Italy (nonoc)") %>%
  filter(pct>.01) 

t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_it.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_descriptives <- df_it %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  # filter(!is.na(contyp)) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$contyp,useNA = "ifany")
df_descriptives$contyp <- recode(df_descriptives$contyp, 
                                 "NA = '00 Missing';
                                 1 = '01 Permanent';
                                 2 = '02 Fixed-term';
                                 3 = '03 Temporary'")

# contract type recode
table(df_descriptives$emp_status,useNA = "ifany")
df_descriptives$emp_status  <- recode(df_descriptives$emp_status, 
                                      "1= '01 Permanent'; 
                                      2= '02 Temporary';
                                      NA = 'NA Missing'")


# Write table
t = data.frame(with(df_descriptives, table(emp_status,contyp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Italy (contratt)") 
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_it.csv"), row.names = FALSE)

# Education
df_descriptives <- df_it %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "1 = '01 None';
                                 2 = '02 Elementary school';
                                 3 = '03 Middle school';
                                 4 = '04 High school';
                                 5 = '05 Bachelors degree';
                                 6 = '06 Post-graduate qualification'")

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
         country = "Italy (studio)")
t

write.csv(t, file = paste0(support_files, "descriptives/edu_it.csv"), row.names = FALSE)

# Sample creation ----

df_sample_0 <- df_it %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "IT"

saveRDS(df_sample_0, file = paste0(data_files, "it_sample.rds"))

