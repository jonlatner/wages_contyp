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

# FOLDERS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/JP/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_jp <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank_cpi.csv"), sep = ",") %>%
        select(year, JP) %>%
        rename(cpi=JP)

# Merge data ----

df_jp <- merge(df_jp,df_inflation)
rm(df_inflation)

# Clean ----

df_jp <- df_jp %>%
        mutate(age=year-birth_year) %>%
        arrange(pid, year)

# Employment status ----

# labor force participation
df_jp$lfp <- recode(df_jp$lfs, "1:5=1; 6=0; 9=NA")

# unemployment status
df_jp$unmp <- recode(df_jp$lfs, "5=1; 1:4=0; else=NA")

# Employment status (1=permanent, 2=temporary)
# 1 Self-employed worker
# 2 Professional
# 3 Family employee
# 4 Executive officer
# 5 Full-time employee
# 6 Part-time employee
# 7 Temporary employee
# 8 Contreated/Commissioned employee
# 9 Other

df_jp$emp_status  <- recode(df_jp$position, "1:6=1; c(7,8)=2; 9=NA")

# Self-employed
df_jp$slf <- recode(df_jp$empstat, "1=1; else=0")

# Occupation (ISCO 88)
# Prestige (Treiman)
# Occupational data are 1-digit measures (i.e. no prestige)
# 1 Agriculture, forestry, fisheries
# 2 Mine worker
# 3 Sales worker (a retail storekeeper, storekeeper, sales clerk, salesperson, real estate broker)
# 4 Service worker (beautician, barber, employee in a restaurant or hotel, dustman)
# 5 Administrator (congressman in the national or local government, manager whose position is higher than the chief in a company/group/government office)
# 6 Office worker (general officer, accountancy, operator, sales officer)
# 7 Transportation and communication worker (railroad worker, car driver, ship driver, pilot, conductor, cable operator, broadcasting and radio communication worker)
# 8 Manufacturing/construction /security/moving worker
# 9 Information processing engineer (such as system engineer and programmer.)
# 10 Professional or technological worker (such as researcher/engineer in a company, healthcare worker, legal affairs worker, teacher or artist, excluding information processing engineer)
# 11 Preservation and guards worker (such as a member of Self-Defense Force, police officer, firefighter, guard) 
# 12 Other (Please specify)
df_jp$occ <- recode(df_jp$occ, "1:2=1;12=99")

# Hours (weekly)
df_jp$hours_overtime <- recode(df_jp$hours_overtime, "888:hi=NA")
df_jp$hours_total <- recode(df_jp$hours_total, "888:hi=NA")
df_jp$hours <- df_jp$hours_total - is.na(df_jp$hours_overtime)
df_jp$hours <- df_jp$hours
df_jp$hours <- recode(df_jp$hours, "lo:-.001=NA")

# Annual wages (ten thousand yen)
df_jp$wages <- recode(df_jp$wages, "88888:hi=NA")
df_jp <- df_jp %>%
        mutate(wages = (wages*10000)/(cpi/100),
               wages=(wages/12),
               hourly_wage = wages/(hours*4))

# demographics -----
# education (ISCED)
df_jp <- df_jp %>%
        arrange(pid,year) %>%
        group_by(pid) %>%
        mutate(edu = first(edu)) %>%
        ungroup()

with(df_jp,table(year,edu))

df_jp$edu_cat <- recode(df_jp$edu, "lo:-1 = NA; 1=1; 2:3=2; 4:6=3; 7:hi=NA")


# sex
df_jp$male <- recode(df_jp$gender, "2 = 0")


# Age
df_jp <- df_jp %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Sample creation ----

df_sample_0 <- df_jp %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "JP"

saveRDS(df_sample_0, file = paste0(data_files, "jp_sample.rds"))

