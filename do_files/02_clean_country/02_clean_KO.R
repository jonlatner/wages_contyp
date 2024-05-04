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

data_files = "data_files/KO/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ko <- readRDS(paste0(data_files,"covars.rds"))

table(df_ko$year)
df_ind <- df_ko %>%
  filter(year == 1999) %>%
  group_by(pid) %>%
  slice(1) %>%
  ungroup()
nrow(df_ind)


# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, KO) %>%
        rename(cpi=KO)

# Merge data ----

df_ko <- merge(df_ko,df_inflation)
rm(df_inflation)

# Occupation ----

df_ko$occ <- recode(df_ko$occ, "lo:0=NA")

# Employment status ----

# labor force participation
df_ko$lfp <- recode(df_ko$empstat, "1:3=1; 4:hi=0; lo:0=NA")

# unemployment status
df_ko$unmp <- recode(df_ko$empstat, "1:2=0; 3=1; else=NA")

# Employment status (1=permanent, 2=temporary)
# (main job) work period stated in contract p__0501
# (1) yes - temporary
# (2) no - permanent
# (3) don't know - NA

df_ko$emp_status  <- recode(df_ko$contyp, "2=1; 1=2; else=NA")

with(df_ko,table(emp_status, contyp,useNA = "ifany"))

# Self-employed
df_ko$slf <- recode(df_ko$emptype, "2=1; else=0")

# Hours (weekly)
df_ko$hours <- recode(df_ko$hours, "lo:-.001=NA")
summary(df_ko$hours)

# Monthly wages  (unit: krw 10,000)
df_ko$wages <- recode(df_ko$wages, "lo:-.001=NA")

df_ko <- df_ko %>%
        mutate(wages = (wages*10000)/(cpi/100),
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours,
               )

# demographics -----
# education (ISCED)
table(df_ko$edu)

df_ko$edu_cat <- recode(df_ko$edu, "1:4=1; 5=2; 6:9=3; else=NA")

# sex
df_ko$male <- recode(df_ko$gender, "2 = 0")

# Age
df_ko <- df_ko %>%
        mutate(age=year-birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_ko %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
table(df_descriptives$unmp)
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")
# employment original
table(df_descriptives$empstat,useNA = "ifany")

df_descriptives$lfp <- recode(df_descriptives$empstat, 
                              "1 = '01 W - Worked';
                                 2 = '02 W - temporarily away from work';
                                 3 = '03 NW - searched for job';
                                 4 = '04 NW - looked after family';
                                 5 = '05 NW - childcare';
                                 6:8 = '06 NW - attended school';
                                 9:10 = '10 Ilness';
                                 11:14 = '11 Preparation for work, school, military';
                                 15:hi = '12 Other';
                              ")

# Write table

t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Korea (0203)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_ko.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_descriptives <- df_ko %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(!is.na(wages)) %>%
  filter(year>=2000)

# contract type recode
table(df_descriptives$emp_status,useNA = "ifany")
df_descriptives$emp_status <- recode(df_descriptives$emp_status, 
                                 "NA = 'NA Missing';
                                 1 = '01 Permanent';
                                 2 = '02 Temporary'")

# contract type original
table(df_descriptives$contyp)
df_descriptives$contyp <- recode(df_descriptives$contyp, 
                              "-1 = '00 Missing';
                              NA = '00 Missing';
                              1 = '01 Permanent';
                                 2 = '02 Temporary';
                                 3 = '03 Dont know'")

# Write table
with(df_descriptives, table(emp_status,contyp,useNA = "ifany"))

t = data.frame(with(df_descriptives, table(emp_status,contyp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Korea (0501)") 
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_ko.csv"), row.names = FALSE)

# Education
df_descriptives <- df_ko %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_descriptives$edu,useNA = "ifany")
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "-1 = 'NA Missing';
                              2 = '02 No schooling';
                                 3 = '03 elementary';
                                 4 = '04 lower secondary';
                                 5 = '05 upper secondary';
                                 6 = '06 2-years college';
                                 7 = '07 university (4 years or more)';
                                 8 = '08 graduate school (masters)';
                                 9 = '09 graduate school (doctoral)'
                              ")

# education recode
table(df_descriptives$edu_cat,useNA = "ifany")
df_descriptives$edu_cat <- recode(df_descriptives$edu_cat, 
                                  "NA = 'Missing';
                                1 = '01 less than secondary';
                                2 = '02 secondary';
                                3 = '03 More than secondary'")



# Contract status
with(df_descriptives, table(edu,edu_cat,useNA = "ifany"))
t = data.frame(with(df_descriptives, table(edu,edu_cat,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Korea (0110)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/edu_ko.csv"), row.names = FALSE)

# Sample creation ----

df_sample_0 <- df_ko %>%
        select(pid, year, age, age_cat, edu_cat, male, slf, lfp, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "KO"

saveRDS(df_sample_0, file = paste0(data_files, "ko_sample.rds"))

