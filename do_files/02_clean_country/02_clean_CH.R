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

data_files = "data_files/CH/"
support_files = "support_files/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ch <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",")
df_inflation <- df_inflation %>%
        select(year, CH) %>%
        rename(cpi=CH)

# Merge data ----

df_ch <- merge(df_ch,df_inflation)
rm(df_inflation)

# Clean ----

df_ch <- df_ch %>%
        rename(occ = is4maj,
               prestige = tr1maj,
               hours= pw77,
               edu = isced,
               pid=idpers) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_ch$lfp <- recode(df_ch$wstat, "lo:0 = NA; 3=0; 1:2=1")

# unemployment status
df_ch$unmp <- recode(df_ch$wstat, "lo:0 = NA; 1=0; 2=1; else=NA")

# Employment status (1=permanent, 2=temporary)
df_ch$emp_status  <- recode(df_ch$pw36, "lo:0 = NA; 1=2; 2=1")
with(df_ch,table(pw36,emp_status,useNA = "ifany"))
with(df_ch,table(pw37,pw36,useNA = "ifany"))

# Self-employed
df_ch$slf <- recode(df_ch$pw29, "3=1; else=0")

# Occupation (ISCO 88)
df_ch$occ <- recode(df_ch$occ, "lo:0 = NA")

# Prestige (Treiman)
df_ch$prestige <- recode(df_ch$prestige, "lo:0 = NA")

filter(df_ch,pid==10776101)

# Hours (weekly)
df_ch$hours <- recode(df_ch$hours, "lo:-.001 = NA")

# Wages (monthly)
df_ch$wages <- recode(df_ch$wages, "lo:-.001 = NA")

df_ch <- df_ch %>%
        mutate(wages = wages/(cpi/100),
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours,
               )

# demographics -----
# education (ISCED)
table(df_ch$edu)
df_ch$edu_cat <- recode(df_ch$edu, "lo:-1 = NA; c(0,10,20)=1; 30:33=2; 40:hi=3")
with(df_ch,table(edu,edu_cat))

# sex
df_ch$male <- recode(df_ch$sex, "2 = 0")

# Age
df_ch <- df_ch %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_ch %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")

# employment original
table(df_descriptives$wstat)
df_descriptives$lfp <- recode(df_descriptives$wstat, 
                                 "lo:0 = 'NA Missing';
                                 1 = '01 Active occupied';
                                 2 = '02 Unemployed';
                                 3 = '03 Not in labour force'")
# Write table

t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Switzerland (wstat)") %>%
  filter(pct>.01) 

t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_ch.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_descriptives <- df_ch %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$pw36,useNA = "ifany")
df_descriptives$contyp <- recode(df_descriptives$pw36, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 Yes';
                                 2 = '02 No'")

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
         country = "Switzerland (pw36)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_ch.csv"), row.names = FALSE)

# Education
df_descriptives <- df_ch %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_descriptives$edu)
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "lo:0 = '00 Missing';
                              10 = '1 Primary';
                                 20 = '2 Lower secondary';
                                 31 = '3A Secondary - Preparation for tertiary education';
                                 32 = '3B Secondary - Preparation for further prof. education';
                                 33 = '3C Secondary - Entrance into the labour market';
                                 41 = '4A Post-secondary education';
                                 51 = '5A First stage of tertiary (general)';
                                 52 = '5B First stage of tertiary (professional)';
                                 60 = '6 Second stage of tertiary'")

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
         country = "Switzerland (isced 97)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/edu_ch.csv"), row.names = FALSE)

# Sample creation ----

df_sample_0 <- df_ch %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "CH"

saveRDS(df_sample_0, file = paste0(data_files, "ch_sample.rds"))

