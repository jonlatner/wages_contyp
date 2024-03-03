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

# FOLDERS
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/AU/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----
# This data declares that all job contracts that are not permanent are temporary

df_au <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, AU) %>%
        rename(cpi=AU)

# Merge data ----

df_au <- merge(df_au,df_inflation)
rm(df_inflation)


# Clean ----

df_au <- df_au %>%
        rename(occ = jbm682,
               prestige = jbmo6s,
               lfs = esdtl,
               hours=jbmhruc,
               wages=wsfei, # DV: Financial year gross wages & salary ($) [imputed] [weighted topcode]
               edu = edhigh1,
               contyp = jbmcnt,
               sex = hgsex,
               empstat = esempdt,
               age=hgage,
               pid = xwaveid) %>%
        arrange(pid,year)

# Employment status ----

# labor force participation
df_au$lfp <- recode(df_au$lfs, "lo:0=NA; 1:4=1; 5:7=0")

# unemployment status
df_au$unmp <- recode(df_au$lfs, "lo:0=NA; 1:2=0; 3:4=1; else=NA")

# contract type
table(df_au$contyp)
# jbmcnt 1 Employed on a fixed-term contract
# jbmcnt 2 Employed on a casual basis 
# jbmcnt 3 Employed on a permanent or ongoing basis 
# jbmcnt 8 Other

# Employment status (1=permanent, 2=temporary)
# this is the definition used in 02_clean_AU_v01.R, where FTC/Casual are grouped together
# df_au$emp_status  <- recode(df_au$contyp, "lo:0=NA; 1:2=2; 3=1; 8=NA")

# Employment status (1=permanent, 2=temporary, 3=employed, but not temp or perm)
df_au <- df_au %>%
        mutate(emp_status = ifelse(contyp == 3, yes = 1,
                                   ifelse(contyp == 1, yes = 2, # temporary and ftc
                                          ifelse(contyp == 2, yes = 3, no = NA)))) # temporary and not ftc (i.e. casual)

with(df_au, table(contyp,emp_status,useNA = "ifany"))

# Self-employed
df_au$slf <- recode(df_au$empstat, "4:5=1; else=0")

# Occupation (ISCO 88)
df_au$occ <- recode(df_au$occ, "lo:0=NA")

# Prestige (Treiman)
df_au$prestige <- recode(df_au$prestige, "lo:0 = NA")

# Hours (weekly)
df_au$hours <- recode(df_au$hours, "lo:-.001 = NA")

# Wages (annual)
df_au$wages <- recode(df_au$wages, "lo:-.001 = NA")
summary(df_au$wages)
df_au <- df_au %>%
        mutate(wages = wages/(cpi/100),
               wages=wages/12, # monthly wages
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours, 
        )

# demographics -----
# education (ISCED)
df_au$edu_cat <- recode(df_au$edu, "lo:-1 = NA; 6:9=1; 4:5=2; 1:3=3; 10=NA")

# sex
df_au$male <- recode(df_au$sex, "lo:0 = NA; 2 = 0")

# Age
df_au <- df_au %>%
        mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))


# Descriptive graphs ----

# Employment status 
df_descriptives <- df_au %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# employment recode
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")

# employment original
df_descriptives$lfp <- recode(df_descriptives$lfs, 
                              "1 = '01 Employed FT';
                                 2 = '02 Employed PT';
                                 3 = '03 Unemployed, looking for FT work';
                                 4 = '04 Unemployed, looking for PT work';
                                 5 = '05 Not in labour force, marginally attached';
                                 6 = '06 Not in labour force, not marginally attached';
                                 7 = '07 Employed, but usual hours worked unknown'")

# Write table
t = data.frame(with(df_descriptives, table(lfp,emp))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Australia (esdtl)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "emp_status_au.csv"), row.names = FALSE)

# Contract type, conditional on employed
df_descriptives <- df_au %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$contyp,useNA = "ifany")
df_descriptives$contyp <- recode(df_descriptives$contyp, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 Employed on a FTC';
                                 2 = '02 Employed on a casual basis';
                                 3 = '03 Employed on a permanent or ongoing basis';
                                 8 = '08 Other'")

# contract type recode
table(df_descriptives$emp_status,useNA = "ifany")
df_descriptives$emp_status  <- recode(df_descriptives$emp_status, 
                                      "1= '01 Permanent'; 
                                      2= '02 Temporary';
                                      3 = 'NA Missing';
                                      NA = 'NA Missing'")


# Write table
t = data.frame(with(df_descriptives, table(emp_status,contyp))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Australia (jbmcnt)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "contyp_au.csv"), row.names = FALSE)

# Education
df_descriptives <- df_au %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "1 = '01 Postgrad - masters or doctorate';
                                 2 = '02 Grad diploma, grad certificate';
                                 3 = '03 Bachelors or honours';
                                 4 = '04 Adv diploma, diploma';
                                 5 = '05 Cert III or IV';
                                 8 = '08 Year 12';
                              9 = '09 Year 11 and below';
                              10 = 'Undetermined'")

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
         country = "Australia (edhigh1)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "edu_au.csv"), row.names = FALSE)

# save ----

df_sample_0 <- df_au %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "AU"

saveRDS(df_sample_0, file = paste0(data_files, "au_sample_v02.rds"))

