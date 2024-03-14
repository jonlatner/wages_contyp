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

data_files = "data_files/NE/LSP/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)

options(scipen = 999) # disable scientific notation

# Load data ----

df_ne <- readRDS(paste0(data_files,"covars.rds"))

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, CH) %>%
        rename(cpi=CH)

# Merge data ----

df_ne <- merge(df_ne,df_inflation)
rm(df_inflation)

# Employment status ----

with(df_ne,table(year,empstat))
with(df_ne,table(year,contyp))
with(df_ne,table(year,edu))

# labor force participation
df_ne$lfp <- recode(df_ne$empstat, "0:2=1; 3=0; 4=1; 5=0; 7=0")

# unemployment status
df_ne$unmp <- recode(df_ne$empstat, "0:1=0; 4=1; else=NA")

# Employment status (1=permanent, 2=temporary)
# Wat voor soort dienstverband heeft u? eb002
# 1 vast dienstverband
# 2 tijdelijk contract met uitzicht op een vast dienstverband
# 3 tijdelijk contract
# 4 anders
df_ne$emp_status <- recode(df_ne$contyp, "1=1; 2:3=2; else=NA")

# Self-employed
df_ne$slf <- recode(df_ne$empstat, "2=1; else=0")

# Hours (weekly)
df_ne$hours <- recode(df_ne$hours, "lo:-.001 = NA")

# Wages (monthly)
df_ne$wages <- recode(df_ne$wages, "lo:-.001 = NA")

df_ne <- df_ne %>%
        mutate(wages = wages/(cpi/100),
               hours = hours*4, # monthly hours
               hourly_wage = wages/hours, 
        )

# demographics -----
# education (ISCED)
with(df_ne,table(year, edu))
df_ne$edu_cat <- recode(df_ne$edu, "lo:0 = NA; 2:3=1; 4=2; 5:6=3")

# sex
df_ne$male <- recode(df_ne$gender, "2 = 0")

# Age
df_ne <- df_ne %>%
        mutate(age = year - birth_year,
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_nescriptives <- df_ne %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
table(df_nescriptives$unmp)
df_nescriptives$emp <- recode(df_nescriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")
# employment original
table(df_nescriptives$empst,useNA = "ifany")
df_nescriptives$lfp <- recode(df_nescriptives$empst, 
                              "lo:0 = '00 Other position in current employment';
                                 1 = '01 Salaried';
                                 2 = '02 Self-employed';
                                 3 = '03 Assisting spouse/partner';
                                 4 = '04 No paid work, but looking for work';
                                 5 = '05 No paid work, not participating';
                                 6 = '06 Military service';
                              7 = '07 Day training'")

# Write table

t = data.frame(with(df_nescriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Netherlands (ca001)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_ne.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_nescriptives <- df_ne %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(year>=2000)

# contract type original
table(df_nescriptives$contyp,useNA = "ifany")
df_nescriptives$contyp <- recode(df_nescriptives$contyp, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 Permanent employment';
                                 2 = '02 Temporary contract with prospect of permanent employment';
                                 3 = '03 Temporary contract';
                                 4 = '04 Other'")

# contract type recode
table(df_nescriptives$emp_status,useNA = "ifany")
df_nescriptives$emp_status  <- recode(df_nescriptives$emp_status, 
                                      "1= '01 Permanent'; 
                                      2= '02 Temporary';
                                      NA = 'NA Missing'")


# Write table
t = data.frame(with(df_nescriptives, table(emp_status,contyp))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Netherlands (eb002)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_ne.csv"), row.names = FALSE)

# Education
df_nescriptives <- df_ne %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_nescriptives$edu)

df_nescriptives$edu <- recode(df_nescriptives$edu, 
                              "lo:-1 = '00 Missing';
                                 2 = '02 elementary school';
                                 3 = '03 vocational education (vmbo/lbo/mavo)';
                                 4 = '04 intermediate education (mbo/havo/vwo)';
                                 5 = '05 higher vocational education (hbo)';
                                 6 = '06 tertiery education (wo - soi 6,7)'")

# education recode
df_nescriptives$edu_cat <- recode(df_nescriptives$edu_cat, 
                                  "NA = 'Missing';
                                1 = '01 less than secondary';
                                2 = '02 secondary';
                                3 = '03 More than secondary'")



# Contract status
t = data.frame(with(df_nescriptives, table(edu,edu_cat))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Netherlands (ba016)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/edu_ne.csv"), row.names = FALSE)

# save ----

df_sample_0 <- df_ne %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "NE-LSP"

saveRDS(df_sample_0, file = paste0(data_files, "ne_sample.rds"))

