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

data_files = "data_files/DE/"
support_files = "support_files/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(zoo)
library(Hmisc)

options(scipen = 999) # disable scientific notation

# Load data ----

df_de <- readRDS(paste0(data_files, "covars.rds"))

df_de <- df_de %>%
        rename(year=syear)

# inflation data - world bank
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
        select(year, DE) %>%
        rename(cpi=DE)

# Merge data ----

df_de <- merge(df_de,df_inflation)
rm(df_inflation)

# Employment status ----

# labor force participation
df_de$lfp <- recode(df_de$lfs, "lo:0 = NA; seq(1,5,1)=0; seq(6,12,1)=1")

# unemployment status
df_de$unmp <- recode(df_de$empst, "lo:0 = NA; 1:2=0; 5=1; else = NA")

# Employment status (1=permanent, 2=temporary)
with(df_de, table(year,contyp))

df_de$emp_status <- recode(df_de$contyp, "lo:0 = NA; 3:hi=NA")

t <- with(df_de, table(year,emp_status))
prop.table(t,1)

# Self-employed
df_de$slf <- recode(df_de$contyp, "3:hi=1; else=0")

# Occupation (ISCO 88)
df_de$occ <- recode(df_de$occ, "lo:0 = NA")

# Prestige (Treiman)
df_de$prestige <- recode(df_de$prestige, "lo:0 = NA")

# Hours (annual)
df_de$hours <- recode(df_de$hours, "lo:-.001 = NA")
describe(df_de$hours)

# Wages (annual)
df_de$wages <- recode(df_de$wages, "lo:-.001 = NA")

df_de <- df_de %>%
        mutate(wages = wages/(cpi/100),
               hourly_wage = wages/hours,
               wages=wages/12, # monthly wages
               hours=hours/12, # monthly hours
               )

describe(df_de$hours)

# demographic characteristics ----

# education (CASMIN)
# https://paneldata.org/soep-long/df_covars/pgen/pgcasmin
df_de$edu_cat <- recode(df_de$edu, "lo:0=NA; c(1,2,3)=1; c(4,5,6,7)=2; c(8,9)=3")
with(df_de, table(edu,edu_cat))

# sex
df_de <- df_de %>%
        mutate(sex = ifelse(sex == 2, yes = 0, no = sex)) %>%
        rename(male = sex) 

# Age
df_de <- df_de %>%
        mutate(age = ifelse(birth_year>0, yes = year - birth_year, no = NA),
               age_cat = ifelse(age >= 25 & age < 35, 1,
                                ifelse(age >= 35 & age < 45, 2, 
                                       ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_de %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
table(df_descriptives$unmp)
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")
# employment original
table(df_descriptives$empst,useNA = "ifany")
df_descriptives$lfp <- recode(df_descriptives$empst, 
                              "lo:0 = '00 Missing';
                                 1 = '01 Full-time employment';
                                 2 = '02 Regular part-time employment';
                                 3 = '03 Vocational training';
                                 4 = '04 Marginal, irregular part-time employment';
                                 5 = '05 Not employed';
                                 6 = '06 Sheltered workshop'")

# Write table

t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Germany (pgemplst)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/emp_status_de.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_descriptives <- df_de %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  # filter(contyp>0&contyp<3) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$contyp,useNA = "ifany")
df_descriptives$contyp <- recode(df_descriptives$contyp, 
                                 "lo:0 = '00 Missing';
                                 1 = '01 Permanent';
                                 2 = '02 Temporary';
                                 3 = '03 No work contract/self-employed'")

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
         country = "Germany (plb0037_h)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "descriptives/contyp_de.csv"), row.names = FALSE)

# Education
df_descriptives <- df_de %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_descriptives$edu)
df_descriptives$edu <- recode(df_descriptives$edu, 
                              "lo:-1 = '00 Missing';
                              0 = '0 in school';
                              1 = '1a  inadequately completed';
                                 2 = '1b general elementary school';
                                 3 = '1c basic vocational qualification';
                                 4 = '2b intermediate general qualification';
                                 5 = '2a intermediate vocational';
                                 6 = '2c_gen general maturity certificate';
                                 7 = '2c_voc vocational maturity certificate';
                                 8 = '3a lower tertiary education';
                                 9 = '3b higher tertiary education';
                              ")

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
         country = "Germany (pgcasmin)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "descriptives/edu_de.csv"), row.names = FALSE)

# Sample creation ----

df_sample_0 <- df_de %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "DE"

saveRDS(df_sample_0, file = paste0(data_files, "de_sample.rds"))

