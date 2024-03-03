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
df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") %>%
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

# labor force participation (v170)
# 1 Performed paid work - Mostly worked
# 2 Performed paid work - Worked while mostly attending school
# 3 Performed paid work - Worked while mostly keeping house
# 4 Did not perform any paid work - Took leave from work
# 5 Did not perform any paid work - Was looking for work
# 6 Did not perform any paid work - Attended school; kept house; other
df_jp$lfp <- recode(df_jp$lfs, "1:5=1; 6=0; 9=NA")

# unemployment status
df_jp$unmp <- recode(df_jp$lfs, "5=1; 1:4=0; else=NA")

# Employment status (v218)
with(df_jp,table(empstat))

# 1 Self-employed worker
# 2 Professional
# 3 Family employee
# 4 Working at home without an employee relationship with a company
# 5 Wage worker (working at a company, organization, etc.) (as an employee of an employer)
# 6 Consigned work or subcontractor (without an employee relationship)

# Position (v219), [(7) is for respondents who answered “5 Wage worker” in (6)]
# 1 Full-time, regular employee – no title
# 2 Full-time, regular employee – with title
# 3 Full-time, regular employee - manager
# 4 Contract employee
# 5 Part-time worker
# 6 Subcontracted worker
# 7 Specialized contract employee
with(df_jp,table(position))

# Employment status (1=permanent, 2=temporary)
with(df_jp,table(empstat,position))
df_jp$emp_status  <- recode(df_jp$position, "1:3=1; c(4,6,7)=2; 5=1; 9=NA")
df_jp <- df_jp %>%
  mutate(emp_status = ifelse(empstat==5, emp_status, NA))

with(df_jp,table(position,emp_status,useNA = "ifany"))

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
         wages=(wages/12), # monthly wages
         hours=hours*4, # monthly hours
         hourly_wage = wages/hours,
  )

# demographics -----
# education (ISCED)
df_jp <- df_jp %>%
  arrange(pid,year) %>%
  group_by(pid) %>%
  mutate(edu = first(edu)) %>%
  ungroup()

with(df_jp,table(year,edu))

# what to do with category 6?  its labeled "other", this is not specific, but its not missing?  
# its also a lot of cases and its not central to our analysis (just a sensitivity/heterogeneity check)
# from email to jonlatner@gmail.com dated 13 December 2023:  For the case of "Other" the respondent had the option of writing an specific answer. Although for the majority of the cases the answers were  専門学校（Specialized training college） it is hard to judge if it can be aggregated into values over or under secondary school (there may be cases that could also be classified as under secondary school). In case you have interest, you can apply for the JHPS/KHPS Open Answer file and check the answers. Please be aware that this file is available just in Japanese.
table(df_jp$edu)
df_jp$edu_cat <- recode(df_jp$edu, "lo:-1 = NA; 1=1; 2:3=2; 4:5=3; 6=4; 7:hi=NA")

# sex
df_jp$male <- recode(df_jp$gender, "2 = 0")


# Age
df_jp <- df_jp %>%
  mutate(age_cat = ifelse(age >= 25 & age < 35, 1,
                          ifelse(age >= 35 & age < 45, 2, 
                                 ifelse(age >= 45 & age < 55, 3, NA))))

# Descriptive graphs ----

# Employment status 
df_descriptives <- df_jp %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000) 

# employment recode
table(df_descriptives$unmp)
df_descriptives$emp <- recode(df_descriptives$unmp,
                              "0 = '1 Employed';
                               1 = '0 Unemployed';
                               NA = 'Not in labour force'")
# employment original
df_descriptives$lfp <- recode(df_descriptives$lfs, 
                              "1 = '01 W - I worked full-time';
                                 2 = '02 W - I worked besides attending school';
                                 3 = '03 W - I worked besides doing housework';
                                 4 = '04 NW - I was absent from work';
                                 5 = '05 NW - I looked for a job';
                                 6 = '06 NW - I attended school/housework/others';
                                 9 = 'NA Missing'")

# Write table

t = data.frame(with(df_descriptives, table(lfp,emp,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Japan (v170)") %>%
  filter(pct>.01)
t

write.csv(t, file = paste0(support_files, "emp_status_jp.csv"), row.names = FALSE)


# Contract type, conditional on employed
df_descriptives <- df_jp %>%
  filter(age>=25 & age <=54) %>%
  filter(unmp == 0) %>%
  filter(empstat==5) %>%
  filter(year>=2000)

# contract type original
table(df_descriptives$position)
df_descriptives$contyp <- recode(df_descriptives$position, 
                                 "1 = '01 Full-time, regular employee – no title';
                                 2 = '02 Full-time, regular employee – with title';
                                 3 = '03 Full-time, regular employee - manager';
                                 4 = '04 Contract employee';
                                 5 = '05 Part-time worker';
                                 6 = '06 Subcontracted worker';
                                 7 = '07 Specialized contract employee';
                                 9 = 'NA Missing'")

# contract type recode
table(df_descriptives$emp_status,useNA = "ifany")
df_descriptives$emp_status <- recode(df_descriptives$emp_status, 
                                 "NA = 'NA Missing';
                                 1 = '01 Permanent';
                                 2 = '02 Temporary'")


# Write table
t = data.frame(with(df_descriptives, table(emp_status,contyp))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Japan (v219)")
t

write.csv(t, file = paste0(support_files, "contyp_jp.csv"), row.names = FALSE)

# Education
df_descriptives <- df_jp %>%
  filter(age>=25 & age <=54) %>%
  filter(year>=2000)

# education original
table(df_descriptives$edu,useNA = "ifany")
with(df_descriptives,table(year,edu))
df_descriptives$edu_cat <- recode(df_descriptives$edu, "lo:-1 = NA; 1=1; 2:3=2; 4:5=3; 6=4; 7:hi=NA")

df_descriptives$edu <- recode(df_descriptives$edu, 
                              "1 = '01 Elementary school';
                                 2 = '02 High school';
                                 3 = '03 Junior college';
                                 4 = '04 University';
                                 5 = '05 Graduate school';
                                 6 = '06 Other';
                                 7:hi = '00 Missing'
                                 ")

# education recode
table(df_descriptives$edu_cat,useNA = "ifany")
df_descriptives$edu_cat <- recode(df_descriptives$edu_cat, 
                                  "NA = 'Missing';
                                 1 = '01 less than secondary';
                                 2 = '02 secondary';
                                 3 = '03 More than secondary';
                                 4 = '04 Other'"
                                  )



# Contract status
with(df_descriptives, table(edu,edu_cat,useNA = "ifany"))
t = data.frame(with(df_descriptives, table(edu,edu_cat,useNA = "ifany"))) %>% 
  filter(Freq>0) %>%
  mutate(total = sum(Freq),
         pct = Freq/total,
         country = "Japan (v106)") %>%
  filter(pct>.01) 
t

write.csv(t, file = paste0(support_files, "edu_jp.csv"), row.names = FALSE)

# Sample creation ----

with(df_jp,table(edu,edu_cat,useNA = "ifany"))

df_sample_0 <- df_jp %>%
        select(pid, year, age, age_cat, edu_cat, male, lfp, slf, unmp, emp_status, hours, wages, hourly_wage)

df_sample_0$country <- "JP"

saveRDS(df_sample_0, file = paste0(data_files, "jp_sample.rds"))

